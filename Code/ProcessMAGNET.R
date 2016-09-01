#######################################################
##### PROCESS MAGNET VARIABLES             ############
#######################################################

### FUNCTIONS
# Simple aggregation over sectors using mapping
subtot_f <-function(df, grp, tvar, map){
  FUN <- match.fun("sum, na.rm=TRUE") # na.rm ADDED
  df_subtot <- df %>%
    left_join(.,map) %>%
    na.omit %>% # Remove unmapped sectors
    group_by_(.dots=grp) %>%
    summarise_(value = interp(~FUN(v), v=as.name(tvar))) %>%
    ungroup()
}

# Weighted aggregation
wsubtot_f <-function(df, grp, tvar, weight, map){
  FUN <- match.fun("sum, na.rm=TRUE")
  df %>%
    left_join(.,map) %>%
    na.omit %>% # Remove unmapped sectors
    group_by_(.dots=grp) %>%
    #summarize(value = sum(x*y)/sum(y))
    summarise_(value = interp(~FUN(v*w)/FUN(w), v=as.name(tvar), w=as.name(weight))) %>%
    ungroup
}

projectPath <- "D:\\R\\agCLIM50" 

### CREATE MAPPINGS
# Load concordance table MAGNET2FS
regMAGNET2agCLIM50 <- read_csv(file.path(projectPath, "Mappings/regMAGNET2agCLIM50.csv")) %>%
  unique()

secMAGNET2agCLIM50 <- read_csv(file.path(projectPath, "Mappings/secMAGNET2agCLIM50.csv")) %>%
  dplyr::select(-Note) %>%
  unique()

# Create regional and sectoral mappings
map_reg <- regMAGNET2agCLIM50 %>%
  select(REG, region) %>%
  na.omit %>%
  unique

map_con <- regMAGNET2agCLIM50 %>%
  select(REG, region = con) %>%
  na.omit %>%
  unique

map_wld <- regMAGNET2agCLIM50 %>%
  select(REG, region = wld) %>%
  na.omit %>%
  unique

map_sec <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = sec) %>%
  na.omit %>%
  unique

map_crp <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = crp) %>%
  na.omit %>%
  unique

map_lsp <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = lsp) %>%
  na.omit %>%
  unique

map_agr <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = agr) %>%
  na.omit %>%
  unique

map_tot <- secMAGNET2agCLIM50 %>%
  select(TRAD_COMM, sector = tot) %>%
  na.omit %>%
  unique


 
# ### TEST df
# prodcon <- constant.f("prodcon", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG"))
# test <- filter(prodcon, TRAD_COMM %in% c("pdr", "wht", "gro", "hort", "osd", "c_b", "oagr", "crops")) %>% 
#   mutate(REG = toupper(REG))
# test <- AREA_raw
# df <- test
# grp <- c("scenario", "year", "FSsector", "FSregion", "variable", "unit")
# tvar <- "value"
# mapsec <- map_sec
# mapreg <- map_wld
# rm(mapsec, mapreg, df, grp, tvar)



####################################################
#### Variables with sector and region dimension ####
####################################################

MAGNET1_raw <- list()

#### AREA: harvested and grazed area
# MAGNET: land demand per sector (km2) = AREA
# Note: several countries have multiple land types, here only GHA. As "ENDWL_COMM" is not in the grouping variable. These are summed.
MAGNET1_raw[["AREA"]] <- current.f("AREA", "BaseData_b.gdx", "LDEM", lookup_upd, "LDEM", c("PROD_SECT", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT) %>%
  mutate(value = value/10, # MAGNET AREA is in km2
         unit = "1000 ha")

#### PROD: total production
MAGNET1_raw[["PROD"]] <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
  mutate(unit = "mil 2007 USD")

#### PRODval: Production value, market prices = VALOUTPUT(SSEC,SREG,SUM) AND VALOUTPUT(SSEC,SREG,SUM)  (NOT certified)
MAGNET1_raw[["PRODval"]] <- current.f("PROD", "BaseData_b_view.gdx", "valoutput", lookup_upd_view, "valoutput", c("TRAD_COMM", "REG", "OUTVALUE"), c("TRAD_COMM","REG")) %>%
  mutate(unit = "mil USD")

#### CONS: total domestic consumption
# NB not CONS as defined in MAGNET but domestic use!

PROD <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
  rename(PROD = value) %>%
  ungroup() %>%
  select(-variable)

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export volume at market prices
EXPO <- constant2.f("EXPO", "BaseData_b.gdx", "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGSOURCE", "REGDEST"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGSOURCE, EXPO = value) %>%
  ungroup() %>%
  select(-variable) 

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import volume at market prices
IMPO <- constant2.f("IMPO", "BaseData_b.gdx", "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST, IMPO = value) %>% 
  ungroup() %>%
  select(-variable) 

# Domestic use
MAGNET1_raw[["CONS"]] <- left_join(PROD, IMPO) %>%
  left_join(., EXPO) %>%
  mutate(EXPO = ifelse(is.na(EXPO), 0, EXPO)) %>%
  mutate(IMPO = ifelse(is.na(IMPO), 0, IMPO)) %>%
  mutate(value = PROD+IMPO-EXPO) %>%
  select(-PROD, -IMPO, -EXPO) %>%
  mutate(variable = "CONS",
         unit = "mil 2007 USD")
rm(PROD, EXPO, IMPO)

# # Nutrients per sector
# MAGNET1_raw[["NQSECT"]] <- current.f("NQSECT", "fsbasecalories_2007-2010_update_view.gdx",  "NQSECT", lookup_upd_view, "NQSECT", c("NUTRIENTS", "PRIM_AGRI", "REG"), c("NUTRIENTS", "PRIM_AGRI","REG"))  %>%
#   rename(TRAD_COMM = PRIM_AGRI, unit = NUTRIENTS)

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export volume at market prices in volume
MAGNET1_raw[["EXPO"]] <- constant2.f("EXPO", "BaseData_b.gdx", "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGSOURCE", "REGDEST"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
    group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
    summarize(value = sum(value, na.rm=T)) %>%
    rename(REG = REGSOURCE) %>%
    ungroup() %>%
    mutate(variable = "EXPO",
         unit = "mil 2007 USD")

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import volume at market prices in volume
MAGNET1_raw[["IMPO"]] <- constant2.f("IMPO", "BaseData_b.gdx", "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE"), "qxs", c("TRAD_COMM", "REGSOURCE", "REGDEST")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST) %>% 
  ungroup() %>%
  mutate(variable = "IMPO",
       unit = "mil 2007 USD")

# NB: in case of EXPO REGSOURCE is the exporter and REDDEST the importer.
### EXPO: export value at market prices
MAGNET1_raw[["EXPOval"]] <- current.f("EXPO", "BaseData_b.gdx", "VXMD", lookup_upd, "VXMD", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE")) %>%
  group_by(scenario, year, variable, REGSOURCE, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGSOURCE) %>%
  ungroup() %>%
  mutate(variable = "EXPO",
         unit = "mil USD")

# NB: in case of IMPO, REGDEST is the importer and REGSOURCE the exporter.
### IMPO: import value at market prices
MAGNET1_raw[["IMPOval"]] <- current.f("IMPO", "BaseData_b.gdx", "VIMS", lookup_upd, "VIMS", c("TRAD_COMM", "REGSOURCE", "REGDEST"), c("TRAD_COMM", "REGDEST", "REGSOURCE")) %>%
  group_by(scenario, year, variable, REGDEST, TRAD_COMM) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  rename(REG = REGDEST) %>% 
  ungroup() %>%
  mutate(variable = "IMPO",
         unit = "mil USD")


### PCONS: Total private domestic consumption volume
# Private domestic consumption volume
# Private consumption of domestic products volume
pridomconsvol <- constant2.f("pridomconsvol", "BaseData_b.gdx", "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpd", c("TRAD_COMM", "REG"))
# Private consumption of imported products volume
priimpconsvol <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG"))

MAGNET1_raw[["PCONS"]] <- rbind(pridomconsvol, priimpconsvol) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "PCONS",
         unit = "mil 2007 USD")
rm(pridomconsvol, priimpconsvol)

### PCONS: Total private domestic consumption value
# Private domestic consumption value
# Private consumption of domestic products value
pridomconsval <- current.f("pridomconsval", "BaseData_b.gdx", "VDPM", lookup_upd, "VDPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))
# Private consumption of imported products value
priimpconsval <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"))

# Total private domestic consumption val
MAGNET1_raw[["PCONSval"]] <- rbind(pridomconsval, priimpconsval) %>%
  group_by(REG, TRAD_COMM, scenario, year) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "PCONS",
         unit = "mil USD")
rm(pridomconsval, priimpconsval)


# Private consumption of imported products volume
MAGNET1_raw[["VIMP"]] <- constant2.f("priimpconsvol", "BaseData_b.gdx", "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG"), "qpm", c("TRAD_COMM", "REG")) %>%
  mutate(variable = "VIPM",
         unit = "mil 2007 USD")

# Private consumption of imported products value
MAGNET1_raw[["VIMPval"]] <- current.f("priimpconsval", "BaseData_b.gdx", "VIPM", lookup_upd, "VIPM", c("TRAD_COMM", "REG"), c("TRAD_COMM", "REG")) %>%
  mutate(variable = "VIPM",
         unit = "mil USD")


### FOOD, FEED and OTHU
source(file.path(projectPath, "Code\\FOODFEED.r"))
MAGNET1_raw[["FEED"]] <- FEED; rm(FEED)
MAGNET1_raw[["FOOD"]] <- FOOD; rm(FOOD)
MAGNET1_raw[["OTHU"]] <- OTHU; rm(OTHU)

############################################
### Variables with only region dimension ###
############################################
MAGNET2_raw <- list()

# GDP volume
MAGNET2_raw[["GDPT"]] <-  constant2.f("GDPT","BaseData_b_view.gdx", "GDPSRC", c("REG", "GDPSOURCE"), "REG", "qgdp", "REG") %>%
         mutate(unit = "mil 2007 USD")
       
# POP total population
MAGNET2_raw[["POPT"]] <- constant2.f("POPT", "BaseData_b.gdx", "POP", c("REG"), c("REG"), "pop", c("REG")) %>%
  mutate(unit = "mil pers")

# GDP value = GDPSRC(SREG,SUM) AND GDPSRC(SREG,SUM)  (NOT certified)
MAGNET2_raw[["GDPval"]] <- current.f("GDPT", "BaseData_b_view.gdx", "GDPSRC", lookup_upd_view, "GDPSRC", c("REG", "GDPSOURCE"), c("REG")) %>%
  mutate(unit = "mil USD")
# 
# MAGNET2_raw[["NQT"]] <- current.f("NQT", "fsbasecalories_2007-2010_update_view.gdx",  "NQT", lookup_upd_view, "NQT", c("NUTRIENTS", "REG"), c("NUTRIENTS", "REG")) %>%
#   rename(unit = NUTRIENTS)



#####################
### COMBINE DATA ###
####################

MAGNET1 <- bind_rows(MAGNET1_raw) %>%
              ungroup() %>%
              mutate(REG = toupper(REG)) # REG in capitals for mapping

MAGNET2 <- bind_rows(MAGNET2_raw) %>%
  mutate(REG = toupper(REG)) # REG in capitals for mapping
         
  

#########################################
### MAKE REGION AND SECTOR AGGREGATES ###
#########################################

# Sectoral mappings
MAGNET1 <- bind_rows(
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
MAGNET1 <-bind_rows(
  subtot_f(MAGNET1, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
  subtot_f(MAGNET1, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
  )

MAGNET2 <-bind_rows(
  subtot_f(MAGNET2, c("scenario", "year", "region", "variable", "unit"), "value", map_reg),
  subtot_f(MAGNET2, c("scenario", "year", "region", "variable", "unit"), "value", map_wld),
  subtot_f(MAGNET2, c("scenario", "year", "region", "variable", "unit"), "value", map_con)
) %>%
  mutate(sector = "TOT") # Add TOT for all national level indicators


#################################
### MERGE ALL AGGREGATED DATA ###
#################################

MAGNET1_2 <- rbind(MAGNET1, MAGNET2)


############################
### ADDITIONAL VARIABLES ###
############################

MAGNET3_raw <- list()

### YILD: Endogenous yield
# Need to replace LSP woth LPS defined over RMEAT and DAIRY only.
PRODlsp <- constant.f("PROD", "VALOUTPUT", c("TRAD_COMM","REG", "GDPSOURCE"), c("TRAD_COMM", "REG"), "qo", c("NSAV_COMM", "REG")) %>%
            mutate(unit = "mil 2007 USD", 
                   REG = toupper(REG)) %>% 
            filter(TRAD_COMM %in% c("cattle", "milk"))

PRODlsp <- subtot_f(PRODlsp, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_lsp)

# Regional mappings
PRODlsp <-bind_rows(
  subtot_f(PRODlsp, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
  subtot_f(PRODlsp, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
  subtot_f(PRODlsp, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
)

MAGNET3_raw[["YILD"]] <- bind_rows(
          filter(MAGNET1_2, variable %in% c("AREA") & 
                  sector %in% c("AGR", "CGR", "CRP", "LSP", "DRY", "OSD", "PFB", "RIC", "RUM", "SGC", "VFN", 
                                  "WHT", "TOT")),
          filter(MAGNET1_2, variable %in% c("PROD") & unit %in% c("mil USD") &
                 sector %in% c("AGR", "CGR", "CRP", "DRY", "OSD", "PFB", "RIC", "RUM", "SGC", "VFN", 
                                 "WHT", "TOT")),
          PRODlsp) %>% # Only sectors with land
      select(-unit) %>%
      group_by(scenario, region, sector, year) %>%
      summarize(value = value[variable == "PROD"]/value[variable == "AREA"]) %>%
      mutate(variable = "YILD",
      unit = "1000 USD/ha")
rm(PRODlsp)

### YEXO: Exogenous yield
# Cumulative yield growth is extracted with aland2.f (2007 = 1)
# Note that in aland2.f year is made numeric, which creates some problems as it is character in all other data frames. This is corrected again.
aland <- aland2.f("aland", "aland", c("PROD_SECT", "MREG")) %>%
  rename(REG = MREG, 
         aland = value,
         TRAD_COMM = PROD_SECT) %>%
  select(-variable)  %>%
  mutate(year = as.character(year))
 

AREA <-current.f("AREA", "BaseData_b.gdx", "LDEM", lookup_upd, "LDEM", c("PROD_SECT", "REG"), c("PROD_SECT", "REG")) %>%
  rename(TRAD_COMM = PROD_SECT,
         AREA = value) %>% 
  select(-variable) %>%
  filter(year == 2007) %>%
  select(-year)

YEXO_raw <- left_join(aland, AREA) %>%
        mutate(aland_w = aland*AREA) %>%
        select(-aland) %>%
        gather(variable, value, -scenario: -year) %>%
        mutate(unit = "none",
               REG = toupper(REG))

# Sectoral mappings
YEXO_raw <- bind_rows(
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_sec),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_agr),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_crp),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "REG", "variable", "unit"), "value", map_tot)
)

# Regional mappings
YEXO_raw <-bind_rows(
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_reg),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_wld),
  subtot_f(YEXO_raw, c("scenario", "year", "sector", "region", "variable", "unit"), "value", map_con)
  )

MAGNET3_raw[["YEXO"]] <- YEXO_raw %>% 
                      group_by(scenario, year, sector, region, unit) %>%
                      summarize(value = value[variable == "aland_w"]/value[variable == "AREA"]) %>%
                      mutate(variable = "YEXO",
                      unit = "1000 USD/ha")
rm(YEXO_raw, AREA, aland)


### XPRX: Real export price 
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "mil USD", "GDPval", variable)) %>%
  select(-unit, -sector) %>%
  spread(variable, value) 

# Paasche price index
MAGNET3_raw[["XPRX"]] <- MAGNET1_2 %>%
  filter(variable %in% c("EXPO")) %>%
  mutate(variable = ifelse(unit == "mil USD", "EXPOval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = EXPOval/EXPO/GDPval*GDPT,
         variable = "XPRX",
         unit = "Paasche index") %>%
  select(-EXPO, -EXPOval, -GDPT, -GDPval)
rm(GDPdef)

### NETT: Net trade
MAGNET3_raw[["NETT"]] <- MAGNET1_2 %>%
  filter(variable %in% c("EXPO", "IMPO") & unit == "mil 2007 USD") %>%
  spread(variable, value) %>%
  mutate(value = EXPO - IMPO,
         variable = "NETT",
         unit = "mil 2007 con USD") %>%
  select(-EXPO, -IMPO)
  

### XPRP Real producer price 
GDPdef <- MAGNET1_2 %>%
  filter(variable %in% c("GDPT")) %>%
  mutate(variable = ifelse(unit == "mil USD", "GDPval", variable)) %>%
  select(-unit, -sector) %>%
  spread(variable, value)  

# Paasche price index
MAGNET3_raw[["XPRP"]] <- MAGNET1_2 %>%
  filter(variable %in% c("PROD")) %>%
  mutate(variable = ifelse(unit == "mil USD", "PRODval", variable)) %>%
  select(-unit) %>%
  spread(variable, value) %>%
  left_join(., GDPdef) %>%
  mutate(value = PRODval/PROD/GDPval*GDPT,
         variable = "XPRI",
         unit = "Paasche index") %>%
  select(-GDPT, -GDPval, -PROD, -PRODval)
rm(GDPdef)        




####################
### AVAILABILITY ###
####################


# # AV1
# ### Per capita total amount of net calories available
# # Nutrients
# AV1 <- MAGNET1_2 %>%
#   filter(variable %in% c("NQT", "POPT") & unit %in% c("CAL", "mil pers")) %>%
#   group_by(scenario, FSregion, FSsector, year) %>%
#   summarize(value = value[variable == "NQT"]/value[variable == "POPT"]/365) %>%
#   mutate(unit = "kcal/cap/d",
#          variable = "CALO")



#################
### MERGE ALL ###
#################

MAGNET_tot <- bind_rows(MAGNET1_2, MAGNET3_raw) %>%
                  mutate(model = "MAGNET",
                  year = as.numeric(year))



###################
### CORRECTIONS ###
###################

# Area in Canada for rice is zero, resulting in infinite values for YILD
# Set to zero
MAGNET_tot$value[is.infinite(MAGNET_tot$value)] <- 0

# agCLIM50 uses - LYLD for	Livestock yield (endogenous) and LYXO for	Exogenous livestock yield trend 
# We change the names
MAGNET_tot$variable[MAGNET_tot$variable == "YILD" & MAGNET_tot$sector %in% c("LSP", "DRY", "OAP", "RUM")] <- "LYLD"
MAGNET_tot$variable[MAGNET_tot$variable == "YEXO" & MAGNET_tot$sector %in% c("LSP", "DRY", "OAP", "RUM")] <- "LYXO"

# Rename scenarios in line with agCLIM50
scenMAGNET2agCLIM50 <- read_csv(file.path(projectPath, "Mappings/scenMAGNET2agCLIM50.csv")) %>%
  rename(scenario = scenMAGNET)

MAGNET_tot <- left_join(MAGNET_tot, scenMAGNET2agCLIM50) %>%
  select(-scenario) %>%
  rename(scenario = scenagCLIM50)


############
### SAVE ###
############

agCLIM50Path <- file.path(projectPath, "Cache")
write_csv(MAGNET_tot, file.path(agCLIM50Path, paste("MAGNET_agCLIM50_", Sys.Date(), ".csv", sep="")))
xtabs(~sector+variable, data = MAGNET_tot)

