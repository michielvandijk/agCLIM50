# PROJECT: FOODSECURE agCLIM50
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Plot resuls from different models
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("readr", "readxl", "foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "dplyr", "ggplot2", "openxlsx", "scales", "lazyeval", "ggthemes", "scales", "forcats")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET PATHS
wdPath <- "D:\\Data\\Github\\agCLIM50"
setwd(wdPath)

dataPath <- "D:\\Dropbox\\AgClim50 scenario results"

# SOURCE PLOT FUNCTION
source("Code/plot_f.r")

# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)

# ## COMPARE ALL
# # read data
TOTAL <- read.csv(file.path(dataPath, "ModelResults\\TOTAL_2016-10-04.csv"))
TOTAL <- TOTAL %>% 
  mutate(scenario = fct_relevel(scenario, 
                                c("SSP1_NoCC", "SSP1_CC6", "SSP1_NoCC_m", "SSP1_CC26_m",
                                  "SSP2_NoCC", "SSP2_CC6", "SSP2_NoCC_m", "SSP2_CC26_m",
                                  "SSP3_NoCC", "SSP3_CC6", "SSP3_NoCC_m", "SSP3_CC26_m")))
xtabs(~ variable + model, data = TOTAL)
 
# # create df with index observations for more than one model
# # NOT WORKING YET
# # TOTALsel <- TOTAL %>%
# #   group_by(year, scenario, region, item, variable) %>%
# #   filter(length(model) == 2)
# # 
# # xtabs(~ model + variable, data = TOTALsel)
# 
# # Line plot - index
# TOTAL_lineplot_i <- TOTAL %>%
#   group_by(variable, item) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = lineplot_f(., "Index"))
# 
# pdf(file = file.path(dataPath, "Graphs/TOTAL.pdf"), width = 12, height = 7)
# TOTAL_lineplot_i$plots
# dev.off()

# Comparison GDP, POP and YEXO
# Some models present two units for YEXO and YILD
checkUnit <- filter(TOTAL, variable %in% c("YEXO", "YILD"))
xtabs(~ unit + model + variable, data = checkUnit)

# filter out fm t/ha which is only presented by GLOBIOM
TOTAL <- filter(TOTAL, !(unit == "fm t/ha" & model == "GLOBIOM"))
xtabs(~ unit + model, data = TOTAL)

GDP_POP_YEXO <- TOTAL %>%
  filter(variable %in% c("POPT", "GDPT", "YEXO"))
  
xtabs(~ variable + model, data = GDP_POP_YEXO)
xtabs(~ unit + model, data = GDP_POP_YEXO)

# Line plot - index
GDP_POP_YEXO_lineplot_i <- GDP_POP_YEXO %>%
  group_by(variable, item) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = file.path(dataPath, "Graphs/GDP_POP_YEXO_i.pdf"), width = 12, height = 7)
GDP_POP_YEXO_lineplot_i$plots
dev.off()

# BAR GRAPHS for WLD
# Create colours
red <- colorRampPalette(c("orange", "darkred"))
blue <- colorRampPalette(c("skyblue", "darkblue"))
green <- colorRampPalette(c("greenyellow", "darkgreen"))
colour <- c(green(4), blue(4), red(4))
names(colour) <- levels(TOTAL$scenario)


# Function to plot bar graph
barplot_f <- function(df, var, itm){
  df <- filter(df, variable == var, item == itm)
  title <- paste(var, itm, sep = "_")
  p = ggplot(data = df, aes(x = scenario, y = index, fill = scenario)) +
    scale_fill_manual(values = colour) +
    geom_bar(stat="identity", colour = "black") + 
    facet_wrap(~model) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Index (2010=1)")
  p
}


# Create database for plotting
TOTAL_WLD <- filter(TOTAL, region == "WLD", year == 2050)
xtabs(~variable + unit, data = TOTAL_WLD)

# Create pdf
pdf(file = file.path(dataPath, paste("Graphs/Sevilla_", Sys.Date(),".pdf")), width = 12, height = 7)
barplot_f(TOTAL_WLD, "XPRP", "AGR")
barplot_f(TOTAL_WLD, "XPRP", "CRP")
barplot_f(TOTAL_WLD, "XPRP", "LSP")

barplot_f(TOTAL_WLD, "PROD", "AGR")
barplot_f(TOTAL_WLD, "PROD", "CRP")
barplot_f(TOTAL_WLD, "PROD", "LSP")
barplot_f(TOTAL_WLD, "PROD", "WHT")
barplot_f(TOTAL_WLD, "PROD", "CGR")
barplot_f(TOTAL_WLD, "PROD", "DRY")
barplot_f(TOTAL_WLD, "PROD", "NRM")
barplot_f(TOTAL_WLD, "PROD", "RIC")
barplot_f(TOTAL_WLD, "PROD", "RUM")

barplot_f(TOTAL_WLD, "FOOD", "AGR")
barplot_f(TOTAL_WLD, "FOOD", "CRP")
barplot_f(TOTAL_WLD, "FOOD", "LSP")
barplot_f(TOTAL_WLD, "FOOD", "WHT")
barplot_f(TOTAL_WLD, "FOOD", "CGR")
barplot_f(TOTAL_WLD, "FOOD", "DRY")
barplot_f(TOTAL_WLD, "FOOD", "NRM")
barplot_f(TOTAL_WLD, "FOOD", "RIC")
barplot_f(TOTAL_WLD, "FOOD", "RUM")

barplot_f(TOTAL_WLD, "FEED", "AGR")
barplot_f(TOTAL_WLD, "FEED", "CRP")
#barplot_f(TOTAL_WLD, "FEED", "LSP")
barplot_f(TOTAL_WLD, "FEED", "WHT")
barplot_f(TOTAL_WLD, "FEED", "CGR")
#barplot_f(TOTAL_WLD, "FEED", "DRY")
#barplot_f(TOTAL_WLD, "FEED", "NRM")
barplot_f(TOTAL_WLD, "FEED", "RIC")
#barplot_f(TOTAL_WLD, "FEED", "RUM")

barplot_f(TOTAL_WLD, "OTHU", "AGR")
barplot_f(TOTAL_WLD, "OTHU", "CRP")
barplot_f(TOTAL_WLD, "OTHU", "LSP")
barplot_f(TOTAL_WLD, "OTHU", "WHT")
barplot_f(TOTAL_WLD, "OTHU", "CGR")
barplot_f(TOTAL_WLD, "OTHU", "DRY")
barplot_f(TOTAL_WLD, "OTHU", "NRM")
barplot_f(TOTAL_WLD, "OTHU", "RIC")
barplot_f(TOTAL_WLD, "OTHU", "RUM")

barplot_f(TOTAL_WLD, "LAND", "AGR")
barplot_f(TOTAL_WLD, "LAND", "CRP")
barplot_f(TOTAL_WLD, "LAND", "GRS")

barplot_f(TOTAL_WLD, "AREA", "AGR")
barplot_f(TOTAL_WLD, "AREA", "CRP")
barplot_f(TOTAL_WLD, "AREA", "LSP")

barplot_f(TOTAL_WLD, "YILD", "GRS")
barplot_f(TOTAL_WLD, "YILD", "CRP")

barplot_f(TOTAL_WLD, "YEXO", "CRP")
#barplot_f(TOTAL_WLD, "YEXO", "GRS")

barplot_f(TOTAL_WLD, "NETT", "AGR")
barplot_f(TOTAL_WLD, "NETT", "CRP")
barplot_f(TOTAL_WLD, "NETT", "LSP")
barplot_f(TOTAL_WLD, "NETT", "WHT")
barplot_f(TOTAL_WLD, "NETT", "CGR")
barplot_f(TOTAL_WLD, "NETT", "DRY")
barplot_f(TOTAL_WLD, "NETT", "NRM")
barplot_f(TOTAL_WLD, "NETT", "RIC")
barplot_f(TOTAL_WLD, "NETT", "RUM")

barplot_f(TOTAL_WLD, "IMPO", "AGR")
barplot_f(TOTAL_WLD, "IMPO", "CRP")
barplot_f(TOTAL_WLD, "IMPO", "LSP")
barplot_f(TOTAL_WLD, "IMPO", "WHT")
barplot_f(TOTAL_WLD, "IMPO", "CGR")
barplot_f(TOTAL_WLD, "IMPO", "DRY")
barplot_f(TOTAL_WLD, "IMPO", "NRM")
barplot_f(TOTAL_WLD, "IMPO", "RIC")

barplot_f(TOTAL_WLD, "EXPO", "AGR")
barplot_f(TOTAL_WLD, "EXPO", "CRP")
barplot_f(TOTAL_WLD, "EXPO", "LSP")
barplot_f(TOTAL_WLD, "EXPO", "WHT")
barplot_f(TOTAL_WLD, "EXPO", "CGR")
barplot_f(TOTAL_WLD, "EXPO", "DRY")
barplot_f(TOTAL_WLD, "EXPO", "NRM")
barplot_f(TOTAL_WLD, "EXPO", "RIC")

barplot_f(TOTAL_WLD, "CONS", "AGR")
barplot_f(TOTAL_WLD, "CONS", "CRP")
barplot_f(TOTAL_WLD, "CONS", "LSP")
barplot_f(TOTAL_WLD, "CONS", "WHT")
barplot_f(TOTAL_WLD, "CONS", "CGR")
barplot_f(TOTAL_WLD, "CONS", "DRY")
barplot_f(TOTAL_WLD, "CONS", "NRM")
barplot_f(TOTAL_WLD, "CONS", "RIC")
barplot_f(TOTAL_WLD, "CONS", "RUM")

barplot_f(TOTAL_WLD, "EMIS", "TOT")
barplot_f(TOTAL_WLD, "EMIS", "AGR")
barplot_f(TOTAL_WLD, "EMIS", "CRP")
barplot_f(TOTAL_WLD, "EMIS", "LSP")

barplot_f(TOTAL_WLD, "ECO2", "TOT")
#barplot_f(TOTAL_WLD, "ECO2", "AGR")
#barplot_f(TOTAL_WLD, "ECO2", "CRP")
#barplot_f(TOTAL_WLD, "ECO2", "LSP")

barplot_f(TOTAL_WLD, "ECH4", "TOT")
barplot_f(TOTAL_WLD, "ECH4", "AGR")
barplot_f(TOTAL_WLD, "ECH4", "CRP")
barplot_f(TOTAL_WLD, "ECH4", "LSP")

barplot_f(TOTAL_WLD, "GDPT", "TOT")
barplot_f(TOTAL_WLD, "POPT", "TOT")
dev.off()

xtabs(~variable + unit, data = MAgPIE)
# bar graph - index

##############################
# Part below is from FOODSECURE, not adapted yet to agCLIM50

# Comparison of consumption
CONS <- TOTAL2 %>% 
        filter(variable == "CONS")
xtabs(~model + sector, data = CONS)

# Line plot - index
CONS_lineplot_i <- CONS %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/CONS_lineplot_i.pdf", width = 12, height = 7)
CONS_lineplot_i$plots
dev.off()

# Comparison AREA and LAND
LAND_AREA <- TOTAL %>%
  filter(variable %in% c("AREA", "LAND")) 

# Line plot - index
LAND_AREA_lineplot_i <- LAND_AREA %>%
  group_by(variable, item) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = file.path(dataPath, "Graphs/LAND_AREA_line_i.pdf"), width = 12, height = 7)
LAND_AREA_lineplot_i$plots
dev.off()

# Line plot - value
LAND_AREA_lineplot_ha <- LAND_AREA %>%
  group_by(variable, sector) %>%
  do(plots = lineplot_f(., "1000 ha")) 

pdf(file = "./Graphs/LAND_AREA_line_ha.pdf", width = 7, height = 7)
LAND_AREA_lineplot_ha$plots
dev.off()

# 
# LAND_AREA_lineplot_WLD <- LAND_AREA_db %>%
#   filter(FSregion == "WLD") %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = lineplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/LAND_AREA_line_WLD.pdf", width = 7, height = 7)
# IM_GL_lineplot_WLD$plots
# dev.off()

# 
# # BW plot
# LAND_AREA_bwplot <- LAND_AREA_db %>%
#   group_by(variable, sector) %>%
#   select(-value) %>%
#   rename(value = index) %>%
#   do(plots = bwplot_f(., "Index")) 
# 
# pdf(file = "./Results/Graphs/LAND_AREA_bw.pdf", width = 7, height = 7)
# IM_GL_bwplot$plots
# dev.off()

# Compare base data models (by = 2010)
LAND_AREA_baseplot <- LAND_AREA %>%
    filter(scenario == "ECO") %>%
    group_by(variable, sector, scenario) %>%
  do(plots = baseplot_f(., 2010)) 

pdf(file = "./Graphs/LAND_AREA_base.pdf", width = 7, height = 7)
LAND_AREA_baseplot$plots
dev.off()

## Comparison YILD, PROD AND AREA

YILD_PROD_AREA <- filter(TOTAL2, variable %in% c("AREA", "PROD", "YILD"), unit != "USD 2000/cap/d") %>% 
  filter(model != "IMAGE")
xtabs(~model + sector + FSregion, data = YILD_PROD_AREA)

# compare index
YILD_PROD_AREA_lineplot_i <- YILD_PROD_AREA %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/YILD_PROD_AREA_line_i.pdf", width = 7, height = 7)
YILD_PROD_AREA_lineplot_i$plots
dev.off()


# Comparison FS indicators
# Calorie consumption per capita per day
CALO <- filter(TOTAL2, variable == "CALO" & unit == "kcal/cap/d" & sector == "TOT")

# compare index
CALO_lineplot_i <- CALO %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Graphs/CALO_line_i.pdf", width = 7, height = 7)
CALO_lineplot_i$plots
dev.off()













# WDI data
# WDI <- WDI(country="all", indicator=c("SP.POP.TOTL"), 
#            start=1960, end=2015) %>%
#   mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
#   filter(!is.na(iso3c)) %>%
#   select(POP = SP.POP.TOTL, iso3c)

#saveRDS(WDI, file = paste("./Data/Add_data", "WDI_", Sys.Date(), ".rds", sep=""))
WDI <- readRDS(file = "./Data/Add_data/WDI_2016-05-25.rds")

# Region concordance
MAGNET2FS_REG <- read.csv(".\\Mappings\\MAGNET2FS_REG.csv") %>%
  dplyr::select(Region = FS_region2, FS_region_name_short) %>%
  unique()

# Country concordance
FS2ISO_REG <- read.csv(".\\Mappings\\FStoISO_MAGNETCountryAggregation.csv") %>%
  select(FS_region_name_short, iso3c = ISO) %>%
  left_join(., MAGNET2FS_REG)

## CALORIE CONSUMPTION
# Load historical data
histcal_r <- read.csv("./Data/Add_data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year, Region) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T))

histcal_w <- read.csv("./Data/Add_data/calcpcpd.csv") %>%
  mutate(iso3c = countrycode(AreaCode, "fao", "iso3c")) %>%
  filter(!is.na(iso3c)) %>% #remove reg aggregates
  select(iso3c, year = Year, value = Value) %>%
  left_join(., FS2ISO_REG) %>% 
  left_join(., WDI) %>%
  group_by(year) %>%
  summarize(value = sum(value*POP, na.rm = T)/sum(POP, na.rm = T)) %>%
  mutate(Region = "WLD")

scen <- expand.grid(Scenario = unique(TOTAL$scenario), Region = unique(TOTAL$FSregion))  
histcal <- rbind(histcal_r, histcal_w) %>%
  left_join(scen, .) %>%
  filter(year <=2010) %>%
  rename(scenario = Scenario, FSregion = Region) %>%
  filter(year >=1990)

histcal_base <- filter(histcal, year == 2010) %>%
  rename(Base2010 = value) %>%
  select(-year) 

# Rebase simulations 2010 to historical data (2010=100)
CALO_i <- CALO %>%
  group_by(scenario, FSregion) %>%
  left_join(., histcal_base) %>%
  mutate(value = Base2010*index/100)

# kcd plot
pdf(file = "./Results/Graphs/CAL_line_kcd.pdf", width = 12, height = 12)
#bwplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO, "index")
dev.off()

# Plot
pdf(file = "./Results/Graphs/CAL_line_i.pdf", width = 12, height = 12)
#lineplot2_f(CALO_i, histcal, "kcal/cap/d")
lineplot_f(CALO_i, "kcal/cap/d")
dev.off()

# Prices

PRICES <- TOTAL2 %>%
  filter(variable %in% c("XPRI") & sector %in% c("AGR", "CRP"))

PRICES_i <- PRICES %>%
  group_by(variable, sector) %>%
  select(-value) %>%
  rename(value = index) %>%
  do(plots = lineplot_f(., "Index")) 

pdf(file = "./Results/Graphs/PRICES_line_i.pdf", width = 7, height = 7)
PRICES_i$plots
dev.off()









