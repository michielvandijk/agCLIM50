# PROJECT: agCLIM50
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Merge resuls from different models
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "dplyr", "ggplot2", "readxl", "readr")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET DATAPATH
source("Code/get_dataPath.r")


# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)

# FUNCTIONS

# READ DATA

# MAgPIE
# Process
MAgPIE <- read_csv(file.path(dataPath, "ModelResults\\agclim50_MAgPIE_2016-09-22.csv")) %>%
  rename(model = Model, scenario = Scenario, region = Region, item = Item, unit = Unit, variable = Variable, year = Year, value = Value)

# Check
summary(MAgPIE)
xtabs(~item + variable, data = MAgPIE)
xtabs(~unit + variable, data = MAgPIE)

# Check if there are variables with missing information for 2010
# There are a few combination in MAgPIE that lack 2010 data => corrected in later updates
check2010 <- MAgPIE %>%
  arrange(model, scenario, region, item, variable, year) %>%
  group_by(model, scenario, region, item, variable) %>%
  filter(!any(year==2010))

# # Remove series with missing values in 2010
# MAgPIE <- MAgPIE %>%
#   arrange(model, scenario, region, item, variable, unit, year) %>%
#   group_by(model, scenario, region, item, unit, variable) %>%
#   filter(any(year==2010)) 
# xtabs(~item + variable, data = MAgPIE)

# GLOBIOM
# Process
GLOBIOM <- read_csv(file.path(dataPath, "ModelResults\\agclim50_GLOBIOM_20160926.csv")) %>%
  rename(model = Model, scenario = Scenario, region = Region, item = Item, unit = Unit, variable = Variable, year = Year, value = Value) %>%
  mutate(variable = toupper(variable))

# Check
summary(GLOBIOM)
xtabs(~item + variable, data = GLOBIOM)
xtabs(~variable + unit, data = GLOBIOM)

# GLOBIOM has duplicate units for some variable. I exclude 1000 dm t and dm t/ha
# GLOBIOM <- filter(GLOBIOM, !(unit %in% c("1000 dm t", "dm t/ha")))

# Check if there are variables with missing information for 2010
check2010 <- GLOBIOM %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
GLOBIOM <- GLOBIOM %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, unit, variable) %>%
  filter(any(year==2010)) 
xtabs(~item + variable, data = GLOBIOM)

# IMAGE
# Process
IMAGE <- read_csv(file.path(dataPath, "ModelResults\\AGCLIM50_IMAGE_23092016.csv")) %>%
  rename(model = Model, scenario = Scenario, region = Region, item = Item, unit = Unit, variable = Variable, year = Year, value = Value) %>%
  mutate(year = as.numeric(year))

# Check
summary(IMAGE)
xtabs(~item + variable, data = IMAGE)

# Check if there are variables with missing information for 2010
check2010 <- IMAGE %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# MAGNET
MAGNET <- read_csv(file.path(dataPath, "ModelResults\\agCLIM50_MAGNET_2016-10-06.csv")) 
xtabs(~item + scenario, data = MAGNET)

# CAPRI
CAPRI <- read_csv(file.path(dataPath, "ModelResults\\agclim50_CAPRI_20170302.csv")) %>%
  setNames(c("region", "variable", "item", "year", "scenario", "value")) %>%
  mutate(model = "CAPRI",
         unit = NA)
xtabs(~year + variable, data = CAPRI)
# 
# # CAPRI is missing base data values. It has BASELINE values. I assume this is the base for each scenario
# oldBase <- filter(CAPRI, year == 2010)
# scenarios <- unique(MAGNET$scenario)
# scenRep_f <- function(scen){
#   df <- oldBase %>% 
#     mutate(scenario = scen)
# }
# newBase <- bind_rows(lapply(scenarios, scenRep_f))
# CAPRI <- bind_rows(newBase, CAPRI) %>%
#   filter(scenario != "BASELINE")
  
# Remove CAPRI baseline
CAPRI <- filter(CAPRI, scenario != "BASELINE")
xtabs(~item + scenario, data = CAPRI)

check2010 <- CAPRI %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
CAPRI <- CAPRI %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, unit, variable) %>%
  filter(any(year==2010)) 
xtabs(~item + variable, data = CAPRI)

# CAPRI includes more than the core regions. I filter them out
xtabs(~item + region, data = CAPRI)
regions <- unique(GLOBIOM$region) 
CAPRI <- filter(CAPRI, region %in% regions)
xtabs(~year + variable, data = CAPRI)

# Bind in one file
TOTAL <- bind_rows(MAGNET, MAgPIE, GLOBIOM, IMAGE, CAPRI) %>% 
              filter(year>=2010)
summary(TOTAL)

# Calculate index
TOTAL <- TOTAL %>%
  arrange(model, scenario, region, item, variable, unit, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  mutate(index = (value/value[year==2010]*1)) %>%
  arrange(model, scenario, variable, region, item, unit, year)

# Remove NAN values for MAgPIE (GDPT = 0)
inf.nan.na.clean_f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  return(x)
}

TOTAL <- inf.nan.na.clean_f(TOTAL) %>% 
  filter(!is.na(index))

# Checks on missing values
check <- TOTAL %>%
  group_by(model, variable, item) %>%
  summarize(
    n=n()
    #miss = sum(is.na(Value)),
    #mean = mean(value, na.rm=TRUE)
  ) %>%
  spread(model, n)

xtabs(~unit + model, data = TOTAL)
xtabs(~variable + model, data = TOTAL)
xtabs(~region + model, data = TOTAL)
xtabs(~scenario + model, data = TOTAL)

# Save data
write_csv(TOTAL, file.path(dataPath, paste0("ModelResults\\TOTAL_", Sys.Date(), ".csv")))
