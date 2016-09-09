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

# SET PATHS
wdPath<-"D:\\Data\\Projects\\agCLIM50"
setwd(wdPath)

dataPath <- "D:\\Dropbox\\AgClim50 scenario results"

# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)

# FUNCTIONS
# Plus function that ensures NA + NA is NA not 0 as in sum
plus <- function(x) {
  if(all(is.na(x))){
    c(x[0],NA)} else {
      sum(x,na.rm = TRUE)}
}

# ma function to calculate moving average
ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}

# ANALYSIS
# Combine files of four models and add FAOSTAT data
# MAGNET2FS_REG <- read.csv(file.path(".\\Mappings\\FStoISO_MAGNETCountryAggregation.csv")) %>%
#   dplyr::select(Region = FS_region_name, FS_region_name_short) %>%
#   unique()


## Read data per model

# MAgPIE
# Process
MAgPIE <- read_csv(file.path(dataPath, "ModelResults\\agclim50_MAgPIE.csv")) %>%
  rename(model = Model, scenario = Scenario, region = Region, item = Item, unit = Unit, variable = Variable, year = Year, value = Value)

# Check
summary(MAgPIE)
xtabs(~item + variable, data = MAgPIE)

# Some values are infinite => set to NA
inf <- filter(MAgPIE, is.infinite(value))
MAgPIE$value[is.infinite(MAgPIE$value)] <- NA

# Rename several items that have wrong name
# CHECK WITH MAgPIE about other names..
MAgPIE$item[MAgPIE$item == "LVS"] <- "LSP"
MAgPIE$item[MAgPIE$item == "SUG"] <- "SGC"

# Check if there are variables with missing information for 2010
# There are a few combination in MAgPIE that lack 2010 data
check <- MAgPIE %>%
  arrange(model, scenario, region, item, variable, year) %>%
  group_by(model, scenario, region, item, variable) %>%
  filter(!any(year==2010))

# Remove series with missing values in 2010
MAgPIE <- MAgPIE %>%
  arrange(model, scenario, region, item, variable, year) %>%
  group_by(model, scenario, region, item, variable) %>%
  filter(any(year==2010)) 
xtabs(~item + variable, data = MAgPIE)


# MAGNET
MAGNET <- read_csv(file.path(dataPath, "ModelResults\\agCLIM50_MAGNET_2016-09-01.csv")) 

# Bind in one file
TOTAL <- rbind(MAGNET, MAgPIE) %>% 
              filter(year>=2010)

# Index (2010=100)
# CHECK: GLOBIOM has various results for CALO with different units
# CHECK: GLOBIOM PROD has wrong unit.
# CHECK: product groups GLOBION. VFN = V_F? AGR in MAGNET?
# CHECK HARMONISATION FOOD GROUPS IN TEMPLATE.

TOTAL <- TOTAL %>%
  arrange(model, scenario, region, item, variable, year) %>%
  group_by(model, scenario, region, item, variable, unit) %>%
  mutate(index = value/value[year==2010]*100) %>%
  arrange(model, scenario, variable, region, item, year)

# Checks on missing values
check <- TOTAL %>%
  group_by(model, variable, item) %>%
  summarize(
            #n=n()
            #miss = sum(is.na(Value)),
            mean = mean(value, na.rm=TRUE)
            ) %>%
  spread(model, mean)


# Remove NAN values for MAgPIE (GDPT = 0)
inf.nan.na.clean_f<-function(x){
  x[do.call(cbind, lapply(x, is.nan))]<-NA
  x[do.call(cbind, lapply(x, is.infinite))]<-NA
  return(x)
}
TOTAL <- inf.nan.na.clean_f(TOTAL) %>% 
  filter(!is.na(index))

xtabs(~model + variable, data = TOTAL)
xtabs(~model + item, data = TOTAL)
xtabs(~model + scenario, data = TOTAL)
write_csv(TOTAL, file.path(dataPath, "ModelResults\\TOTAL.csv"))
