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
MAgPIE <- read_csv(file.path(dataPath, "agclim50_MAgPIE.csv")) 

# Check
summary(MAgPIE)
xtabs(~Item + Variable, data = MAgPIE)

# Some values are infinite => set to NA
inf <- filter(MAgPIE, is.infinite(Value))
MAgPIE$Value[is.infinite(MAgPIE$Value)] <- NA

# Rename several items that have wrong name
# CHECK WITH MAgPIE about other names..
MAgPIE$Item[MAgPIE$Item == "LVS"] <- "LSP"
MAgPIE$Item[MAgPIE$Item == "SUG"] <- "SGC"

# Check if there are variables with missing information for 2010
# There are a few combination in MAgPIE that lack 2010 data
check <- MAgPIE %>%
  arrange(Model, Scenario, Region, Item, Variable, Year) %>%
  group_by(Model, Scenario, Region, Item, Variable) %>%
  filter(!any(Year==2010))

# Remove series with missing values in 2010
MAgPIE <- MAgPIE %>%
  arrange(Model, Scenario, Region, Item, Variable, Year) %>%
  group_by(Model, Scenario, Region, Item, Variable) %>%
  filter(any(Year==2010)) 
xtabs(~Item + Variable, data = MAgPIE)


# MAGNET
MAGNET <- read_csv(file.path(dataPath, "MAGNET_agCLIM50_2016-08-31.csv")) %>%
            rename(Year = year, Model = model, Scenario = scenario, Region = region, Item = sector, Variable = variable, Unit = unit, Value = value)

# Bind in one file
TOTAL <- rbind(MAGNET, MAgPIE) %>% 
              filter(Year>=2010)

# Index (2010=100)
# CHECK: GLOBIOM has various results for CALO with different units
# CHECK: GLOBIOM PROD has wrong unit.
# CHECK: product groups GLOBION. VFN = V_F? AGR in MAGNET?
# CHECK HARMONISATION FOOD GROUPS IN TEMPLATE.

TOTAL <- TOTAL %>%
  arrange(Model, Scenario, Region, Item, Variable, Year) %>%
  group_by(Model, Scenario, Region, Item, Variable, Unit) %>%
  mutate(index = Value/Value[Year==2010]*100) %>%
  arrange(Model, Scenario, Variable, Region, Item, Year)

xtabs(~Model + Variable, data = TOTAL2)
xtabs(~Model + Item, data = TOTAL2)
xtabs(~Model + Scenario, data = TOTAL2)
write_csv(TOTAL, file.path(dataPath, "TOTAL.csv"))
