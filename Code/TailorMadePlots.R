# PROJECT: FOODSECURE agCLIM50
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Plot resuls from different models
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES
BasePackages <- c("readr", "readxl", "foreign", "stringr", "gdata", "car", "zoo", "tidyr", "RColorBrewer", "dplyr", "ggplot2", "openxlsx", "scales", "lazyeval", "ggthemes", "scales")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET PATHS
wdPath <- "D:\\Data\\Projects\\agCLIM50"
setwd(wdPath)

dataPath <- "D:\\Dropbox\\AgClim50 scenario results"


# R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)

## COMPARE ALL
# read data
TOTAL <- read_csv(file.path(dataPath, "ModelResults\\TOTAL.csv"))
xtabs(~ variable + model, data = TOTAL)

# Price bar plot
price <- filter(TOTAL, variable == "XPRP", item == "CRP", region == "WLD", year == 2050)
xtabs(~model+scenario, data = price)

ggplot(data = price, aes(x = model, y = index, fill = model)) +
  geom_bar(stat="identity", colour = "black") + facet_wrap(~scenario) +
  guides(fill=FALSE) +
  ggtitle("XPRP_CRP") +
  ylab("Index (2010=100")

ggplot(data = price, aes(x = scenario, y = index, fill = scenario)) +
  geom_bar(stat="identity", colour = "black") + facet_wrap(~model) +
  ggtitle("XPRP_CRP") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Index (2010=100")

ggsave("price.pdf", width = 12, height = 10)


# Plots requested by Elke
var <- c("ECH4")

MIT <- filter(TOTAL, variable %in% var, item == "TOT", 
              scenario %in% c("SSP1_NoCC", "SSP2_NoCC", "SSP3_NoCC"))
xtabs(~model+scenario, data = MIT)

plot1_f <- function(df){
 point <- filter(df, year == 2050)
 title <- paste(unique(df$variable))
 p = ggplot() +
      geom_line(data = df, aes(x = year, y = index, colour = scenario, linetype = model)) + 
      geom_point(data = point, aes(x= year, y = index, shape = model, colour = scenario)) + 
      scale_x_continuous(limits = c(2010,2051), breaks = seq(2010,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
      scale_y_continuous(labels = comma) +
      ggtitle(title) +
      ylab("Index (2010=100)") +
      facet_wrap(~region) +
      theme_bw()
   
 p
}

plot2_f <- function(df){
  title <- paste(unique(df$variable))
  p = ggplot(data = df, aes(x = scenario, y = index, fill = scenario)) +
    geom_bar(stat="identity", colour = "black") + 
    facet_wrap(~model) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Index (2010=100")
  p
}

plot1_f(MIT)
plot2_f(filter(MIT, region == "WLD", year == 2050))

ggsave("MIT1.pdf", width = 12, height = 10)
