# PROJECT: FOODSECURE WP7
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# Merge results from different MAGNET runs
# ``````````````````````````````````````````````````````````````````````````````````````````````````
# `````````````````````````````````````````````````````````````````````````````````````````````````` 

# PACKAGES"
BasePackages <- c("readr", "readxl", "foreign", "stringr", "car", "zoo", "tidyr", "RColorBrewer", "plyr", "dplyr", "ggplot2", "scales")
lapply(BasePackages, library, character.only = TRUE)
#SpatialPackages <- c("rgdal", "gdalUtils", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4")
#lapply(SpatialPackages, library, character.only = TRUE)
AdditionalPackages <-  c("WDI", "countrycode")
lapply(AdditionalPackages, library, character.only = TRUE)

# SET PATHS
wdPath<-"D:\\R\\agCLIM50"
setwd(wdPath)

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

# Read MAGNET data per model and merge
MAGNETruns <- list()
MAGNETruns[[1]] <- read_csv("Cache/agclim50_MAGNET_2016-09-13.csv")
MAGNETruns <- bind_rows(MAGNETruns) %>%
  mutate(modelrun = "2016-09-13") %>%
  na.omit
#xtabs(~Modelrun + variable, data = MAGNETruns)


# Line plot to compare models
lineplot_f <- function(df){
  
  title = unique(with(df, paste(variable, item, sep="_")))
  point <- filter(df, year == 2050)
  
  p = ggplot() +
    geom_line(data = df, aes(x = year, y = value, colour = scenario), size = 0.5) +
    geom_point(data = point, aes(x = year, y = value, colour = scenario, shape = scenario)) +
    #scale_colour_manual(values = c("green","cyan","red","purple", "brown", "grey"), name="Scenario")+ 
    #scale_linetype_manual(values=c("solid","longdash", "dotted"), name = "Modelrun") +
    scale_shape_manual(values=seq(0,15), name = "Scenario") +
    ylab(unique(df$unit)) + xlab("") +
    facet_wrap(~region, scale = "free")
  
  p = p +ggtitle(title) 
  
  p = p + guides(fill = guide_legend(keywidth = 1, keyheight = 1, override.aes = 
                                       list(alpha = 0.1, size = 0.5, colour = c("green","cyan","red","purple"))))
  
  p = p + theme_classic() +
    theme(panel.border = element_blank(),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black")) +
    scale_x_continuous(limits = c(2010,2050.1), breaks = seq(2010,2050,by=10),expand=c(0,0)) + # note that: Limits sets the limits for the axes (added 0.1) but always some space is added. Latter is controlled by expand
    scale_y_continuous(labels = comma) +
    theme(legend.background=element_blank()) +
    theme(legend.key=element_rect(size=0.5, color="white"), # Increase space between legend boxes - doubt if it works
          strip.background = element_rect(colour="white", fill="white")) # Remove box and background of facet
  
  
  p
}


# Main regions
mainRegions <- filter(MAGNETruns,region %in% c("NAM", "OAM", "AME", "SAS", "EUR", "FSU", "SSA", "WLD"))

plot_i <- mainRegions %>%
  group_by(variable, item, unit) %>%
  do(plots = lineplot_f(.)) 

pdf(file = "Graphs/agCLIM50MAGNET_MR.pdf", width = 10, height = 7)
plot_i$plots
dev.off()
rm(plot_i)

# Smaller regions
smallRegions <- filter(MAGNETruns,region %in% c("CAN", "USA", "BRA", "OSA", "MEN", "CHN", "IND", "SEA", "OAS", "ANZ"))
plot_i <- smallRegions %>%
  group_by(variable, sector, unit) %>%
  do(plots = lineplot_f(.)) 

pdf(file = "Graphs/agCLIM50MAGNET_SR.pdf", width = 10, height = 7)
plot_i$plots
dev.off()
rm(plot_i)

