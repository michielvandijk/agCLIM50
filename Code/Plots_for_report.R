#'========================================================================================================================================
#' Project:  AgCLIM50
#' Subject:  Plots for ERL paper
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot","labelled", "formattable")
# Spatial packages
#p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "leaflet", "mapview")
# Additional packages
p_load("grid")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)
setwd(root)


### R SETTINGS
options(scipen=99) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=2)


### SET DATAPATH
source(file.path(root, "code/get_dataPath.r"))


# LOAD DATA
TOTAL_raw <- read.csv(file.path(dataPath, "ModelResults/TOTAL_2017-10-31.csv"))
TOTAL <- TOTAL_raw %>%
  mutate(scenario = forcats::fct_relevel(scenario,
                                         c("SSP1_NoCC", "SSP1_CC6", "SSP1_NoCC_m", "SSP1_CC26_m",
                                           "SSP2_NoCC", "SSP2_CC6", "SSP2_NoCC_m", "SSP2_CC26_m",
                                           "SSP3_NoCC", "SSP3_CC6", "SSP3_NoCC_m", "SSP3_CC26_m"))) %>%
  mutate(ssp = substring(scenario, 1, 4),
         scenario2 = factor(substring(scenario, 6), levels = c("NoCC", "NoCC_m", "CC6", "CC26_m")),
         id = paste(item, variable, sep ="_"),
         diff = (index - 1)*100)

# filter out fm t/ha which is only presented by GLOBIOM
TOTAL <- filter(TOTAL, !(unit == "fm t/ha" & model == "GLOBIOM"))

# Create database for plotting
sel <- c("AGR_PROD", "CRP_AREA", "AGR_XPRP", "LSP_XPRP", "AGR_ECH4", "AGR_EN2O", "TOT_GDPT", "TOT_POPT", "LSP_AREA", "AGR_EMIS")
TOTAL <- filter(TOTAL, year == 2050, id %in% sel)
xtabs(~variable + item, data = TOTAL)
saveRDS(TOTAL, "Cache/TOTAL.rds")
TOTAL <- readRDS("Cache/TOTAL.rds")

# Create database for results
TOTAL_WLD <- filter(TOTAL, region == "WLD")

# Create database for difference with NOCC
nocc_net <- TOTAL_WLD %>%
  group_by(id, model, ssp) %>%
  mutate(nocc_net = diff - diff[scenario2 == "NoCC"]) %>%
  filter(scenario2 != "NoCC")

# BAR GRAPHS for WLD
# Create colours
red <- colorRampPalette(c("orange", "darkred"))
blue <- colorRampPalette(c("skyblue", "darkblue"))
green <- colorRampPalette(c("greenyellow", "darkgreen"))
colour <- c(green(4), blue(4), red(4))
names(colour) <- levels(TOTAL_WLD$scenario)


# Function to plot bar graph
barplot_f <- function(df, var, itm){
  df <- filter(df, variable == var, item == itm)
  title <- paste(var, itm, sep = "_")
  ssp1 <- textGrob("SSP1", gp=gpar(fontsize=11))
  ssp2 <- textGrob("SSP2", gp=gpar(fontsize=11))
  ssp3 <- textGrob("SSP3", gp=gpar(fontsize=11))
  
  p = ggplot(data = df, aes(x = scenario, y = index, fill = scenario)) +
    scale_fill_manual(values = colour) +
    geom_bar(stat="identity", colour = "black") + 
    facet_wrap(~model) +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ylab("Index (2010=1)") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 4.5, linetype = "dashed") +
    geom_vline(xintercept = 8.5, linetype = "dashed") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    annotation_custom(ssp1, xmin = 2.25, xmax = 2.25, ymin= 2, ymax = 2) + 
    annotation_custom(ssp2, xmin = 6.25, xmax = 6.25, ymin= 2, ymax = 2) +
    annotation_custom(ssp3, xmin = 10.25, xmax = 10.25, ymin= 2, ymax = 2) +
    scale_y_continuous(expand = c(0,0)) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2)) +
    theme(plot.margin = unit(c(1,1,3,1), "cm"))
  p
}    

barplot2_f <- function(df, var, itm, ypos, y_min, y_max){
  df <- filter(df, variable == var, item == itm)
  ssp1 <- textGrob("SSP1", gp=gpar(fontsize=10))
  ssp2 <- textGrob("SSP2", gp=gpar(fontsize=10))
  ssp3 <- textGrob("SSP3", gp=gpar(fontsize=10))
  
  p = ggplot(data = df, aes(x = scenario, y = index, fill = scenario2)) +
    scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
    geom_bar(stat="identity", colour = "black") + 
    facet_wrap(~model) +
    labs( y = "Index (2010=1)", x = "", fill = "") +
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 4.5, linetype = "dashed") +
    geom_vline(xintercept = 8.5, linetype = "dashed") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    annotation_custom(ssp1, xmin = 2.25, xmax = 2.25, ymin= ypos, ymax = ypos) + 
    annotation_custom(ssp2, xmin = 6.25, xmax = 6.25, ymin= ypos, ymax = ypos) +
    annotation_custom(ssp3, xmin = 10.25, xmax = 10.25, ymin= ypos, ymax = ypos) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme(legend.position = "bottom") +
    theme(strip.background = element_blank())
    
  p
}    

barplot2_f(TOTAL_WLD, "PROD", "AGR", ypos = 1.75, 0.5, 1.85)
barplot2_f(TOTAL_WLD, "AREA", "CRP", ypos = 1.45, 0.5, 1.5)
barplot2_f(TOTAL_WLD, "AREA", "LSP", ypos = 1.45, 0.5, 1.5)
barplot2_f(TOTAL_WLD, "XPRP", "AGR", ypos = 2.5, 0.5, 2.6)
barplot2_f(TOTAL_WLD, "XPRP", "LSP", ypos = 2.5, 0.5, 2.6)
barplot2_f(TOTAL_WLD, "ECH4", "AGR", ypos = 1.9, 0.5, 2)
barplot2_f(TOTAL_WLD, "EN2O", "AGR", ypos = 1.9, 0.5, 2)


barplot3_f <- function(df, var, itm, ypos, y_min, y_max){
  df <- filter(df, variable == var, item == itm)
  ssp1 <- textGrob("SSP1", gp=gpar(fontsize=10))
  ssp2 <- textGrob("SSP2", gp=gpar(fontsize=10))
  ssp3 <- textGrob("SSP3", gp=gpar(fontsize=10))
  
  p = ggplot(data = df, aes(x = scenario, y = diff, fill = scenario2)) +
    scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
    geom_bar(stat="identity", colour = "black") + 
    facet_wrap(~model) +
    labs( y = "2010-2050 growth (%)", x = "", fill = "") +
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    geom_vline(xintercept = 4.5, linetype = "dashed") +
    geom_vline(xintercept = 8.5, linetype = "dashed") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    annotation_custom(ssp1, xmin = 2.25, xmax = 2.25, ymin= ypos, ymax = ypos) + 
    annotation_custom(ssp2, xmin = 6.25, xmax = 6.25, ymin= ypos, ymax = ypos) +
    annotation_custom(ssp3, xmin = 10.25, xmax = 10.25, ymin= ypos, ymax = ypos) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme(legend.position = "bottom") +
    theme(strip.background = element_blank())
  
  p
}    

barplot3_f(TOTAL_WLD, "PROD", "AGR", ypos = 75, 0, 80)
barplot3_f(TOTAL_WLD, "AREA", "CRP", ypos = 40, -15, 45)
barplot3_f(TOTAL_WLD, "AREA", "LSP", ypos = 20, -15, 25)
barplot3_f(TOTAL_WLD, "XPRP", "AGR", ypos = 160, -40, 170)
barplot3_f(TOTAL_WLD, "XPRP", "LSP", ypos = 160, -30, 170)
barplot3_f(TOTAL_WLD, "ECH4", "AGR", ypos = 90, -50, 100)
barplot3_f(TOTAL_WLD, "EN2O", "AGR", ypos = 90, -50, 100)


# Mean results
TOTAL_WLD_mean <- TOTAL_WLD %>%
  group_by(id, ssp, scenario2) %>%
  mutate(mean = mean(diff))

df <- filter(TOTAL_WLD_mean, variable == "PROD", item == "AGR")

barplot4_f <- function(df, var, itm){
  df <- filter(df, variable == var, item == itm)
  
  p = ggplot() +
    scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
    geom_bar(data = filter(df, model == "GLOBIOM"), aes(x = ssp, y = mean, fill = ssp), stat="identity", colour = "black") +
    geom_point(data = df, aes(x = ssp, y = diff, shape = model))  + 
    labs( y = "2010-2050 growth (%)", x = "", fill = "", shape = "") +
    facet_wrap(~scenario2, nrow = 1) +
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    #scale_y_continuous(expand = c(0,0)) +
    #coord_cartesian(ylim = c(y_min, y_max)) +
    theme(legend.position = "bottom") +
    theme(strip.background = element_blank()) +
    guides(fill = FALSE)
  
  p
}    


barplot4_f(TOTAL_WLD_mean, "PROD", "AGR")
barplot4_f(TOTAL_WLD_mean, "AREA", "CRP")
barplot4_f(TOTAL_WLD_mean, "AREA", "LSP")
barplot4_f(TOTAL_WLD_mean, "XPRP", "AGR")
barplot4_f(TOTAL_WLD_mean, "XPRP", "LSP")
barplot4_f(TOTAL_WLD_mean, "ECH4", "AGR")
barplot4_f(TOTAL_WLD_mean, "EN2O", "AGR")


### PLOTS TO SHOW DIFFERENCE BETWEEN SCENARIOS
# CC6 - NoCC
cc6_nocc <- TOTAL_WLD %>%
  group_by(id, model, ssp) %>%
  mutate(net_val = diff - diff[scenario2 == "NoCC"]) %>%
  filter(scenario2 %in% c("CC6")) %>%
  ungroup() %>%
  group_by(id, ssp) %>%
  mutate(mean = mean(net_val),
         net = "cc6_nocc")

# NoCC_m - NoCC
noccm_nocc <- TOTAL_WLD %>%
  group_by(id, model, ssp) %>%
  mutate(net_val = diff - diff[scenario2 == "NoCC"]) %>%
  filter(scenario2 %in% c("NoCC_m")) %>%
  ungroup() %>%
  group_by(id, ssp) %>%
  mutate(mean = mean(net_val),
         net = "noccm_nocc")

# cc26_m - NoCC
cc26m_nocc <- TOTAL_WLD %>%
  group_by(id, model, ssp) %>%
  mutate(net_val = diff - diff[scenario2 == "NoCC"]) %>%
  filter(scenario2 %in% c("CC26_m")) %>%
  ungroup() %>%
  group_by(id, ssp) %>%
  mutate(mean = mean(net_val),
         net = "cc26m_nocc")

# CC26_m - NoCC_m
cc26m_noccm <- TOTAL_WLD %>%
  group_by(id, model, ssp) %>%
  mutate(net_val = diff - diff[scenario2 == "NoCC_m"]) %>%
  filter(scenario2 %in% c("CC26_m")) %>%
  ungroup() %>%
  group_by(id, ssp) %>%
  mutate(mean = mean(net_val),
         net = "cc26m_noccm")

# cc26_m-CC6_m
cc26m_cc6 <- TOTAL_WLD %>%
  group_by(id, model, ssp) %>%
  mutate(net_val = diff - diff[scenario2 == "CC6"]) %>%
  filter(scenario2 %in% c("CC26_m")) %>%
  ungroup() %>%
  group_by(id, ssp) %>%
  mutate(mean = mean(net_val),
         net = "cc26m_cc6")

scen_diff <- bind_rows(cc6_nocc, noccm_nocc, cc26m_noccm, cc26m_nocc, cc26m_cc6) %>% 
  ungroup() %>%
  mutate(net = forcats::fct_relevel(net,
                              c("cc26m_noccm",  "cc6_nocc", "noccm_nocc", "cc26m_cc6", "cc26m_nocc")),
         net = forcats::fct_recode(net,
           "CC RCP 2.6" = "cc26m_noccm", 
           "CC RCP 6.0" = "cc6_nocc", 
           "Mitigation" = "noccm_nocc",
           "Residual CC + Mitigation" = "cc26m_cc6",
           "Mitigation + RCP 2.6" = "cc26m_nocc"))
         


barplot5_f <- function(df, var, itm){
  df <- filter(df, variable == var, item == itm)
  
  p = ggplot() +
    scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
    geom_bar(data = filter(df, model == "GLOBIOM"), aes(x = ssp, y = mean, fill = ssp), stat="identity", colour = "black") +
    geom_point(data = df, aes(x = ssp, y = net_val, shape = model))  + 
    labs( y = "Percentage point", x = "", fill = "", shape = "") +
    facet_wrap(~net, nrow = 1) +
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    #scale_y_continuous(expand = c(0,0)) +
    #coord_cartesian(ylim = c(y_min, y_max)) +
    theme(legend.position = "bottom") +
    theme(strip.background = element_blank()) +
    guides(fill = FALSE)
  
  p
}    

barplot5_f(scen_diff, "PROD", "AGR")
barplot5_f(scen_diff, "AREA", "CRP")
barplot5_f(scen_diff, "AREA", "LSP")
barplot5_f(scen_diff, "XPRP", "AGR")
barplot5_f(scen_diff, "XPRP", "LSP")
barplot5_f(scen_diff, "ECH4", "AGR")
barplot5_f(scen_diff, "EN2O", "AGR")
barplot5_f(scen_diff, "EMIS", "AGR")


# As barplot 5 but without mitigation + RCP 2.6
# FINAL ONES USED IN ERL PAPER!!
barplot6_f <- function(df, var, itm){
  df <- filter(df, variable == var, item == itm) %>%
    filter(net != "Residual CC + Mitigation")
  
  p = ggplot() +
    scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
    geom_bar(data = filter(df, model == "GLOBIOM"), aes(x = ssp, y = mean, fill = ssp), stat="identity", colour = "black") +
    geom_point(data = df, aes(x = ssp, y = net_val, shape = model))  + 
    labs( y = "percentage point", x = "", fill = "", shape = "") +
    facet_wrap(~net, nrow = 1) +
    theme_bw(base_size = 13) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(axis.ticks.x=element_blank()) +
    #scale_y_continuous(expand = c(0,0)) +
    #coord_cartesian(ylim = c(y_min, y_max)) +
    theme(legend.position = "bottom") +
    theme(strip.background = element_blank()) +
    guides(fill = FALSE)
  
  p
}    


barplot6_f(scen_diff, "PROD", "AGR")
barplot6_f(scen_diff, "AREA", "CRP")
barplot6_f(scen_diff, "AREA", "LSP")
barplot6_f(scen_diff, "XPRP", "AGR")
barplot6_f(scen_diff, "XPRP", "LSP")
barplot6_f(scen_diff, "ECH4", "AGR")
barplot6_f(scen_diff, "EN2O", "AGR")
barplot6_f(scen_diff, "EMIS", "AGR")



### GDP AND POP PLOTS
# gdp
gdp <- TOTAL %>%
  filter(variable %in% c("GDPT"),
         year == 2050,
         model == "MAGNET",
         scenario2 == "NoCC") 

# Plots
fig_gdp = ggplot(data = gdp, aes(x = ssp, y = index, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "Index (2010=1)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
 theme(legend.position = "bottom") +
 theme(panel.spacing = unit(0, "lines")) +
 theme(panel.border = element_blank()) +
 theme(strip.background = element_rect(fill = NA, colour = "black")) +
 theme(axis.line.y = element_line(color="black"))

fig_gdp

fig_gdp2 = ggplot(data = gdp, aes(x = ssp, y = diff, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  #coord_cartesian(ylim = c(0,12)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_gdp2

# pop
pop <- TOTAL %>%
  filter(variable %in% c("POPT"),
         year == 2050,
         model == "MAGNET",
         scenario2 == "NoCC") 

fig_pop = ggplot(data = pop, aes(x = ssp, y = index, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "Index (2010=1)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,2.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_pop

fig_pop2 = ggplot(data = pop, aes(x = ssp, y = diff, fill = ssp)) +
  scale_fill_manual(values = c("#E69F00", "#009E73", "#F0E442", "#0072B2")) +
  geom_bar(stat="identity", colour = "black") + 
  facet_grid(~region, switch = "x") +
  labs( y = "2010-2050 growth (%)", x = "", fill = "") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(-10,150)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.spacing = unit(0, "lines")) +
  theme(panel.border = element_blank()) +
  theme(strip.background = element_rect(fill = NA, colour = "black")) +
  theme(axis.line.y = element_line(color="black"))

fig_pop2