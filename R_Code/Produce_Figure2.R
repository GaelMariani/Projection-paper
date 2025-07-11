########################################
##########                    ##########
##########  Produce Figure 2  ##########
##########                    ##########
########################################
rm(list = ls(), envir = .GlobalEnv)
library(ggplot2)


### ---------------------------------------------
### Load R functions
### ---------------------------------------------
### ----- 
source(here::here("R_Code", "Rfunctions.R"))
### ----- 


### ---------------------------------------------
### Load RData
### ---------------------------------------------
### ----- 

  ### ----- Bivariate data between Cexport and Fishing
  load(here::here("RData", "delta.RData"))

  ### ----- FishMIP data
  load(here::here("RData", "delta_tas_biomass_fishMIP.RData"))

  ### ----- Load icons
  biomass_icon <- png::readPNG(here::here("Icons", "biomass_icon.png")) ;  biomass_icon <- grid::rasterGrob(biomass_icon, interpolate = TRUE)
  carcass_icon <- png::readPNG(here::here("Icons", "carcass_icon.png")) ;  carcass_icon <- grid::rasterGrob(carcass_icon, interpolate = TRUE)
  FP_icon      <- png::readPNG(here::here("Icons", "fecal_pellets_icon.png")) ; FP_icon <- grid::rasterGrob(FP_icon, interpolate = TRUE)
  Cflux_icon   <- png::readPNG(here::here("Icons", "Cexport_icon.png")) ; Cflux_icon <- grid::rasterGrob(Cflux_icon, interpolate = TRUE)

### -----
  

### ---------------------------------------------
### Produce the panels and the figure
### ---------------------------------------------
### ----- 
  
  ### ----- Main plot
  p <- yearly_evol_paper(data               = delta,
                         value_to_plot      = "delta",
                         fill_var           = "join",
                         group_var          = "join",
                         y_lab              = "% change",
                         ssp_to_select      = c("Historical", "ssp126", "ssp585"),
                         pathways_to_select = c("Biomass", "Carcass", "Fecal Pellets"),
                         fishing_to_select  = c("nh", "hf"),
                         envelope           = TRUE, 
                         fishmip            = NULL,
                         worm               = NULL, # worm.data.list,
                         name               = NULL) 
  
  ### ----- Plot icons
  biom  <- ggplot() + annotation_custom(biomass_icon) + theme_void()
  carc  <- ggplot() + annotation_custom(carcass_icon) + theme_void()
  fp    <- ggplot() + annotation_custom(FP_icon) + theme_void()
  
  ### ----- Arrange the figure
  fig <- cowplot::ggdraw() +
    cowplot::draw_plot(p, x = 0.00, y = 0.00, width = 1, height = 1) + 
    cowplot::draw_plot(biom, x = 0.16,  y = 0.67, width = 0.1,  height = 0.1) +
    cowplot::draw_plot(biom, x = 0.58,  y = 0.67, width = 0.1,  height = 0.1) +
    cowplot::draw_plot(fp,   x = 0.16,  y = 0.44, width = 0.06, height = 0.06) +
    cowplot::draw_plot(fp,   x = 0.59, y = 0.44, width = 0.06, height = 0.06) +
    cowplot::draw_plot(carc, x = 0.16, y = 0.18, width = 0.1,  height = 0.1) +
    cowplot::draw_plot(carc, x = 0.58,  y = 0.18, width = 0.1,  height = 0.1) +
    cowplot::draw_plot_label(label = c("a", "b", "c", "d", "e", "f"),
                             size = 11,
                             x = c(rep(c(0.14, 0.57), 3)),
                             y = c(rep(0.93, 2), rep(0.68, 2), rep(0.43, 2)))
  
  ### ----- Save the figure 
  ggplot2::ggsave(here::here("Figures", "Figure2-v2.jpeg"), width = 4.5, height = 5, device = "jpeg")
  
### -----
