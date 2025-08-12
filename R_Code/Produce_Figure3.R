########################################
##########                    ##########
##########  Produce Figure 3  ##########
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

  ### ----- Load tas data
  load(here::here("RData", "tas_data.RData"))
  
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

  ### ----- Plots of climate change impacts (Panels A & B)
  p1 <- output_vs_AtmTemp(data.carbon = delta, 
                          data.tas    = tas.data, 
                          fishmip     = delta_tas_fishmip, 
                          name        = "data_plot_tas")
  
  ### ----- Plots of fishing impacts (Panels C & D)
  p2 <- deltaB_vs_deltaFlux(data.carbon = delta,
                            name        = "data_plot_fishing")

  ### ----- Plot icons
  biom  <- ggplot() + annotation_custom(biomass_icon) + theme_void()
  Cflux <- ggplot() + annotation_custom(Cflux_icon) + theme_void()
  fp    <- ggplot() + annotation_custom(FP_icon) + theme_void()
  carc  <- ggplot() + annotation_custom(carcass_icon) + theme_void()
  
  ### ----- Arrange the figure
  figure3 <- cowplot::ggdraw() +
    cowplot::draw_plot(p1$plot_biomass, x = 0.00, y = 0.50, width = 0.43, height = 0.5) + 
    cowplot::draw_plot(p1$plot.flux,    x = 0.43, y = 0.50, width = 0.57, height = 0.5) + 
    cowplot::draw_plot(p2$plot.fp,      x = 0.00, y = 0.00, width = 0.43, height = 0.5) + 
    cowplot::draw_plot(p2$plot.car,     x = 0.43, y = 0.00, width = 0.57, height = 0.5) + 
    cowplot::draw_plot(biom,  x = 0.07, y = 0.565,  width = 0.09, height = 0.09) +
    cowplot::draw_plot(Cflux, x = 0.50, y = 0.575,  width = 0.08,  height = 0.08) +
    cowplot::draw_plot(fp,    x = 0.06, y = 0.085,  width = 0.08, height = 0.08) +
    cowplot::draw_plot(carc,  x = 0.50, y = 0.090,  width = 0.08,   height = 0.08) +
    cowplot::draw_plot_label(label = c("a", "b", "c", "d"), # "(A)", "(B)", "(C)", "(D)"
                             size = 18,
                             x = c(0, 0.43, 0, 0.43),
                             y = c(1, 1, 0.5, 0.5))
  
  ### ----- Save figure 2
  ggplot2::ggsave(here::here("Figures", "Figure3-v2.jpeg"), width = 11, height = 8, device = "jpeg", dpi = 600)

### -----  
  