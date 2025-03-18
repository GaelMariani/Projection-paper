########################################
##########                    ##########
##########  Produce Figure 4  ##########
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

  ### ----- Load bivariate data
  load(here::here("RData", "data_bivar_Cseq_biom.RData"))
  
  ### ----- Load KMCA data
  load(here::here("RData", "data_quantile_0.75.RData"))

### ----- 


### ---------------------------------------------
### Plot Data
### ---------------------------------------------
### ----- 
  
  ### ----- Create the bivariate color scale
  bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                               upperleft   = rgb(130,0,80, maxColorValue = 255), 
                                               upperright  = rgb(0,150,235, maxColorValue = 255),
                                               bottomleft  = "white",
                                               bottomright = rgb(255,230,15, maxColorValue = 255), 
                                               ylab        = "Cflux",
                                               xlab        = "GFW")
  
  ### ----- Produce the bivariates maps
  map_biom_seqO <- lapply(data_bivar_Cseq_biom, 
                          function(x) bivariate_map(data_map              = x, 
                                                    bivariate_color_scale = bivariate_color_scale,
                                                    ylab                  = "Sequestration", 
                                                    xlab                  = "Biomass",
                                                    name                  = NULL)) 
  
  ### ----- Produce the quantiles maps
  map_q75 <- lapply(data_quantile_0.75, 
                    function(x) univariate_map(data_map    = x,
                                               color_scale = rgb(0,150,235, maxColorValue = 255), # scales::show_col(rgb(255,230,15, maxColorValue = 255))
                                               legend      = "",
                                               delta       = FALSE,
                                               show.legend = FALSE,
                                               overlap     = FALSE,
                                               name        = NULL))
  
  ### ----- Arrange the figure
  figure5 <- cowplot::ggdraw() +
    
    cowplot::draw_plot(map_biom_seqO[[1]], x = 0.00, y = 0.38, width = 0.50, height = 0.63) +
    cowplot::draw_plot(map_biom_seqO[[2]], x = 0.50, y = 0.38, width = 0.50, height = 0.63) +
    cowplot::draw_plot(map_q75[[1]],       x = 0.00, y = 0.0, width = 0.50, height = 0.4) +
    cowplot::draw_plot(map_q75[[2]],       x = 0.50, y = 0.0, width = 0.50, height = 0.4) +
    cowplot::draw_text(text     = c("1950'", "2010'"),
                       fontface = rep("bold", 2),
                       color    = rep("black", 2),
                       x        = c(0.25, 0.75), 
                       y        = c(0.98, 0.98), 
                       size     = 18) +
    cowplot::draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                             size = 14,
                             x = c(0, 0.5, 0, 0.5),
                             y = c(0.99, 0.99, 0.39, 0.39))
  
  ### ----- Save the figure
  ggplot2::ggsave(here::here("Figures", "Figure5.jpeg"), width = 12, height = 8, device = "jpeg", dpi = 1000)
  

### -----