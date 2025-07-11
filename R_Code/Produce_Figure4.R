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

  ### ----- Load data for absolute values
  load(here::here("RData", "data_map_abs_ls.RData"))

  ### ----- Load delta values
  load(here::here("RData", "data_map_delta_ls.RData"))

### ----- 


### ---------------------------------------------
### Plot Data
### ---------------------------------------------
### ----- 
  
  ### ----- Maps of absolute values (Panels A to D)
  leg <- c(rep(expression(atop("(tC."~km^-2*")")), 2), rep(expression(atop("(tC."~km^-2*"."~yr^-1*")")), 2))
  map.ls.vals <- lapply(1:length(data.map.abs.ls), 
                        function(i) univariate_map(data_map    = data.map.abs.ls[[i]],
                                                   color_scale = viridis::viridis(6, direction = 1),
                                                   legend      = leg[i], 
                                                   delta       = FALSE,
                                                   show.legend = TRUE,
                                                   overlap     = FALSE,
                                                   name        = NULL))
  
  ### ----- Maps of deltas (Panels E & F)
  title = c("Biomass", "Flux")
  map.ls.delta <- lapply(data.map.delta.ls, 
                         function(x) univariate_map(data_map    = x,
                                                    values      = c(-100,0, 100),
                                                    color_scale = c("darkred", "white", "darkblue"),
                                                    legend      = "Change (%)",
                                                    delta       = TRUE,
                                                    show.legend = TRUE,
                                                    overlap     = FALSE, 
                                                    name        = NULL))
  
  ### ----- Arrange the figure
  figure4 <- cowplot::ggdraw() + 
    cowplot::draw_plot(map.ls.vals[[1]]  + ggtitle("Biomass (1950s)")  + theme(plot.title = element_text(size = 17, face = "bold")),  x = 0.00, y = 0.66, width = 0.52, height = 0.4) +
    cowplot::draw_plot(map.ls.vals[[3]]  + ggtitle("Fluxes (1950s)")     + theme(plot.title = element_text(size = 17, face = "bold")),  x = 0.50, y = 0.66, width = 0.52, height = 0.4) +  
    cowplot::draw_plot(map.ls.vals[[2]]  + ggtitle("Biomass (2090s)")  + theme(plot.title = element_text(size = 17, face = "bold")),  x = 0.00, y = 0.33, width = 0.52, height = 0.4) +
    cowplot::draw_plot(map.ls.vals[[4]]  + ggtitle("Fluxes (2090s)")     + theme(plot.title = element_text(size = 17, face = "bold")),  x = 0.50, y = 0.33, width = 0.52, height = 0.4) +  
    cowplot::draw_plot(map.ls.delta[[1]] + ggtitle(expression(bold(Delta*Biomass))) + theme(plot.title = element_text(size = 17, face = "bold")), x = 0.00, y = 0.00, width = 0.50, height = 0.4) +
    cowplot::draw_plot(map.ls.delta[[2]] + ggtitle(expression(bold(Delta*Fluxes)))    + theme(plot.title = element_text(size = 17, face = "bold")), x = 0.50, y = 0.00, width = 0.50, height = 0.4) +  
    cowplot::draw_plot_label(label = c("(A)", "(D)", "(B)", "(E)", "(C)", "(F)"),
                             size  = 16,
                             x     = c(0, 0.5, 0, 0.5, 0, 0.5),
                             y     = c(0.99, 0.99, 0.66, 0.66, 0.33, 0.33)) 
  
  
  ### ----- Save the figure
  ggplot2::ggsave(here::here("Figures", "Figure4-cap.jpeg"), width = 12, height = 10, device = "jpeg", dpi = 1000)
  

### ----- 

