#################################################
##########                             ##########
##########  Produce Supp Figure 1 + 2  ##########
##########                             ##########
#################################################
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

  ### ----- Change in SST under SSP 5-8.5 in the 2090'
  load(here::here("RData", "SST_ssp585_2090.RData"))
  
  ### ----- Change in NPP under SSP 5-8.5 in the 2090'
  load(here::here("RData", "NPP_ssp585_2090.RData"))
  
  ### ----- NPP change in 2090 for IPSL and GFDL
  load(here::here("RData", "NPP_ssp585_IPSL_GFDL.RData"))

### -----
  

### ---------------------------------------------
### Produce the figures: SST and NPP outputs from ESM
### ---------------------------------------------
### ----- 
  
  ### ----- Maps of changes in NPP and SST
  
    ## ---- SST
    min.SST <- range(temp85_90$data$layer,na.rm = T)[1]
    max.SST <- range(temp85_90$data$layer,na.rm = T)[2]
    p1 <- univariate_map(data_map    = temp85_90,
                         values      = c(min.SST, 0, max.SST),
                         color_scale = rev(c("darkred", "white", "darkblue")),
                         legend      = "Change \n (°C)",
                         delta       = TRUE,
                         log_trans   = FALSE,
                         show.legend = TRUE,
                         overlap     = FALSE,
                         name        = NULL) ; p1
    
    ## ---- NPP
    npp85_90$data$layer[npp85_90$data$layer > 100] <- 100
    min.NPP <- range(npp85_90$data$layer,na.rm = T)[1]
    max.NPP <- range(npp85_90$data$layer,na.rm = T)[2]
    p2 <- univariate_map(data_map    = npp85_90,
                         values      = c(min.NPP, 0, max.NPP),
                         color_scale = c("darkred", "white", "darkblue"),
                         legend      = "Change \n (%)",
                         delta       = TRUE,
                         log_trans   = FALSE,
                         show.legend = TRUE,
                         overlap     = FALSE,
                         name        = NULL) ; p2
    
    ## ---- Combine maps
    fig <- cowplot::ggdraw() +
      
      cowplot::draw_plot(p1, x = 0.00, y = 0.0, width = 0.50, height = 1) +
      cowplot::draw_plot(p2, x = 0.50, y = 0.0, width = 0.50, height = 1) +
      cowplot::draw_plot_label(label = c("(a)", "(b)"),
                               size = 14,
                               x = c(0, 0.5),
                               y = c(0.70, 0.70)) ; fig
    
    ggplot2::ggsave(here::here("Figures", "Sup-Fig9.jpeg"), width = 12, height = 6, device = "jpeg", dpi = 1000)
    
    
  ### ----- Comparison NPP between GFDL and IPSL Earth System Models
    
    ## ---- Maps
    maps_npp <- lapply(npp_all_map, function(x) univariate_map(data_map    = x,
                                                               values      = c(-100,0,100),
                                                               color_scale = c("darkred", "white", "darkblue"),
                                                               legend      = "Change \n (%)",
                                                               delta       = TRUE,
                                                               log_trans   = FALSE,
                                                               show.legend = TRUE,
                                                               overlap     = FALSE,
                                                               name        = NULL)) 
    
    ## ---- Combine maps
    fig <- cowplot::ggdraw() +
      
      cowplot::draw_plot(maps_npp[[1]] + ggtitle("GFDL - SSP 8-5.8"), x = 0.00, y = 0.0, width = 0.50, height = 1) +
      cowplot::draw_plot(maps_npp[[2]] + ggtitle("IPSL - SSP 8-5.8"), x = 0.50, y = 0.0, width = 0.50, height = 1) +
      cowplot::draw_plot_label(label = c("(a)", "(b)"),
                               size = 14,
                               x = c(0, 0.5),
                               y = c(0.70, 0.70)) ; fig
    
    ggplot2::ggsave(here::here("Figures", "Sup-Fig10.jpeg"), width = 12, height = 6, device = "jpeg", dpi = 1000)
    
### -----  
    