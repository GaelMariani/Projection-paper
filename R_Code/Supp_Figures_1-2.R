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

  ### ----- Bivariate data between Cexport and Fishing
  load(here::here("RData", "delta.RData"))
  
  ### ----- FishMIP data
  load(here::here("RData", "delta_tas_biomass_fishMIP.RData"))
  
### -----
  
  
### ---------------------------------------------
### Produce the figures: Comparison BOATS vs. fishMIP outputs
### ---------------------------------------------
### ----- 
  
  ### ----- Plot for SSP 1-2.6
  p_ssp126 <- yearly_evol_paper(data               = delta,
                                value_to_plot      = "delta",
                                fill_var           = "join",
                                group_var          = "join",
                                y_lab              = "% change",
                                ssp_to_select      = c("Historical", "ssp126"),
                                pathways_to_select = c("Biomass"),
                                fishing_to_select  = c("nh"),
                                envelope           = TRUE, 
                                fishmip            = delta_tas_fishmip |>  dplyr::filter(stringr::str_detect(join, pattern = "ssp585", negate = TRUE)),
                                worm               = NULL, # worm.data.list,
                                name               = NULL) 
  
    ## ---- Save the figure
    ggplot2::ggsave(here::here("Figures", "Sup-Fig1.jpeg"), width = 5, height = 5, device = "jpeg")
  
  
  ### ----- Plot for SSP 5-8.5
  p_ssp585 <- yearly_evol_paper(data               = delta,
                                value_to_plot      = "delta",
                                fill_var           = "join",
                                group_var          = "join",
                                y_lab              = "% change",
                                ssp_to_select      = c("Historical", "ssp585"),
                                pathways_to_select = c("Biomass"),
                                fishing_to_select  = c("nh"),
                                envelope           = TRUE, 
                                fishmip            = delta_tas_fishmip |>  dplyr::filter(stringr::str_detect(join, pattern = "ssp126", negate = TRUE)),
                                worm               = NULL, # worm.data.list,
                                name               = NULL) 
  
    ## ---- Save the figure
    ggplot2::ggsave(here::here("Figures", "Sup-Fig2.jpeg"), width = 5, height = 5, device = "jpeg")
  
  

  
  
### ----- 