#####################################################################
##########                                                 ##########
##########  Produce Supp Figure 4 to 8 on Mortality Rates  ##########
##########                                                 ##########
#####################################################################
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

  ### ----- Proportion of the biomass dead from Non-predation mortality over Total Mortality
  assign("PropM", get(load(here::here("RData", "SeneOverTot_assemble_Hist_1990.RData")))) ; rm(hist)
  
  ### ----- Non-predation mortality rates
  assign("Mrate", get(load(here::here("RData", "SenMortRate_assemble_Hist_1990.RData")))) ; rm (hist)

### -----

  
### ---------------------------------------------
### Format RData
### ---------------------------------------------
### ----- 
  
  ### ----- Mean over the global ocean for each group (n = 3) and their respective size class
  PropM_mean <- apply(PropM, 3:4, mean, na.rm = TRUE)
  Mrate_mean <- apply(Mrate, 3:4, mean, na.rm = TRUE)
  
  ### ----- Turn both arrays into a dataframe
  
    ## ---- For the proportion
    PropM_mean_df <- as.data.frame(PropM_mean) |> 
      dplyr::mutate(group_number = dplyr::row_number()) |> 
      tidyr::pivot_longer(cols = -group_number, names_to = "age", values_to = "Prop") |> 
      dplyr::mutate(age = as.integer(stringr::str_remove(age, "V")),
                    group_number = as.factor(group_number)) |> 
      dplyr::filter(!is.nan(Prop))
    
    ## ---- For mortality rates
    Mrate_mean_df <- as.data.frame(Mrate_mean) |> 
      dplyr::mutate(group_number = dplyr::row_number()) |> 
      tidyr::pivot_longer(cols = -group_number, names_to = "age", values_to = "Mrate") |> 
      dplyr::mutate(age = as.integer(stringr::str_remove(age, "V")),
                    group_number = as.factor(group_number)) |> 
      dplyr::filter((group_number == 1 & age < 20) | (group_number == 2 & age < 38) | (group_number == 3))
    
  
### -----
  

### ---------------------------------------------
### Plot RData
### ---------------------------------------------
### ----- 
  
  ### ----- Maps
    
    ## ---- Group 1
    map1 <- list()
    for(i in 1:19){map1[[i]] <- map_raster(data = PropM, grp = 1, sc = i, midpoint = 1)}
    plot1 <- do.call(gridExtra::arrangeGrob, c(map1, ncol = 4, nrow = 5))
    ggsave(plot1, filename = here::here("Figures/", "Supp-Fig4.jpeg"), device = "jpeg", width = 21, height = 14)
    
    ## ---- Group 2
    map2 <- list()
    for(i in 1:37){map2[[i]] <- map_raster(data = PropM, grp = 2, sc = i, midpoint = 1)}
    plot2 <- do.call(gridExtra::arrangeGrob, c(map2, ncol = 4, nrow = 10))
    ggsave(plot2, filename = here::here("Figures/", "Supp-Fig5.jpeg"), device = "jpeg", width = 21, height = 28)
    
    ## ---- Group 3
    map3 <- list()
    for(i in 1:50){map3[[i]] <- map_raster(data = PropM, grp = 3, sc = i, midpoint = 1)}
    plot3 <- do.call(gridExtra::arrangeGrob, c(map3, ncol = 4, nrow = 13))
    ggsave(plot3, filename = here::here("Figures/", "Supp-Fig6.jpeg"), device = "jpeg", width = 21, height = 36.4)
    
  
  
  ### ----- Age-mortality curves
    
    ## ---- Plot proportions
    ggplot(data = PropM_mean_df, mapping = aes(x = age, y = Prop, color = group_number)) +
      geom_line() +
      scale_color_manual(name   = "Size group",
                         values = c("1" = "#098518", "2" = "#cf4d44", "3" = "#426fe3"),
                         labels = c("1" = "Small" ,
                                    "2" = "Medium",
                                    "3" = "Large")) +
      labs(x = "Size class", y = "Non-predation natural mortality/Total mortality") +
      theme_bw()
    
    ggsave(here::here("Figures", "Supp-Fig7.jpeg"), width = 5.5, height = 4, device = "jpeg", dpi = 1000)
    
    ## ---- Plot mortality rates 
    ggplot(data = Mrate_mean_df, mapping = aes(x = age, y = Mrate, color = group_number)) +
      geom_line() +
      scale_color_manual(name   = "Size group",
                         values = c("1" = "#098518", "2" = "#cf4d44", "3" = "#426fe3"),
                         labels = c("1" = "Small" ,
                                    "2" = "Medium",
                                    "3" = "Large")) +
      labs(x = "Size class", y = "Non-predation natural\nmortality rate (per month)") +
      theme_bw()
    
    ggsave(here::here("Figures", "Supp-Fig8.jpeg"), width = 5.5, height = 4, device = "jpeg", dpi = 1000)
    

### -----