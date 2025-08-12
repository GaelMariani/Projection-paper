#########################################
##########                     ##########
##########   Estimate slopes   ##########
##########                     ##########
#########################################
rm(list = ls(), envir = .GlobalEnv)


### ---------------------------------------------
### Load RData
### ---------------------------------------------
### ----- 

  ### ----- Load data for panels A & B
  p1 <- get(load(here::here("RData", "data_plot_tas.RData"))) ; rm(outputs)
  
  ### ----- Load data for panels C & D
  p2 <- get(load(here::here("RData", "data_plot_fishing.RData"))) ; rm(outputs)

### ----- 

  
### ---------------------------------------------
### Test if the slope is significantly different
### ---------------------------------------------
### ----- 
  
  ## ---- For biomass and fluxes vs. change in atmospherical temperatures
  
    # --- Extract the data
    slope_temp_data <- rbind(p1$data_line_biomass, p1$data_line_flux)
    
    # --- ANCOVA to get the coef
    mod_temp <- lm(formula = delta_mean ~ delta.tas*pathway2, data = slope_temp_data)
    anova(mod_temp)
    mod_temp$coefficients
    
    # --- Check model's assumptions
    performance::check_model(mod_temp)
    
    # --- Get 95% confidence intervalle of each slope
    slope_temp <- emmeans::lstrends(mod_temp, "pathway2", var = "delta.tas")
    slope_temp
    
    # --- Test if the slope are significantly differents
    pairs(slope_temp)
    
    
  ## ---- For carcass and FP vs. change in biomass
  
    # --- Extract the data
    slope_biom_data <- rbind(p2$data_line_fp, p2$data_line_car)
    
    # --- ANCOVA to get the coef
    mod_biom <- lm(formula = delta_mean ~ biomass*flux_type, data = slope_biom_data)
    anova(mod_biom)
    mod_biom$coefficients
    
    
    # --- Check model's assumptions
    performance::check_model(mod_biom)
    
    # --- Get 95% confidence intervalle of each slope
    slope_biom <- emmeans::lstrends(mod_biom, "flux_type", var = "biomass")
    slope_biom
    
    # --- Test if the slope are significantly differents
    pairs(slope_biom)

### -----
