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

  ### ----- Steady states simulatios
  load(here::here("RData", "data.seq.ss.RData"))
  
  ### ----- Transient simulations
  load(here::here("RData", "data.seq.trans.RData"))
  

### -----

  
### ---------------------------------------------
### Produce the figures: SST and NPP outputs from ESM
### ---------------------------------------------
### ----- 

  ### ----- Plot data
  yearly_seq_plot(data.ss    = data.seq.ss, 
                  data.trans = data.seq.trans, 
                  delta      = FALSE,
                  ylab_trans = "Sequestration (GtC)",
                  type       = "both", 
                  name       = "Supp-Fig11")


### -----
  