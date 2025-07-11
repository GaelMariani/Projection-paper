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
  
  ### ----- Load BOATS b0 data
  BOATS_b0_GFDL <- readxl::read_excel(path = here::here("RData", "B0-3b_BOATS.xlsx"), sheet = 2)  
  
  ### ----- Worms' data
  worm.data.list <- raveio::read_mat(file = here::here("Rdata", "Worm.mat"), ram = TRUE)

### -----
  

### ---------------------------------------------
### Format data
### ---------------------------------------------
### ----- 
  
  ### ----- Format b0 data
  BOATS_b0_GFDL <- BOATS_b0_GFDL |> 
    dplyr::mutate(ref = B0) |>  # in carbon
    dplyr::rename(set.number = ens) |> 
    dplyr::mutate(ESM = "GFDL", pathway = "biomass", fishing = "hf") |> 
    dplyr::select(ESM, pathway, fishing, set.number, ref)  
  
  BOATS_b0 <- BOATS_b0_GFDL |> 
    dplyr::mutate(ESM = "IPSL", fishing = "hf") |> 
    dplyr::bind_rows(BOATS_b0_GFDL) |>
    tidyr::unite(join, c(ESM, fishing, pathway, set.number)) 
  
  
  ### ----- Format BOATS outputs
  delta2 = delta |> 
    dplyr::select(-ref) |> 
    dplyr::filter(pathway == "Biomass", fishing == "hf", ssp != "ssp585") |> 
    dplyr::mutate(pathway = "biomass") |> 
    tidyr::unite(join, c(ESM, fishing, pathway, set.number), sep = "_", remove = FALSE) |> 
    dplyr::left_join(BOATS_b0, by = "join") |> 
    dplyr::select(-join) |> 
    dplyr::mutate(delta = ((carbon-ref)/ref)*100) |> 
    dplyr::mutate(ssp   = forcats::fct_recode(ssp, 
                                              "Historical" = "Hist",
                                              "SSP 1-2.6"  = "ssp126"))
  
  data_envelope = delta2 |> 
    dplyr::group_by(year, ssp) |> 
    dplyr::summarise(y_mean = mean(delta, na.rm = TRUE),
                     y_min = mean(delta, na.rm = TRUE) - sd(delta, na.rm = TRUE),
                     y_max = mean(delta, na.rm = TRUE) + sd(delta, na.rm = TRUE))
  
  
  ### ----- Format Worms' outputs
  df.worm <- data.frame(year = as.vector(worm.data.list$worm.year),
                        b_b0_mean = as.vector(worm.data.list$worm.mid),
                        b_b0_min = as.vector(worm.data.list$worm.bot),
                        b_b0_max = as.vector(worm.data.list$worm.top))
  
  df.worm.delta <- df.worm |> 
    dplyr::mutate(delta_mean = (b_b0_mean - 1)*100,
                  delta_min  = (b_b0_min - 1)*100,
                  delta_max  = (b_b0_max - 1)*100,
                  join2      = dplyr::case_when(year <= 2015 ~ "Historical",
                                                TRUE ~ "SSP 1-2.6"),
                  fishing    = "Fishing",
                  pathway    = "Biomass") |> 
    dplyr::select(year, join2, pathway, fishing, delta_mean, delta_min, delta_max)
  

### -----

### ---------------------------------------------
### Produce the figure: Comparison BOATS vs. Worms  outputs
### ---------------------------------------------
### ----- 
  
ggplot2::ggplot(delta, ggplot2::aes(x = year, y = delta, color = ssp), show.legend = FALSE) +
    ggplot2::geom_point(alpha = 0, show.legend = FALSE) + # 0.025 to make points more transparent
    ggplot2::geom_hline(yintercept = 0, color = "grey30", linetype = "dotdash", size = 0.7) +
    ggplot2::scale_color_manual(values = c("Historical" = "grey50", "SSP 1-2.6" = "dodgerblue2"),
                       name   = NULL) +
    ggplot2::labs(y = "Delta", x = NULL) +
    ggplot2::scale_x_continuous(breaks = seq(1950, 2100, 20)) +
    ggplot2::theme_bw() +
    
    ggplot2::geom_ribbon(data    = data_envelope,
                         mapping = ggplot2::aes(x     = year,
                                                y     = y_mean,
                                                ymin  = y_min,
                                                ymax  = y_max,
                                                color = ssp,
                                                fill  = ssp,
                                                group = ssp),
                         size     = 0.25,
                         alpha    = 0.1,
                         show.legend = TRUE) +
    
    ggplot2::geom_line(data    = data_envelope,
                       mapping = ggplot2::aes(x = year, y = y_mean, color = ssp, group = ssp),
                       size = 0.8, show.legend = TRUE) +
    
    ggplot2::scale_color_manual(values = c("Historical" = "grey50", "SSP 1-2.6" = "dodgerblue2"),
                                name   = NULL) +
    
    ggplot2::scale_fill_manual(values = c("Historical" = "grey50", "SSP 1-2.6" = "dodgerblue2"),
                               name   = NULL) +
    
    ggplot2::scale_x_continuous(breaks = seq(1950, 2100, 15)) +
    
    ggplot2::theme(axis.text.x     = element_text(angle = 45, hjust = 0.9, size = 11),
                   axis.text.y     = element_text(size = 11),
                   axis.title.y    = element_text(size = 15),
                   legend.text     = element_text(size = 12),
                   legend.title    = element_text(size = 13),
                   legend.position = "bottom",
                   strip.text.y = element_blank(),
                   strip.text      = element_text(size = 15),
                   strip.background = element_blank()) +
    
    ggplot2::geom_line(data     = df.worm.delta,
                       mapping  = ggplot2::aes(x = year, y = delta_mean),
                       color    = "black", 
                       # linetype = "dashed",
                       size     = 1, 
                       show.legend = FALSE) +
    
    ggplot2::geom_line(data     = df.worm.delta,
                       mapping  = ggplot2::aes(x = year, y = delta_min),
                       color    = "black", 
                       linetype = "dashed",
                       size     = 1, 
                       show.legend = FALSE) +
    
    ggplot2::geom_line(data     = df.worm.delta,
                       mapping  = ggplot2::aes(x = year, y = delta_max),
                       color    = "black", 
                       linetype = "dashed",
                       size     = 1, 
                       show.legend = FALSE)
  
  
ggplot2::ggsave(here::here("Figures", "Supp-Fig3.jpeg"), width = 5.5, height = 5.5, device = "jpeg")
  
### ----- 
