#' Yearly Evolution Of Carbon Fluxes To Explore
#'
#' @param data the merged data of historical and projection outputs
#' @param value_to_plot the y variable to plot
#' @param fill_var the filling variable, either fishing or ssp
#' @param y_lab the y axis legend, either "Wet mass (Gt."~yr^-1*")" or "Carbon (GtC."~yr^-1*")"
#' @param group_var the grouping variable
#' @param ssp_to_select ssp to be selected
#' @param pathways_to_select pathway to be selected
#' @param fishing_to_select fishing scenarios to be selected
#' @param name the name of the plot to be saved
#' @param width 
#' @param height 
#' @param envelope 
#'
#' @return
#' @export
#'
#' @examples
yearly_evol_paper <- function(data, value_to_plot, fill_var, y_lab, group_var, ssp_to_select, pathways_to_select, fishing_to_select, fishmip, worm, envelope = TRUE, name = NULL, width = 8, height = 3.5){
  
  ### ---- Format data
  data_format <- data |>
    dplyr::filter(ssp %in% ssp_to_select & pathway %in% pathways_to_select & fishing %in% fishing_to_select) |> 
    tidyr::unite(join2, c(ssp, fishing), sep = "_", remove = FALSE) |>  
    dplyr::mutate(carbon = carbon * 0.1 * 1e-15) |> 
    dplyr::filter(join2 %in% c("ssp126_nh", "ssp585_hf", "Historical_nh", "Historical_hf", "ssp126_hf", "ssp585_nh")) |>
    dplyr::select(year, all_of(value_to_plot), join2, pathway, fishing) |> 
    dplyr::mutate(join2   = forcats::fct_relevel(join2, "Historical_nh", "ssp126_nh", "ssp585_nh", "Historical_hf", "ssp126_hf", "ssp585_hf"),
                  join2   = forcats::fct_recode(join2, "Historical" = "Historical_nh",
                                                "SSP 1-2.6"  = "ssp126_nh",
                                                "SSP 5-8.5"  = "ssp585_nh", 
                                                "Historical" = "Historical_hf",
                                                "SSP 1-2.6"  = "ssp126_hf",
                                                "SSP 5-8.5"  = "ssp585_hf"),
                  fishing = forcats::fct_relevel(fishing, "nh", "hf"),
                  fishing = forcats::fct_recode(fishing, "No fishing" = "nh", "Fishing" = "hf"))
  
  
  colnames(data_format) <- c("x", "y", "fill", "facet", "fishing")
  
  
  ### ----- Plot data
  
  ## ---- Add points (one value for each set of parameter and esm)
  plot <- ggplot(data_format, aes(x = x, y = y, color = fill, group = fill), show.legend = FALSE) +
    geom_point(alpha = 0, show.legend = FALSE) + # 0.025 to make points more transparent
    geom_hline(yintercept = 0, color = "grey30", linetype = "dotdash", size = 0.7) +
    scale_color_manual(values = c("Historical" = "grey50", "SSP 1-2.6" = "dodgerblue2", "SSP 5-8.5" = "firebrick2"),
                       name   = NULL) +
    labs(y = y_lab, x = NULL) +
    scale_x_continuous(breaks = seq(1950, 2100, 20)) +
    facet_grid(facet ~ fishing) +
    theme_bw()
  
  ## ---- Add envelope
  if(envelope == TRUE){
    
    data_envelope = data_format |> 
      dplyr::group_by(x, fill, facet, fishing) |> 
      dplyr::summarise(y_mean = mean(y, na.rm = TRUE),
                       y_min = mean(y, na.rm = TRUE) - sd(y, na.rm = TRUE),
                       y_max = mean(y, na.rm = TRUE) + sd(y, na.rm = TRUE))
    
    plot <- plot +
      
      ggplot2::geom_ribbon(data    = data_envelope,
                           mapping = ggplot2::aes(x     = x,
                                                  y     = y_mean,
                                                  ymin  = y_min,
                                                  ymax  = y_max,
                                                  fill  = fill,
                                                  group = fill),
                           size     = 0.25,
                           alpha    = 0.1,
                           show.legend = TRUE) +
      
      ggplot2::geom_line(data    = data_envelope,
                         mapping = ggplot2::aes(x = x, y = y_mean, color = fill, group = fill),
                         size = 0.8, show.legend = TRUE) +
      
      ggplot2::facet_grid(facet ~ fishing) +
      
      ggplot2::scale_fill_manual(values = c("Historical" = "grey50", "SSP 1-2.6" = "dodgerblue2", "SSP 5-8.5" = "firebrick2"),
                                 name   = NULL) +
      
      ggplot2::scale_x_continuous(breaks = seq(1950, 2100, 15)) +
      
      ggplot2::theme(axis.text.x     = element_text(angle = 45, hjust = 0.9, size = 8),
                     axis.text.y     = element_text(size = 11),
                     axis.title.y    = element_text(size = 15),
                     legend.text     = element_text(size = 12),
                     legend.title    = element_text(size = 13),
                     legend.position = "bottom",
                     strip.text.y = element_blank(),
                     strip.text      = element_text(size = 15),
                     strip.background = element_blank()) +
      
      guides(colour = guide_legend(ncol = 3))
    
  }
  
  if(is.null(fishmip) == FALSE){
    
    fishmip.data <- fishmip |> 
      dplyr::select(year, ssp, pathway, fishing, delta_mean, delta_min, delta_max) |> 
      dplyr::rename(delta = delta_mean, join2 = ssp) |> 
      dplyr::mutate(join2   = forcats::fct_relevel(join2, "Historical", "ssp126", "ssp585"),
                    join2   = forcats::fct_recode(join2, 
                                                  "Historical" = "Historical",
                                                  "SSP 1-2.6"  = "ssp126",
                                                  "SSP 5-8.5"  = "ssp585"),
                    fishing = forcats::fct_relevel(fishing, "nh", "hf"),
                    fishing = forcats::fct_recode(fishing, "No fishing" = "nh", "Fishing" = "hf"))
    
    colnames(fishmip.data) <- c("x", "fill", "facet", "fishing", "y_mean", "y_min", "y_max")
    
    if(is.null(worm) == FALSE){
      
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
      
      colnames(df.worm.delta) <- c("x", "fill", "facet", "fishing", "y_mean", "y_min", "y_max")
      
      fishmip.data <- rbind(fishmip.data, df.worm.delta)  
      
    }
    
    # Add fishmip outputs to the plot
    plot <- plot + 
      
      ggplot2::scale_fill_manual(values = c("Historical" = "#985ba6", "SSP 1-2.6" = "#985ba6", "SSP 5-8.5" = "#985ba6"),
                                 name   = NULL) +
      
      ggplot2::scale_color_manual(values = c("Historical" = "#985ba6", "SSP 1-2.6" = "#985ba6", "SSP 5-8.5" = "#985ba6"),
                                 name   = NULL) +
      
      ggplot2::geom_line(data     = fishmip.data,
                         mapping  = ggplot2::aes(x = x, y = y_mean, color = fill, group = fill),
                         size     = 0.8,
                         color    = "#5c9666",
                         show.legend = FALSE) +
      
      ggplot2::geom_ribbon(data    = fishmip.data,
                           mapping = ggplot2::aes(x     = x,
                                                  y     = y_mean,
                                                  ymin  = y_min,
                                                  ymax  = y_max,
                                                  fill  = fill,
                                                  group = fill),
                           size     = 0.25,
                           color    = "#5c9666",
                           fill     = "#5c9666",
                           alpha    = 0.1,
                           show.legend = TRUE) +
      
      ggplot2::guides(color = FALSE, fill = FALSE) +
      
      ## ---- Add labels
      ggplot2::geom_label(label = "BOATS", x = 1980, y = -13, label.padding = unit(0.3, "lines"), label.size = 0.35, # x = 1.7, y = -27.5
                          color = "#985ba6") +
      
      ggplot2::geom_label(label = "FishMIP ensemble", x = 1980, y = -10, label.padding = unit(0.3, "lines"), label.size = 0.35, # x = 1.7, y = -27.5
                          color = "#5c9666") 

  }
  
  if(! is.null(name)) {
    
    save(plot, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = width, height = height, device = "png")
    
  }
  
  return(plot)
  
}



#' Plot Carbon Versus Atmospheric Temperatures For The Paper V2
#'
#' @param data.carbon 
#' @param data.tas 
#' @param fishmip 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
output_vs_AtmTemp <- function(data.carbon, data.tas, fishmip, name = NULL){
  
  ### ----- Format data
  
  ## ---- Data for carbon storage and carbon flux separated
  data.carb <- data.carbon |> 
    dplyr::filter(pathway %in% c("Carcass", "Fecal Pellets", "Biomass") & fishing == "nh") |> 
    dplyr::mutate(pathway2 = dplyr::case_when(pathway == "Biomass" ~ "Biomass",
                                              TRUE ~ "Carbon_Flux")) |> 
    dplyr::group_by(year, pathway2, set.number, ssp, ESM) |> 
    dplyr::summarise(carbon = sum(carbon, na.rm = T),
                     ref    = sum(ref, na.rm = T)) |> 
    dplyr::mutate(delta = ((carbon - ref)/ref)*100) |> 
    dplyr::group_by(year, set.number, ssp, pathway2) |>
    dplyr::summarise(delta = mean(delta)) |>
    tidyr::unite(join, c(year, ssp), sep = "_", remove = FALSE) 
  
  ## ---- Data FISHMIP
  fishmip2 = fishmip |> 
    dplyr::mutate(set.number = "xxx",
                  pathway2   = as.factor("Biomass (Fishmip)")) |> 
    dplyr::select(join, year, set.number, ssp, pathway2, delta_mean, delta.tas) |> 
    dplyr::rename(delta = delta_mean) 
  
  ## ---- Data tas formating
  data.tas <- data.tas |> 
    dplyr::group_by(yrs, ssp) |>
    dplyr::summarise(delta = mean(delta)) |> 
    tidyr::unite(join, c(yrs, ssp), sep = "_", remove = TRUE) |> 
    dplyr::rename("delta.tas" = "delta")
  
  data.tas$join[data.tas$join %in% c("2015_ssp126", "2015_ssp585")] <- "2015_Historical"
  
  ## --- Merge data
  data_to_plot <- data.carb |> 
    dplyr::left_join(data.tas, by = "join", relationship = "many-to-many") |> 
    dplyr::mutate(pathway2 = forcats::fct_relevel(pathway2, "Carbon_Flux", "Biomass"))
  
  data_to_plot <- rbind(data_to_plot, fishmip2) |>  
    dplyr::mutate(pathway2 = forcats::fct_relevel(pathway2, "Carbon_Flux", "Biomass (Fishmip)", "Biomass"))
  
  data_line = data.carb |>
    dplyr::group_by(year, ssp, pathway2) |>
    dplyr::summarise(delta_mean = mean(delta),
                     delta_sd   = sd(delta),
                     delta_min  = delta_mean - delta_sd,
                     delta_max  = delta_mean + delta_sd) |>
    tidyr::unite(join, c(year, ssp), sep = "_", remove = FALSE) |>  
    dplyr::left_join(data.tas, by = "join", relationship = "many-to-many") |> 
    dplyr::mutate(pathway2 = forcats::fct_relevel(pathway2, "Carbon_Flux", "Biomass")) |> 
    dplyr::filter(pathway2 == "Biomass")
  
  
  ### ----- Plot data
  
  ## ---- Plot biomass -----
  data_to_plot_biomass <- data_to_plot |> 
    dplyr::filter(pathway2 %in% c("Biomass", "Biomass (Fishmip)"))
  
  
  plot.biomass <- ggplot2::ggplot(data = data_to_plot_biomass) +
    
    ## ---- Add points
    ggplot2::geom_point(mapping = ggplot2::aes(x = delta.tas, y = delta, color = year, shape = ssp), 
                        size    = 2, #1.5,
                        alpha   = 0.75,
                        show.legend = FALSE) +
    
    ggplot2::scale_color_viridis_c(name  = "Year", option = "viridis", direction = -1) +
    
    ggnewscale::new_scale_color() +
    
    
    ## ---- Add mean smooth 
    ggplot2::geom_ribbon(data = fishmip, 
                         mapping = ggplot2::aes(x     = delta.tas,
                                                # y     = delta_mean,
                                                # ymin  = delta_max,
                                                # ymax  = delta_min),
                                                ymin = predict(lm(delta_min ~ delta.tas)),
                                                ymax = predict(lm(delta_max ~ delta.tas))),
                         fill  = "#04664c", alpha = 0.15) +
    
    ggplot2::geom_ribbon(data = data_line,
                         mapping = ggplot2::aes(x = delta.tas,
                                                ymin = predict(lm(delta_min ~ delta.tas)),
                                                ymax = predict(lm(delta_max ~ delta.tas))),
                         fill  = "#10c79e", alpha = 0.15) +
    
    # ggplot2::geom_ribbon(data = data_line, 
    #                      mapping = ggplot2::aes(x     = delta.tas,
    #                                             y     = delta_mean,
    #                                             ymin  = delta_max,
    #                                             ymax  = delta_min),
    #                      fill  = "#7ae4e6", alpha = 0.25) +
    
    ggplot2::geom_smooth(data = data_line,
                         mapping = ggplot2::aes(x     = delta.tas, 
                                                y     = delta_mean, 
                                                color = factor(pathway2),
                                                linetype = factor(pathway2)), 
                         fill = "transparent", method = "lm", size = 1.2) +  # , color = scales::alpha("black", 0.6)
    
    
    ggplot2::geom_smooth(data = fishmip,
                         mapping = ggplot2::aes(x     = delta.tas, 
                                                y     = delta_mean), 
                         color = "#04664c",
                         fill = "transparent", method = "lm", size = 1.2) +
    
    
    ## ---- Add shaded areas
    # ggplot2::geom_ribbon(data = data_line,
    #                      mapping = ggplot2::aes(x     = delta.tas,
    #                                             y     = delta_mean,
    #                                             ymin  = delta_max,
    #                                             ymax  = delta_min,
    #                                             fill  = factor(pathway2)),
    #                      alpha = 0.65,
    #                      show.legend = FALSE) +
  
  
  
  ## ---- Color & fill scales
  ggplot2::scale_linetype_manual(values = c("Biomass"     = "solid",
                                            "Biomass (Fishmip)" = "solid"),
                                 # "Carbon_Flux" = "solid"),
                                 labels = NULL,
                                 name   = NULL, 
                                 guide  = NULL) +
    
    ggplot2::scale_color_manual(values = c("Biomass"           = "#10c79e",
                                           "Biomass (Fishmip)" = "#04664c"),
                                # "Carbon_Flux" = "#c2ae0a"),
                                labels = c("Biomass (Fishmip)" = "B (FishMIP)",
                                           "Biomass" = "B (BOATS)"),
                                # "Carbon_Flux" = "C export",),
                                name = NULL,
                                guide = NULL) +
    # guide = ggplot2::guide_legend(override.aes = list(linetype = c("Biomass"     = "solid",
    #                                                                "Biomass (Fishmip)" = "solid")))) + 
    # "Carbon_Flux" = "solid")))) +
    
    ggplot2::scale_fill_manual(values = c("Biomass"     = "#10c79e"),
                               # "Carbon_Flux" = "#c2ae0a"),
                               labels = c("Biomass" = "Biomass"),
                               # "Carbon_Flux" = "C export",),
                               name = NULL,
                               guide = NULL) +
    
    ggplot2::scale_shape_manual(values = c("Historical" = 16, "ssp126" = 1, "ssp585" = 2), 
                                name   = NULL,
                                labels = c("Historical", "SSP 1-2.6", "SSP 5-8.5"),
                                guide = NULL) + 
    # guide = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    
    ## ---- Add labels
    ggplot2::geom_label(label = "BOATS\nSlope = -4.2", x = 1.7, y = -21, label.padding = unit(0.3, "lines"), label.size = 0.35, # x = 1.7, y = -27.5
                        color = "black",
                        fill  = "#61edce") +
    # ggplot2::geom_label(label = "BOATS\nSlope = -2.5", x = 4.5, y = -4.5, label.padding = unit(0.3, "lines"), label.size = 0.35,
    #                     color = "black",
    #                     fill  = "#decc35") +
    ggplot2::geom_label(label = "FishMIP\nSlope = -2.8", x = 4.5, y = -4.5, label.padding = unit(0.3, "lines"), label.size = 0.35, # x = 1.7, y = -21
                        color = "black",
                        fill  = "#75baa8") +
    
    ggplot2::xlab(label =  "Change in TAS (\u00B0C)") + #expression("Change in TAS ("*degree*"C)")
    ggplot2::ylab(label = "Change in biomass (%)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x     = element_text(size = 11),
                   axis.text.y     = element_text(size = 11),
                   axis.title.x    = element_text(size = 13),
                   axis.title.y    = element_text(size = 13),
                   legend.text     = element_text(size = 12),
                   legend.title    = element_text(size = 13)) 
  
  # -----
  
  ## ---- Plot fluxes -----
  data_to_plot_flux <- data_to_plot |> 
    dplyr::filter(pathway2 %in% c("Biomass", "Carbon_Flux"))
  
  data_line_flux = data.carb |>
    dplyr::group_by(year, ssp, pathway2) |>
    dplyr::summarise(delta_mean = mean(delta),
                     delta_sd   = sd(delta),
                     delta_min  = delta_mean - delta_sd,
                     delta_max  = delta_mean + delta_sd) |>
    tidyr::unite(join, c(year, ssp), sep = "_", remove = FALSE) |>  
    dplyr::left_join(data.tas, by = "join", relationship = "many-to-many") |> 
    dplyr::mutate(pathway2 = forcats::fct_relevel(pathway2, "Carbon_Flux", "Biomass")) |> 
    dplyr::filter(pathway2 == "Carbon_Flux")
  
  plot.flux <- data_to_plot |> 
    dplyr::filter(pathway2 == "Carbon_Flux") |> 
    ggplot2::ggplot() +
    
    ## ---- Add points
    ggplot2::geom_point(mapping = ggplot2::aes(x = delta.tas, # |> dplyr::filter(pathway2 == "Carbon_Flux"), 
                                               y = delta, # |> dplyr::filter(pathway2 == "Carbon_Flux"), 
                                               color = year, shape = ssp), 
                        size    = 2, # 1.5,
                        alpha   = 0.75) +
    
    ggplot2::scale_color_viridis_c(name  = "Year", option = "viridis", direction = -1) +
    
    ggnewscale::new_scale_color() +
    
    
    ## ---- Add mean smooth 
    ggplot2::geom_ribbon(data = data_line_flux,
                         mapping = ggplot2::aes(x     = delta.tas,
                                                # y     = delta_mean,
                                                # ymin  = delta_max,
                                                # ymax  = delta_min),
                                                ymin = predict(lm(delta_min ~ delta.tas)),
                                                ymax = predict(lm(delta_max ~ delta.tas))),
                         fill  = "#6e48b0", alpha = 0.15) +
    
    ggplot2::geom_smooth(data = data_to_plot_flux,
                         mapping = ggplot2::aes(x     = delta.tas, 
                                                y     = delta, 
                                                color = factor(pathway2),
                                                linetype = factor(pathway2)), 
                         fill = "transparent", method = "lm", size = 1.2) +  # , color = scales::alpha("black", 0.6)
    
    
    ## ---- Add shaded areas
    # ggplot2::geom_ribbon(data = data_line,
    #                      mapping = ggplot2::aes(x     = delta.tas,
    #                                             y     = delta_mean,
    #                                             ymin  = delta_max,
    #                                             ymax  = delta_min,
    #                                             fill  = factor(pathway2)),
    #                      alpha = 0.65,
    #                      show.legend = FALSE) +
  
  
  
  ## ---- Color & fill scales
  ggplot2::scale_linetype_manual(values = c("Biomass"     = "dashed",
                                            "Carbon_Flux" = "solid"),
                                 # "Carbon_Flux" = "solid"),
                                 labels = NULL,
                                 name   = NULL, 
                                 guide  = "none") +
    
    ggplot2::scale_color_manual(values = c("Biomass"           = "#decc35",
                                           "Carbon_Flux" = "#6b2adb"),
                                # "Carbon_Flux" = "#c2ae0a"),
                                labels = c("Carbon_Flux" = "C export",
                                           "Biomass" = "C export"),
                                # "Carbon_Flux" = "C export",),
                                name = NULL,
                                guide = NULL) +
    # guide = ggplot2::guide_legend(override.aes = list(linetype = c("Biomass"     = "solid",
    #                                                                "Carbon_Flux" = "dotted")))) + 
    # "Carbon_Flux" = "solid")))) +
    
    # ggplot2::scale_fill_manual(values = c("Biomass"     = "#10c79e"),
    #                            # "Carbon_Flux" = "#c2ae0a"),
    #                            labels = c("Biomass" = "Biomass"),
    #                            # "Carbon_Flux" = "C export",),
    #                            name = NULL) +
    
    ggplot2::scale_shape_manual(values = c("Historical" = 16, "ssp126" = 1, "ssp585" = 2), 
                                name   = NULL,
                                labels = c("Historical", "SSP 1-2.6", "SSP 5-8.5"),
                                guide = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    
    ## ---- Add labels
    ggplot2::geom_label(label = "BOATS\nSlope = -4.2", x = 1.7, y = -19, label.padding = unit(0.3, "lines"), label.size = 0.35, # x = 1.7, y = -27.5
                        color = "black",
                        fill  = "#decc35") +
    ggplot2::geom_label(label = "BOATS\nSlope = -2.5", x = 4.5, y = -4.5, label.padding = unit(0.3, "lines"), label.size = 0.35,
                        color = "black",
                        fill  = "#a77df0") +
    # ggplot2::geom_label(label = "FishMIP\nSlope = -2.8", x = 4.5, y = -4.5, label.padding = unit(0.3, "lines"), label.size = 0.35, # x = 1.7, y = -21
    #                     color = "black",
    #                     fill  = "#75baa8") +
    
    ggplot2::xlab(label = "Change in TAS (\u00B0C)") +
    ggplot2::ylab(label = "Change in C export (%)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x     = element_text(size = 11),
                   axis.text.y     = element_text(size = 11),
                   axis.title.x    = element_text(size = 13),
                   axis.title.y    = element_text(size = 13),
                   legend.text     = element_text(size = 12),
                   legend.title    = element_text(size = 13))
  
  # -----
  
  outputs <- list(plot_biomass = plot.biomass,
                  plot.flux    = plot.flux,
                  data_line_biomass = data_line,
                  data_line_flux    = data_line_flux)
  
  if(! is.null(name)) {
    
    save(outputs, file = here::here("RData", paste0(name, ".RData")))
    # ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 8, height = 3.5, device = "png")
    # ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 4, height = 4, device = "png")
    # ggplot2::ggsave(here::here("figures", "Projection_paper", paste0(name, ".png")), width = 7, height = 4.5, device = "png")
    
    
  }
  
  return(outputs)  
  
}



#' Delta Biomass Versus Delta Fluxes For The Paper
#'
#' @param data.carbon 
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
deltaB_vs_deltaFlux <- function(data.carbon, name = NULL){
  
  ### ----- Format data
  data.carb <- data.carbon |> 
    dplyr::filter(pathway %in% c("Carcass", "Fecal Pellets", "Biomass") & fishing == "hf" & ssp != "ssp585") |> 
    # dplyr::mutate(pathway2 = dplyr::case_when(pathway == "Biomass" ~ "Biomass",
    #                                           TRUE ~ "Carbon_Flux")) |>
    dplyr::group_by(year, pathway, set.number, ssp, ESM) |> 
    dplyr::summarise(carbon = sum(carbon, na.rm = T),
                     ref    = sum(ref, na.rm = T)) |> 
    dplyr::mutate(delta = ((carbon - ref)/ref)*100) |> 
    dplyr::group_by(year, set.number, ssp, pathway) |> 
    dplyr::summarise(delta = mean(delta)) |> 
    tidyr::unite(join, c(year, ssp), sep = "_", remove = FALSE) |> 
    dplyr::rename("delta.carb" = "delta") |> 
    tidyr::pivot_wider(id_cols = 1:4, names_from = pathway, values_from = delta.carb) |> 
    tidyr::pivot_longer(cols = 6:7, values_to = "Carbon_Flux", names_to = "flux_type") |> 
    dplyr::mutate(Biomass = -Biomass) |> 
    dplyr::mutate(flux_type = forcats::fct_relevel(flux_type, "Fecal Pellets", "Carcass"))
  
  data_line <- data.carb |> 
    dplyr::group_by(year, ssp, flux_type) |>
    dplyr::summarise(delta_mean = mean(Carbon_Flux),
                     delta_sd   = sd(Carbon_Flux),
                     delta_min  = delta_mean - delta_sd,
                     delta_max  = delta_mean + delta_sd,
                     biomass = mean(Biomass))
  
  ### ----- Plot data for fecal pellets
  data.carb.fp <- data.carb |> 
    dplyr::filter(flux_type == "Fecal Pellets")
  
  data.line.fp <- data_line |> 
    dplyr::filter(flux_type == "Fecal Pellets") 
  # dplyr::mutate(min.test = lm(delta_min ~ biomass)$fitted.values,
  #               max.test = lm(delta_max ~ biomass)$fitted.values,
  #               mean.test = lm(delta_mean ~ biomass)$fitted.values)
  
  ## ---- Add the max vlues to expand the range of prediction 
  mod.min = lm(delta_min ~ biomass, data = data.line.fp)
  mod.max = lm(delta_max ~ biomass, data = data.line.fp)
  data.ribbon.fp = data.frame(biomass = c(data.line.fp$biomass, max(data.carb.fp$Biomass)),
                              delta_min = c(data.line.fp$delta_min, min(data.carb.fp$Carbon_Flux)),
                              delta_max = c(data.line.fp$delta_max, min(data.carb.fp$Carbon_Flux))) 
  data.ribbon.fp$ymin = predict(mod.min, newdata = data.ribbon.fp)
  data.ribbon.fp$ymax = predict(mod.max, newdata = data.ribbon.fp)
  
  plot.fp <- ggplot2::ggplot(data.carb.fp) +
    ggplot2::geom_point(mapping = ggplot2::aes(x = Biomass, 
                                               y = Carbon_Flux, 
                                               # alpha   = 0.75,
                                               color   = year,
                                               shape   = ssp), 
                        size = 2.5, show.legend = FALSE) +
    
    ggplot2::scale_color_viridis_c(name = "Year", option = "viridis", direction = -1) +
    
    
    ggnewscale::new_scale_color() +
    
    ## ---- Add mean smooth 
    # ggplot2::geom_ribbon(data = data.line.fp,
    #                      # mapping = ggplot2::aes(x     = biomass,
    #                      #                        y     = mean.test,
    #                      #                        ymin  = max.test,
    #                      #                        ymax  = min.test),
    #                      mapping = ggplot2::aes(x = biomass, 
    #                                             ymin = predict(lm(delta_min ~ biomass)),
    #                                             ymax = predict(lm(delta_max ~ biomass))),
    #                      fill  = "#89a3e0", alpha = 0.25) +
  ggplot2::geom_ribbon(data = data.ribbon.fp,
                       mapping = ggplot2::aes(x = biomass, 
                                              ymin = ymin,
                                              ymax = ymax),
                       fill  = "#89a3e0", alpha = 0.25) +
    
    # ggplot2::geom_smooth(mapping = ggplot2::aes(x = Biomass,
    #                                             y = Carbon_Flux,
    #                                             color = factor(flux_type)),
    #                      fill = "transparent", method = "lm") +
    
    ggplot2::geom_smooth(data = data.line.fp,
                         mapping = ggplot2::aes(x = biomass,
                                                y = delta_mean,
                                                color = factor(flux_type)),
                         fill = "transparent", method = "lm", fullrange = TRUE) +
    
    # geom_line(data = data.line.fp,
    #           mapping = ggplot2::aes(x = biomass,
    #                                  y = predict(lm(delta_mean ~ biomass)),
    #                                  color = factor(flux_type))) +
    
    ggplot2::geom_abline(intercept = 0, slope = -1, color = "#87426d", linewidth = 1, linetype = "dashed") +
    
    ggplot2::scale_linetype_manual(values = "solid",
                                   labels = NULL,
                                   name   = NULL, 
                                   guide  = NULL) +
    
    ggplot2::scale_shape_manual(values = c("Historical" = 2, "ssp126" = 1), 
                                name   = NULL,
                                labels = c("Historical", "SSP 1-2.6"),
                                guide = NULL) + 
    
    ggplot2::scale_color_manual(values = c("Fecal Pellets" = "#07297a"),
                                labels = NULL, 
                                name = NULL,
                                guide = NULL) +
    
    
    ggplot2::geom_label(label = "BOATS\nSlope = -0.73", x = 75, y = -15, label.padding = unit(0.3, "lines"), label.size = 0.35,
                        color = "black",
                        fill  = "#89a3e0") +
    
    ggplot2::geom_label(label = "Slope = -1", x = 25, y = -45, label.padding = unit(0.3, "lines"), label.size = 0.35,
                        color = "black",
                        fill  = "#a77df0") +
    
    ggplot2::xlab(label = "Biomass loss (%)") +
    ggplot2::ylab(label = "Change in C export (%)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x     = element_text(size = 11),
                   axis.text.y     = element_text(size = 11),
                   axis.title.x    = element_text(size = 13),
                   axis.title.y    = element_text(size = 13),
                   legend.text     = element_text(size = 12),
                   legend.title    = element_text(size = 13))
  
  
  ### ----- Plot data for carcass
  data.carb.car <- data.carb |> 
    dplyr::filter(flux_type == "Carcass")
  
  data.line.car <- data_line |> 
    dplyr::filter(flux_type == "Carcass") 
  # dplyr::mutate(min.test = lm(delta_min ~ biomass)$fitted.values,
  #               max.test = lm(delta_max ~ biomass)$fitted.values,
  #               mean.test = lm(delta_mean ~ biomass)$fitted.values)
  
  ## ---- Add the max vlues to expand the range of prediction 
  mod.min = lm(delta_min ~ biomass, data = data.line.car)
  mod.max = lm(delta_max ~ biomass, data = data.line.car)
  data.ribbon.car = data.frame(biomass = c(data.line.car$biomass, max(data.carb.car$Biomass)),
                               delta_min = c(data.line.car$delta_min, min(data.carb.car$Carbon_Flux)),
                               delta_max = c(data.line.car$delta_max, min(data.carb.car$Carbon_Flux))) 
  data.ribbon.car$ymin = predict(mod.min, newdata = data.ribbon.car)
  data.ribbon.car$ymax = predict(mod.max, newdata = data.ribbon.car)
  
  plot.car <- ggplot2::ggplot(data.carb.car) +
    
    ggplot2::geom_ribbon(data = data.ribbon.car,
                         # mapping = ggplot2::aes(x     = biomass,
                         #                        y     = mean.test,
                         #                        ymin  = max.test,
                         #                        ymax  = min.test),
                         mapping = ggplot2::aes(x = biomass, 
                                                ymin = ymin,
                                                ymax = ymax),
                         fill  = "#efc912", alpha = 0.25) +
    
    ggplot2::geom_point(mapping = ggplot2::aes(x = Biomass, 
                                               y = Carbon_Flux,
                                               # alpha = 0.75,
                                               shape = ssp,
                                               color = year), 
                        size = 2.5) +
    
    ggplot2::scale_color_viridis_c(name = "Year", option = "viridis", direction = -1) +
    
    
    ggnewscale::new_scale_color() +
    
    
    
    # ggplot2::geom_smooth(mapping = ggplot2::aes(x = Biomass, 
    #                                             y = Carbon_Flux, 
    #                                             color = factor(flux_type)), 
    #                      fill = "transparent", method = "lm") +
    
    ggplot2::geom_smooth(data = data.line.car,
                         mapping = ggplot2::aes(x = biomass,
                                                y = delta_mean,
                                                color = factor(flux_type)),
                         fill = "transparent", method = "lm", fullrange = TRUE) +
    
    ggplot2::geom_abline(intercept = 0, slope = -1, color = "#87426d", linewidth = 1, linetype = "dashed") +
    
    ggplot2::scale_linetype_manual(values = "solid",
                                   labels = NULL,
                                   name   = NULL, 
                                   guide  = "none") +
    
    ggplot2::scale_shape_manual(values = c("Historical" = 2, "ssp126" = 1), 
                                name   = NULL,
                                labels = c("Historical", "SSP 1-2.6")) +
    
    # ggplot2::scale_alpha_continuous(guide = "none") +
    
    # guide = "none") + 
    
    ggplot2::scale_color_manual(values = c("Carcass" = "#efc912"),
                                labels = NULL, 
                                name = NULL,
                                guide = NULL) +
    
    
    ggplot2::geom_label(label = "BOATS\nSlope = -0.89", x = 62.5, y = -25, label.padding = unit(0.3, "lines"), label.size = 0.35,
                        color = "black",
                        fill  = "#decc35") +
    
    ggplot2::geom_label(label = "Slope = -1", x = 25, y = -45, label.padding = unit(0.3, "lines"), label.size = 0.35,
                        color = "black",
                        fill  = "#a77df0") +
    
    ggplot2::xlab(label = "Biomass loss (%)") +
    ggplot2::ylab(label = "Change in C export (%)") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x     = element_text(size = 11),
                   axis.text.y     = element_text(size = 11),
                   axis.title.x    = element_text(size = 13),
                   axis.title.y    = element_text(size = 13),
                   legend.text     = element_text(size = 12),
                   legend.title    = element_text(size = 13))
  
  outputs <- list(plot.fp = plot.fp,
                  plot.car = plot.car,
                  data_line_car = data.line.car,
                  data_line_fp = data.line.fp)
  
  if(! is.null(name)) {
    
    save(outputs, file = here::here("RData", paste0(name, ".RData")))
    # ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 8, height = 3.5, device = "png")
    # ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 3.7, height = 4, device = "png")
    # ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 5.5, height = 4, device = "png")
    
    
  }
  
  return(outputs)
  
}


#' Univariate Maps
#'
#' @param data_map a list with output from CarcasSink::format_data_to_map()
#' @param values values for the color scale
#' @param color_scale the color scale of cells
#' @param delta if map delta data, = TRUE
#' @param name the names of the map to be saved
#' @param legend the name of the color scale legend
#' @param show.legend 
#' @param overlap TRUE/FALSE
#'
#' @return
#' @export
#'
#' @examples
univariate_map <- function(data_map, values, color_scale, delta = FALSE, log_trans = TRUE, min_value, max_value, legend, show.legend, overlap, factor_overlap, name = NULL){
  
  # if(delta == TRUE & values[3] > 0){data_map$data$layer[data_map$data$layer > 0] <- NA ; values[3] = 0}
  if(delta == TRUE){data_map$data$layer[data_map$data$layer > 100] <- 100}
  if(log_trans == TRUE){data_map$data$layer <- log(data_map$data$layer + 1)}
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data,
                     mapping = ggplot2::aes(fill     = layer,
                                            geometry = geometry),
                     color   = NA,
                     size    = 0.01,
                     show.legend = show.legend) +
    
    ## Add graticules
    ggplot2::geom_sf(data     = data_map$graticules, 
                     linetype = "dotted", 
                     color    = "black", 
                     size     = 0.4) +
    
    ## Add borders grid
    ggplot2::geom_sf(data   = data_map$borders, 
                     colour = NA,  
                     fill   = "gray70") +
    
    ggplot2::geom_sf(data   = data_map$box, 
                     colour = "black", 
                     fill   = NA, 
                     size   = 0.1) +
    
    
    
    ## Add latitude and longitude labels
    ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) + 
    
    ggplot2::labs(fill = legend) +
    
    ggplot2::guides(size = "none", fill = guide_colourbar(title.position = "right", barwidth = 0.7)) +
    
    ## Theme
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = NA),
                   panel.background   = ggplot2::element_blank(),
                   axis.text          = ggplot2::element_blank(),
                   axis.ticks         = ggplot2::element_blank(), 
                   axis.title         = ggplot2::element_blank(),
                   plot.margin        = ggplot2::unit(c(0,0,0,0), "cm"),
                   plot.title         = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = -0.5),
                   legend.title       = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = 0.5, angle = 90),
                   legend.title.align = 0.5, 
                   legend.direction   = "vertical",
                   legend.text        = ggplot2::element_text(size = 12))
  
  
  # if(delta == FALSE){map <- map + scale_fill_viridis_c(na.value = "grey80")} # , limits = c(0, max(rastdf, na.rm=T))
  # if(delta == TRUE){map <- map + scale_fill_gradient2(low = "red", mid = "white", high = "darkblue", midpoint = midpoint, na.value = "grey20")}
  
  ### Scale fill colors
  if(overlap == FALSE){
    map <- map +
      ggplot2::scale_fill_gradientn(colors   = color_scale,
                                    values   = if(delta == TRUE){scales::rescale(values)}, 
                                    # limits   = if(delta == TRUE){c(round(min(values),0), round(max(values), 0))} else {c(min(data_map$data$layer), max(data_map$data$layer))},
                                    limits   = if(delta == TRUE){c(round(min(values),0), round(max(values), 0))} else {c(min_value, max_value)},
                                    na.value = "transparent") 
    
  }
  
  if(overlap == TRUE){
    
    if(factor_overlap == FALSE){
      map <- map +
        ggplot2::scale_fill_manual(values = values, na.value = "transparent") +
        theme(legend.position = "right", #"bottom",
              legend.spacing.y = unit(1, 'lines')) +
        # legend.direction = "horizontal") +
        # theme(legend.key = element_rect(size = 3)) +
        guides(fill = guide_legend(override.aes = list(color = "black", size = 1))) # , nrow = 1
    }
    
    if(factor_overlap == TRUE){
      map <- map +
        # scale_fill_discrete(values = viridis::viridis(option = color_scale, direction = -1, n = 9), na.value = "transparent", na.tranlate = F) +
        ggplot2::scale_fill_viridis_d(option = color_scale, direction = -1) +
        theme(legend.position = "right",
              legend.spacing.y = unit(1, 'lines')) +
        # theme(legend.key = element_rect(size = 3)) +
        guides(fill = guide_legend(override.aes = list(color = "black", size = 1)))
    }
    
  }
  
  
  
  ### Save map
  if(! is.null(name)) {
    
    save(map, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 7, height = 4.5, device = "jpeg", dpi = 300)
    
  }
  
  return(map)
  
  
}



#' Bivariate Color Scale 
#'
#' @param nquantiles the number of quantile to split the data
#' @param upperleft the color in the top left corner
#' @param upperright the color in the top right corner
#' @param bottomleft the color in the bottom left corner
#' @param bottomright the color in the bottom right corner
#' @param xlab x axis label for the plot
#' @param ylab y axis label for the plot
#'
#' @return
#' @export
#'
#' @examples
color_bivariate_map <- function(nquantiles, upperleft, upperright, bottomleft, bottomright, xlab, ylab){
  
  ### Create classes
  my.data <- seq(0, 1, .01)
  my.class <- classInt::classIntervals(my.data, n = nquantiles, style = "quantile") 
  
  ### Extract classes from my.class and assign them a color
  my.pal.1 <- classInt::findColours(my.class, c(upperleft,bottomleft)) 
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright)) 
  
  ### Create a matrix of colors
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  
  ## For loop to assign a color to each element of the matrix
  for(i in 1:101){
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i])) 
    col.matrix[102-i,] <- classInt::findColours(my.class, my.col) # Assign a different color betwwen color extract in my.col for each element of the row 
  }
  
  
  plot(c(1,1), pch = 19, col = my.pal.1, cex = 0.5, xlim = c(0,1), ylim = c(0,1), frame.plot = F, xlab = xlab, ylab = ylab, cex.lab = 1.3) # pch = shape of the point
  
  ### For loop to plot squares
  for(i in 1:101){
    col.temp <- col.matrix[i-1,]
    points(my.data, rep((i-1)/100, 101), pch = 15, col = col.temp, cex = 1) # Drawing a sequence of point at the specified coordinates.
  }
  
  seqs <- seq(0, 100, (100/nquantiles)) 
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)] # Matrix of color with with 10 rows and 10 columns
  
  
  ### Turn it into a table that would match with data
  tbl_color <- dplyr::as_tibble(col.matrix[2:11, 2:11])  |> 
    tidyr::gather(group, fill, V1, V2, V3, V4, V5, V6, V7, V8, V9 ,V10) |> 
    dplyr::mutate(group = as.character(c(seq(1.10, 10.1, 1), seq(1.2, 10.2, 1), seq(1.3, 10.3, 1), seq(1.4, 10.4, 1), seq(1.5, 10.5, 1), seq(1.6, 10.6, 1),
                                         seq(1.70, 10.7, 1), seq(1.8, 10.8, 1), seq(1.9, 10.9, 1), 
                                         "1.10", "2.10", "3.10", "4.10", "5.10", "6.10", "7.10", "8.10", "9.10", "10.10"))) |> 
    as.data.frame(.) |> 
    dplyr::rename()
  
  return(tbl_color)
  
}



#' Bivariate Map
#'
#' @param data_map the data ready to map obtained with CarcasSink::format_data_bivariate_map() 
#' @param bivariate_color_scale a df of the bivariate color scale, obtained with CarcasSink::color_bivariate_map()
#' @param name the name of the map to be saved
#'
#' @return
#' @export
#'
#' @examples
bivariate_map <- function(data_map, bivariate_color_scale, xlab, ylab, name){
  
  # data_map <- tibble::as.tibble(data_map)
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data, 
                     mapping = ggplot2::aes(fill     = fill,
                                            geometry = geometry), 
                     color   = NA, 
                     size    = 0.01) +
    
    ggplot2::scale_fill_identity(na.value = "grey80") +
    ggplot2::theme_void() +
    
    ## Add graticules
    ggplot2::geom_sf(data     = data_map$graticules,
                     linetype = "dotted",
                     color    = "black",
                     size     = 0.4) +
    
    ## Add borders grid
    ggplot2::geom_sf(data   = data_map$borders,
                     colour = NA,
                     fill   = "gray70") +
    
    ggplot2::geom_sf(data   = data_map$box,
                     colour = "black",
                     fill   = NA,
                     size   = 0.1) +
    
    ## Add latitude and longitude labels
    ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) +
    
    ## Theme
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = NA),
                   panel.background   = ggplot2::element_blank(),
                   axis.text          = ggplot2::element_blank(),
                   axis.ticks         = ggplot2::element_blank(), 
                   axis.title         = ggplot2::element_blank(),
                   plot.margin        = ggplot2::unit(c(0,0,0,0), "cm"),
                   plot.title         = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = -0.5),
                   legend.title       = ggplot2::element_text(size  = 20, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = 0.5),
                   legend.text        = ggplot2::element_text(size = 16))
  
  
  ### Color legend
  
  ## Separate groups
  color <- bivariate_color_scale |> 
    dplyr::mutate(x = as.integer(rep(seq(1, 10, 1), 10)),
                  y = as.integer(rep(1:10, each = 10)))
  
  
  ## Plot
  legend <- ggplot2::ggplot() +
    
    ggplot2::geom_tile(data    = color, 
                       mapping = ggplot2::aes(x = x, y = y, fill = fill)) +
    
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = xlab, y = ylab) +
    # ggplot2::geom_hline(yintercept = 3.5, color = "red") +
    cowplot::theme_map() +
    ggplot2::theme(axis.title      = ggplot2::element_text(size = 16), 
                   axis.title.x    = ggplot2::element_text(margin = ggplot2::margin(t = 0, 
                                                                                    r = 0, 
                                                                                    b = 0, 
                                                                                    l = 0)),
                   axis.title.y    = ggplot2::element_text(angle  = 90,
                                                           margin = ggplot2::margin(t = 0,
                                                                                    r = 5,
                                                                                    b = 0,
                                                                                    l = 0)),
                   plot.background = ggplot2::element_rect(fill  = "white", 
                                                           color = "transparent")) +
    ggplot2::coord_fixed()
  
  
  ### Arrange map with legend
  map_bi <- cowplot::ggdraw() +
    # cowplot::draw_plot(map,    x = 0.0, y = 0.00, width = 0.70, height = 1.0) +
    # cowplot::draw_plot(legend, x = 0.65, y = 0.30, width = 0.35, height = 0.35)
    cowplot::draw_plot(map,    x = 0.0, y = 0.30, width = 1.0, height = 0.7) +
    cowplot::draw_plot(legend, x = 0.35, y = 0.00, width = 0.3, height = 0.3)
  
  
  ### Save map
  if(! is.null(name)) {
    
    save(map_bi, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 8.5, height = 6, device = "jpeg", dpi = 300)
    
  }
  
  return(map_bi)
  
}



#' Map A Raster
#'
#' @param data 
#' @param grp 
#' @param sc 
#' @param midpoint 
#'
#' @return
#' @export
#'
#' @examples
map_raster <- function(data, grp, sc, midpoint){
  
  ## --- Rasterize
  rast <- raster::raster(pracma::flipud(data[,, grp, sc]), 
                         ymn = -90,
                         ymx = 90,
                         xmn = -180,
                         xmx = 180,
                         crs = "+proj=longlat +datum=WGS84 +no_defs")
  
  data_plot <- raster::as.data.frame(rast, xy = TRUE)
  
  
  ## --- Plot
  map <- ggplot() +
    geom_raster(data = data_plot, aes(x = x, y = y, fill = layer)) +
    scale_fill_gradient2(low = "darkblue", mid = "white", high = "red", midpoint = midpoint,
                         limits = c(0, 2)) +
    scale_x_continuous(breaks = seq(-180, 180, 20), expand = c(0,0)) +
    scale_y_continuous(breaks = seq(-90, 90, 20), expand = c(0,0)) +
    labs(x = "Longitude", y = "Latitude", fill = "Contribution (%)") +
    theme_bw() +
    ggtitle(paste0("Size Class: ", sc)) +
    guides(size = "none", fill = guide_colourbar(title.position = "right", barwidth = 0.7)) +
    theme(plot.title    = element_text(size = 15, face = "bold", hjust = 0.5, vjust = -1),
          legend.title  = ggplot2::element_text(size  = 14, 
                                                face  = "bold", 
                                                hjust = 0.5, 
                                                vjust = 0.5, angle = -90),
          legend.text        = ggplot2::element_text(size = 14),
          legend.title.align = 0.5, 
          legend.direction   = "vertical")
  
  
  # ggsave(map, 
  #        filename = paste0(here::here("Figures/Maps_mortality"), "/Group", grp, "/SC", sc, ".jpeg"), 
  #        device = "jpeg",
  #        width    = 7,
  #        height   = 3.5)
  
  return(map)
  
  
}


#' Plot Sequestration Barplot
#'
#' @param data.ss steady state data
#' @param data.trans transient simulation data
#' @param type esm or fishing
#' @param name 
#' @param delta 
#' @param ylab_trans 
#'
#' @return
#' @export
#'
#' @examples
yearly_seq_plot <- function(data.ss, data.trans, delta, ylab_trans, type, name = NULL){
  
  if(type == "esm"){names(data.ss)[3] <- "fill" ; names(data.trans)[3] <- "color"}
  if(type == "fishing"){
    
    names(data.ss)[4] <- "fill" ; names(data.trans)[4] <- "color"
    
    data.ss <- data.ss |> 
      dplyr::mutate(fill = forcats::fct_relevel(fill, "nh", "hf"))
    
    data.trans <- data.trans |> 
      dplyr::mutate(fill = forcats::fct_relevel(color, "nh", "hf"))
    
  }
  
  if(type == "both"){
    
    data.ss <- data.ss |>  
      tidyr::unite(fill, ssp, fishing) |> 
      dplyr::mutate(fill = forcats::fct_relevel(fill, "ssp126_nh", "ssp585_nh", "ssp126_hf", "ssp585_hf"))
    
    data.trans <- data.trans |>  
      tidyr::unite(color, ssp, fishing) |> 
      dplyr::mutate(color = forcats::fct_relevel(color, "ssp126_nh", "ssp585_nh", "ssp126_hf", "ssp585_hf")) |> 
      dplyr::group_by(decades, color) |> 
      dplyr::summarise(sum_decade = mean(sum_decade))
    
  }
  
  plot_ss <- ggplot2::ggplot(data = data.ss, mapping = ggplot2::aes(x = decades, y = sum_decade, fill = fill)) +
    ggplot2::geom_col(position="dodge") + #fill = "grey60"
    # geom_errorbar(mapping  = aes(ymin = (y-sd), ymax = (y+sd)),
    #               width    = 0.15, 
    #               colour   = "grey10", 
    #               stat     ="identity", 
    #               position = position_dodge(.9)) +
    # facet_wrap(~facet) +
    # ggplot2::geom_text(mapping  = ggplot2::aes(label = round(sum_decade, 1), x = decades, y = sum_decade),
    #                    size     = 2.5, 
    #                    hjust    = 0.5, 
    #                    vjust    = -0.3,
    #                    angle    = 60, 
  #                    position = ggplot2::position_dodge(1)) +
  ggplot2::labs(x = NULL, y = "Sequestration (GtC)") + 
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1, size = 11),
                   axis.text.y     = ggplot2::element_text(size = 11),
                   axis.title.y    = ggplot2::element_text(size = 15),
                   legend.text     = ggplot2::element_text(size = 12),
                   legend.title    = ggplot2::element_text(size = 13),
                   legend.position = "right")
  
  
  if(delta == TRUE){
    
    mean_data <- data.trans |> 
      dplyr::filter(decades > 1989 & decades < 2000) |> 
      dplyr::group_by(esm, fishing) |> 
      dplyr::summarise(mean_ref = mean(sum_decade))
    
    data.trans <- dplyr::left_join(data.trans, mean_data, by = "esm") |> 
      dplyr::mutate(sum_decade = ((sum_decade - mean_ref)/mean_ref) *100)
    
  }
  
  plot_trans <- ggplot2::ggplot(data.trans, ggplot2::aes(x = decades, y = sum_decade, color = color)) +
    ggplot2::geom_point(show.legend = FALSE) +
    ggplot2::geom_smooth(se = TRUE, show.legend = FALSE) +
    ggplot2::geom_smooth(se = FALSE, show.legend = FALSE) +
    ggplot2::labs(y = ylab_trans, x = NULL) +
    # ggplot2::scale_y_continuous(limits = c(0, max(data.trans$sum_decade))) +
    ggplot2::scale_x_continuous(breaks = seq(1950, 2100, 15)) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x     = ggplot2::element_text(angle = 45, hjust = 0.9, size = 11),
                   axis.text.y     = ggplot2::element_text(size = 11),
                   axis.title.y    = ggplot2::element_text(size = 15),
                   legend.text     = ggplot2::element_text(size = 12),
                   legend.title    = ggplot2::element_text(size = 13))
  
  if(type == "esm"){
    plot_ss <- plot_ss +
      ggplot2::scale_fill_manual(values = c("ssp126" = "darkslategray3", "ssp585" = "firebrick4"),
                                 labels = c("SSP 1-2.6", "SSP 5-8.5"),
                                 name = NULL) 
    
    plot_trans <- plot_trans +
      ggplot2::scale_color_manual(values = c("ssp126" = "darkslategray3", "ssp585" = "firebrick4"),
                                  labels = c("SSP 1-2.6", "SSP 5-8.5"),
                                  name = NULL) 
  }
  
  
  if(type == "fishing"){
    plot_ss <- plot_ss +
      ggplot2::scale_fill_manual(values = c("nh" = "darkslategray3", "hf" = "firebrick4"),
                                 labels = c("No Fishing", "Fishing"),
                                 name = NULL) 
    
    plot_trans <- plot_trans +
      ggplot2::scale_color_manual(values = c("nh" = "darkslategray3", "hf" = "firebrick4"),
                                  labels = c("No Fishing", "Fishing"),
                                  name = NULL) 
  }
  
  if(type == "both"){
    
    plot_ss <- plot_ss +
      # scale_fill_manual(values = c("ssp126_nh" = "darkslategray3", "ssp585_hf" = "firebrick4"),
      #                   labels = c("SSP 1-2.6/No", "SSP 5-8.5/Yes"),
      #                   name = "Climate/Fishing:")
      scale_fill_manual(values = c("ssp126_nh" = "dodgerblue2", "ssp585_nh" = "firebrick2", 
                                   "ssp126_hf" = "dodgerblue4", "ssp585_hf" = "firebrick4"),
                        labels = c("SSP 1-2.6/No", "SSP 5-8.5/No", "SSP 1-2.6/Yes", "SSP 5-8.5/Yes"),
                        name   = "Climate/Fishing:")
    
    plot_trans <- plot_trans +
      # ggplot2::scale_color_manual(values = c("ssp126_nh" = "darkslategray3", "ssp585_hf" = "firebrick4"),
      #                             labels = c("SSP 1-2.6/No", "SSP 5-8.5/Yes"),
      #                             name = NULL) 
      scale_color_manual(values = c("ssp126_nh" = "dodgerblue2", "ssp585_nh" = "firebrick2", 
                                    "ssp126_hf" = "dodgerblue4", "ssp585_hf" = "firebrick4"),
                         labels = c("SSP 1-2.6/No", "SSP 5-8.5/No", "SSP 1-2.6/Yes", "SSP 5-8.5/Yes"),
                         name   = "Climate/Fishing:")
  }
  
  fig <- cowplot::ggdraw() +
    cowplot::draw_plot(plot_trans, x = 0.00, y = 0.00, width = 0.4, height = 1) + 
    cowplot::draw_plot(plot_ss,    x = 0.40, y = 0.00, width = 0.6, height = 1) +
    cowplot::draw_plot_label(label = c("(a)", "(b)"),
                             size = 11,
                             x = c(0, 0.39),
                             y = c(1, 1))
  
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("Figures", paste0(name, ".jpeg")), width = 8, height = 3, device = "jpeg", dpi = 1000)
    
  }
  
  return(fig)
  
}


