# TIME TO ANOXIA
# COMPASS-FME

## 2-functions_analysis.R
## Use this script to for graphs and analysis

## KFP, August 2022

######################## ####
######################## ####

# set color palette ----
pal_horizons = soilpalettes::soil_palette("eutrostox", 2)

#
# OPTODES ----
plot_optode_data_all_samples = function(optode_data_processed){
  
  grouped = 
    optode_data_processed %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = sample_name))+
    geom_line()+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(timepoint ~ location)+
    theme(legend.position = "none")
  
#  grouped %>% plotly::ggplotly(.)
  
  individual = 
    optode_data_processed %>% 
    filter(!is.na(location)) %>%
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = location))+
    geom_line()+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      y = "Dissolved oxygen, mg/L")+
    facet_wrap(~timepoint+sample_name, ncol = 5)+
    #  theme(legend.position = "none")+
    NULL
  
  list(grouped = grouped,
       individual = individual)
}

plot_optode_data_OLD = function(optode_data_processed){
  
  grouped = 
    optode_data_processed %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("24-hour", "2-week-rep")) %>% 
    ggplot(aes(x = time_minutes/60, y = corrected_do_mg_L, color = sample_name))+
    geom_line()+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, hours",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(timepoint ~ location)+
    theme(legend.position = "none")
  
  grouped2 = 
    optode_data_processed %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = sample_name))+
    geom_line()+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(timepoint ~ location)+
    theme(legend.position = "none")
  
  individual = 
    optode_data_processed %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>%
    filter(location != "water") %>% 
    ggplot(aes(x = time_minutes, y = do_mg_L, color = location))+
    geom_line()+
  #  geom_point(data =   x_min %>% filter(is_min), color = "black")+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, minutes",
      y = "Dissolved oxygen, mg/L")+
    facet_wrap(~timepoint+sample_name, ncol = 5)+
    #  theme(legend.position = "none")+
    NULL
  
  list(grouped = grouped,
       individual = individual)
}

plot_optode_data = function(optode_data_processed){
  
  optode_combined2 = 
    optode_data_processed %>% 
    mutate(sample_type = location) %>% 
    separate(sample_type, sep = "-", into = c("transect", "horizon")) %>% 
    mutate(transect = factor(transect, levels = c("upland", "transition", "wetland")))
  
  gg_24hr = 
    optode_combined2 %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("24-hour")) %>% 
    filter(timepoint == c("24-hour", "2-week-rep")) %>% 
    ggplot(aes(x = time_minutes/60, y = DO_rolling_mgL, color = horizon, group = sample_name))+
    geom_line()+
    scale_color_manual(values = pal_horizons)+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, hours",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(. ~ transect)+
    theme(legend.position = "none")    
  
  
  gg_2wk = 
    optode_combined2 %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("2-week-rep")) %>% 
    ggplot(aes(x = (time_minutes/60)/24, y = DO_rolling_mgL, color = location, group = location))+
    geom_line(size = 1)+
    #   scale_color_manual(values = soilpalettes::soil_palette("podzol", 5))+
    #      scale_color_manual(values = PNWColors::pnw_palette("Sunset2", 4))+
    
    scale_color_manual(values = c("#cc5c76", "#f9ad2a", "#625a94", "#1d457f"))+
    
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, days",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    # facet_grid(timepoint ~ .)+
    theme(legend.position = c(0.5,0.5))  +
    NULL
  
  version1 = cowplot::plot_grid(gg_24hr + ggtitle("A: 24-hour"), gg_2wk + ggtitle("B: 2-week\n\n"), 
                     rel_widths = c(2.7,1),
                     label_y = 1,
                     label_x = 0)
  
  
  
  version2_gg_24hr = 
    optode_combined2 %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(timepoint == c("24-hour")) %>% 
    filter(timepoint == c("24-hour", "2-week-rep")) %>% 
    ggplot(aes(x = time_minutes/60, y = DO_rolling_mgL, color = horizon, group = sample_name))+
    geom_line()+
    geom_hline(yintercept = 8.5, linetype = "dashed")+
    scale_color_manual(values = pal_horizons)+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, hours",
      y = "Dissolved oxygen, mg/L")+
    #geom_smooth(se = F)+
    facet_grid(. ~ transect)+
    theme(legend.position = "none") +
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 14))+
    NULL
  
  version2_gg_2wk = 
    optode_combined2 %>% 
    filter(!grepl("skip", notes)) %>% 
    filter(!is.na(location)) %>% 
    filter(location != "water") %>% 
    filter(grepl("upland", location)) %>% 
    filter(timepoint == c("2-week-rep")) %>% 
    ggplot(aes(x = (time_minutes/60)/24, y = DO_rolling_mgL, color = horizon, group = location))+
    #    geom_line(size = 1)+
    geom_point()+
    geom_hline(yintercept = 8.5, linetype = "dashed")+
    ylim(0,10)+
    scale_color_manual(values = pal_horizons)+
    labs(#title = "Time to Anoxia",
      x = "Elapsed time, days",
      y = "Dissolved oxygen, mg/L")+
    #facet_grid(. ~ transect)+
    theme(legend.position = "none")  +
    NULL
  
  legend <- 
    cowplot::get_legend(
    version2_gg_2wk + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "top",
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 14)))
    
    
  version2 = cowplot::plot_grid(version2_gg_24hr + ggtitle("A: 24-hour"), 
                                version2_gg_2wk + ggtitle("B: 2-week (upland only) \n"), 
                     rel_widths = c(2.7,1.1),
                     label_y = 1,
                     label_x = 0)
  
  version2_legend = cowplot::plot_grid(legend, version2, nrow = 2, rel_heights = c(0.1, 1))
  
  list(version1 = version1,
       version2_legend = version2_legend)
  
}

compute_optode_slope = function(optode_data_processed){
  
  # first, figure out when the system turned anoxic
  max_time = 
    optode_data_processed %>% 
    group_by(sample_name) %>% 
    mutate(max = time_minutes == max(time_minutes)) %>% 
    filter(max) %>% 
    dplyr::select(-max)

  # ... or determine manually
  anoxia_time_manual = tribble(
    ~sample_name, ~anoxia_minutes,
    "anoxia_032", 690,
    "anoxia_034", 550,
    "anoxia_035", 695,
    "anoxia_045", 805,
    "anoxia_046", 1070,
    "anoxia_047", 1345,
    "anoxia_053", 765,
    "anoxia_056", 665,
    "anoxia_057", 645,
    "anoxia_060", 800,
    "anoxia_061", 450,
    "anoxia_062", 360
  )  

  # calculate slopes
  # all samples started at 8.5 and ended at 0 mg/L,
  # so all we really need is the time to anoxia
  anoxia_slopes = 
    anoxia_time_manual %>% 
    left_join(sample_key) %>% 
    mutate(
      anoxia_hr = anoxia_minutes/60,
      anoxia_rate = (8.5 - 0)/anoxia_hr) %>% 
    group_by(timepoint, location) %>% 
    dplyr::summarise(
      rate_mean = mean(anoxia_rate),
      rate_se = sd(anoxia_rate)/sqrt(n()),
      anoxia_hr_mean = mean(anoxia_hr),
      anoxia_hr_se = sd(anoxia_hr)/sqrt(n())
    )
}

#
# WEOC ----
 
plot_weoc = function(weoc_processed, sample_key){
  weoc = 
    weoc_processed %>% 
    left_join(sample_key) %>% 
    recode_levels()
  
  mg_l = 
    weoc %>% 
    ggplot(aes(x = timepoint, y = npoc_corr_mgL, shape = horizon, color = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    facet_wrap(~transect)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, mg/L")
  
  ug_g = 
    weoc %>% 
    ggplot(aes(x = timepoint, y = npoc_ug_g, shape = horizon, color = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    facet_wrap(~transect)+
    labs(title = "Water-extractable organic carbon",
         x = "", 
         y = "WEOC, μg/g soil")+
    theme(legend.position = "top")
  
  list(mg_l = mg_l,
       ug_g = ug_g)
}


#
# IONS ----

plot_ions = function(ions_processed, sample_key){
  ions = 
    ions_processed %>% 
    left_join(sample_key) %>% 
    recode_levels() 

  ions_mg_l = 
    ions %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = amount_ppm, color = horizon, shape = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    facet_grid(ion ~ transect, scales = "free_y")+
    labs(title = "Dissolved ions",
         x = "", 
         y = "Ion concentrations, mg/L")  
  
  ions_ug_g =
    ions %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = amount_ug_g, color = horizon, shape = horizon))+
    geom_point(size = 2, stroke = 1)+
    scale_shape_manual(values = c(19, 1))+
    scale_color_manual(values = pal_horizons)+
    expand_limits(y = 0)+
    facet_grid(ion ~ transect, scales = "free_y")+
    labs(title = "Dissolved ions",
         x = "", 
         y = "Ion concentrations, μg/g soil")
  
  list(ions_mg_l = ions_mg_l,
       ions_ug_g = ions_ug_g)
}

# pH ----

plot_pH = function(pH_processed, sample_key){
  
  pH = 
    pH_processed %>% 
    left_join(sample_key)
  
    pH %>% 
    recode_levels() %>% 
    filter(location != "blank-filter") %>% 
    ggplot(aes(x = timepoint, y = pH, color = horizon, shape = horizon))+
      geom_point(size = 2, stroke = 1)+
      scale_shape_manual(values = c(19, 1))+
      scale_color_manual(values = pal_horizons)+
      facet_wrap(. ~ transect)+
    labs(title = "pH",
         x = "", 
         y = "pH")  
  
}


# map ----

make_map = function(){
  
  library(sf)
  ## Set CRS
  common_crs <- 4326
  
  ## Set map size and point size
  point_size <- 2
  map_width = 9
  map_height = 6
  
  ## Set regional and WLE/CB (inset) bounding boxes
  us_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)
  region_bbox <- c(xmin = -95, xmax = -60, ymin = 35, ymax = 50)
  cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5)
  wle_bbox <- c(xmin = -85, xmax = -79, ymin = 38, ymax = 44)
  
  ## Make US states map cropped to GL/CB region
  us <- 
    read_sf("cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
    st_transform(., crs = common_crs) %>% 
    st_crop(., y = us_bbox)
  
  region <- st_crop(us, y = region_bbox)
  
  ## Further crop states to WLE/CB region
  cb_states <- st_crop(region, y = cb_bbox)
  wle_states <- st_crop(region, y = wle_bbox)
  
  ## Get state labels
  st_labels = st_centroid(us) %>% 
    filter(!STUSPS %in% c("TN", "NC", "DC")) # remove state labels that mess with the graph
  
  ## Make the base map with all sites
  base_plot <- 
    ggplot() + 
    geom_sf(data = region) + 
    geom_sf_text(data = st_labels, aes(label = STUSPS))+
    geom_rect(aes(xmin = -85, xmax = -79, ymin = 38, ymax = 44), 
              fill = NA, color = "black", lwd = 0.75) +
    geom_segment(aes(x = -85, xend = -75.5, y = 38, yend = 34.3), 
                 color = "black", lwd = 0.75) + 
    geom_segment(aes(x = -79, xend = -67.8, y = 44, yend = 42.5), 
                 color = "black", lwd = 0.75) + 
    xlim(-90, -67)+
    ylim(34.3, 49)+
    # cowplot::theme_map() + 
    theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
          legend.key = element_rect(fill = "transparent"), 
          legend.position = c(0.85, 0.1)) + 
    labs(x = "", y = "")+
    NULL
  
  ## Make the inset map with just WLE sites
  inset_plot <- 
    ggplot() + 
    geom_sf(data = wle_states) + 
    geom_point(aes(x = -82.3025, y = 41.22), size = 5, color = "red")+
    geom_text(aes(x = -82.3025, y = 40.9), label = "Old Woman Creek", size = 4)+
    geom_text(aes(x = -81.5, y = 42.5), label = "Lake Erie", size = 4)+
    cowplot::theme_map() + 
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "black", size = 1),
          axis.text = element_blank()) 
  
  ## Combine into single figure
  base_plot + 
    annotation_custom(
      ggplotGrob(inset_plot), 
      xmin = -76, xmax = -67, ymin = 28, ymax = 48.8)+
    theme_kp()
  
  
  
}
make_map_OLD = function(DAT, VAR, COLOR_LEGEND){
  
  
  # version 1 ----
  
  library(sf)
  
  ## Set CRS
  common_crs <- 4326
  
  ## Set map size and point size
  point_size <- 2
  map_width = 9
  map_height = 6
  
  # Set up map layers for plotting
  
  ## Set regional and CB (inset) bounding boxes
  us_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)
  region_bbox <- c(xmin = -87, xmax = -78, ymin = 40, ymax = 44)
  
  ## Make US states map cropped to GL/CB region
  us <- read_sf("cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
    st_transform(., crs = common_crs) %>% 
    st_crop(., y = us_bbox)
  
  region <- st_crop(us, y = region_bbox)
  
  #  # make a dataset merging metadata with site lat-longs
  #  df_map <- inner_join(DAT, latlong, by = "kit_id") %>% 
  #    st_as_sf(., coords = c("long", "lat"), crs = common_crs)
  
  
  ## Make the base map with all sites
  #  base_plot <- 
  ggplot() + 
    geom_sf(data = region) + 
    #geom_sf(data = meta_map, size = point_size * 3, color = "white") +
    # geom_sf(data = df_map, 
    #         color = "white", size = point_size * 2.5) + 
    # geom_sf(data = df_map, 
    #         aes_string(color = VAR), 
    #         size = point_size * 2) + 
    #    geom_rect(aes(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5), 
    #              fill = NA, color = "black", lwd = 0.75) +
    #    geom_segment(aes(x = -77.8, xend = -76, y = 40.5, yend = 42), 
    #                 color = "black", lwd = 0.75) + 
    #    geom_segment(aes(x = -74.5, xend = -71, y = 40.5, yend = 41.5), 
  #                 color = "black", lwd = 0.75) + 
  # theme_map() + 
  theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
        legend.key = element_rect(fill = "transparent"), 
        legend.position = c(0.85, 0.1)) + 
    # scale_color_viridis_c(limits = c(var_min, var_max)) + 
    #  labs(x = "", y = "" , color = COLOR_LEGEND) + 
    NULL
  
  
  # version 2 ----
  library(maps)
  
  states    <- c('New York', 'Pennsylvania',  'Indiana', 'Michigan', 'Ohio')
  us.states <- map_data("state", region=states)
  
  #longitudes <- c(-75.5, -75.2, -76.5, -77.7, -78.5, -79.4, -81.0, -83.5, -86.0)
  #latitudes <- c(23.2, 23.0, 24.0, 25.0, 25.7, 26.0, 26.3, 26.5, 27.5)
  #hurricane_rows <- data.frame(longitudes, latitudes)
  ggplot(data=us.states,aes(x=long,y=lat,group=group)) +
    geom_path() +
    coord_map() +
    xlim(-88, -77) +
    ylim(38, 44.5)+
    geom_point(aes(x = -82.3025, y = 41.2234), color="blue", size=4)+
    theme_bw()
  
  
  # version 3 ----
  library(usmap) #import the package
  
  plot_usmap(include = c('New York', 'Pennsylvania',  'Indiana', 'Michigan', 'Ohio'), labels = TRUE)+
    geom_point(aes(x = -82.3025, y = 41.22))
  
  #https://cran.r-project.org/web/packages/usmap/vignettes/advanced-mapping.html
  #
  
  # version 4 - PR's full map ----
  make_map = function(DAT, VAR, COLOR_LEGEND){
    
    ## Set CRS
    common_crs <- 4326
    
    ## Set map size and point size
    point_size <- 2
    map_width = 9
    map_height = 6
    
    # Set up map layers for plotting
    
    ## Set regional and CB (inset) bounding boxes
    us_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)
    region_bbox <- c(xmin = -95, xmax = -70, ymin = 35, ymax = 48)
    cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5)
    
    ## Make US states map cropped to GL/CB region
    us <- read_sf("cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
      st_transform(., crs = common_crs) %>% 
      st_crop(., y = us_bbox)
    
    region <- st_crop(us, y = region_bbox)
    
    ## Further crop states to CB region
    cb_states <- st_crop(region, y = cb_bbox)
    
    ##    # make a dataset merging metadata with site lat-longs
    ##    df_map <- inner_join(DAT, latlong, by = "kit_id") %>% 
    ##      st_as_sf(., coords = c("long", "lat"), crs = common_crs)
    ##    
    ##    ## Crop data  to CB region for inset plot
    ##    df_cb <- st_crop(df_map, y = cb_bbox)
    ##    
    ##    ## get max and min values for variable
    ##    var_min = DAT %>% dplyr::select(-kit_id) %>% pull() %>% min(na.rm = TRUE)
    ##    var_max = DAT %>% dplyr::select(-kit_id) %>% pull() %>% max(na.rm = TRUE)
    
    
    ## Make the base map with all sites
    base_plot <- 
      ggplot() + 
      geom_sf(data = region) + 
      #geom_sf(data = meta_map, size = point_size * 3, color = "white") +
      ##      geom_sf(data = df_map, 
      ##              color = "white", size = point_size * 2.5) + 
      ##      geom_sf(data = df_map, 
      ##              aes_string(color = VAR), 
      ##              size = point_size * 2) + 
      geom_rect(aes(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5), 
                fill = NA, color = "black", lwd = 0.75) +
      geom_segment(aes(x = -77.8, xend = -76, y = 40.5, yend = 42), 
                   color = "black", lwd = 0.75) + 
      geom_segment(aes(x = -74.5, xend = -71, y = 40.5, yend = 41.5), 
                   color = "black", lwd = 0.75) + 
      cowplot::theme_map() + 
      theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
            legend.key = element_rect(fill = "transparent"), 
            legend.position = c(0.85, 0.1)) + 
      #      scale_color_viridis_c(limits = c(var_min, var_max)) + 
      #      labs(x = "", y = "" , color = COLOR_LEGEND)+
      NULL
    
    ## Make the inset map with just CB sites
    inset_plot <- 
      ggplot() + 
      geom_sf(data = cb_states) + 
      #      geom_sf(data = df_cb, 
      #              color = "white", size = point_size * 2) + 
      #      geom_sf(data = df_cb, 
      #              aes_string(color = VAR), 
      #              size = point_size * 1.7) + 
      cowplot::theme_map() + 
      theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
            legend.key = element_rect(fill = "transparent"), 
            #legend.position = c(0.7, 0)
            legend.position = "none") + 
      #      scale_color_viridis_c(limits = c(var_min, var_max)) + 
      theme(panel.background = element_rect(fill = "white", colour = "black"),
            axis.text = element_blank()) + 
      labs(color = "")
    
    ## Combine into single figure
    base_plot + 
      annotation_custom(
        ggplotGrob(inset_plot), 
        xmin = -78, xmax = -70, ymin = 41, ymax = 48.8)
    
  }
  
  # version 5 - from PR's map for Anoxia
  ## Set CRS
  common_crs <- 4326
  
  ## Set map size and point size
  point_size <- 2
  map_width = 9
  map_height = 6
  
  # Set up map layers for plotting
  
  ## Set regional and CB (inset) bounding boxes
  us_bbox <- c(xmin = -125, xmax = -60, ymin = 20, ymax = 50)
  region_bbox <- c(xmin = -95, xmax = -60, ymin = 35, ymax = 50)
  cb_bbox <- c(xmin = -77.8, xmax = -74.5, ymin = 36.8, ymax = 40.5)
  wle_bbox <- c(xmin = -85, xmax = -79, ymin = 38, ymax = 44)
  
  ## Make US states map cropped to GL/CB region
  us <- read_sf("cb_2018_us_state_5m/cb_2018_us_state_5m.shp") %>% 
    st_transform(., crs = common_crs) %>% 
    st_crop(., y = us_bbox)
  
  region <- st_crop(us, y = region_bbox)
  
  ## Further crop states to CB region
  cb_states <- st_crop(region, y = cb_bbox)
  wle_states <- st_crop(region, y = wle_bbox)
  
  st_labels = st_centroid(us) %>% filter(!STUSPS %in% c("TN", "NC", "DC"))
  
  ## Make the base map with all sites
  base_plot <- 
    ggplot() + 
    geom_sf(data = region) + 
    geom_sf_text(data = st_labels, aes(label = STUSPS))+
    #      geom_sf_label(aes(label = NAME))
    geom_rect(aes(xmin = -85, xmax = -79, ymin = 38, ymax = 44), 
              fill = NA, color = "black", lwd = 0.75) +
    geom_segment(aes(x = -85, xend = -75.5, y = 38, yend = 34.3), 
                 color = "black", lwd = 0.75) + 
    geom_segment(aes(x = -79, xend = -67.8, y = 44, yend = 42.5), 
                 color = "black", lwd = 0.75) + 
    xlim(-90, -67)+
    ylim(34.3, 49)+
    #      cowplot::theme_map() + 
    theme(legend.background = element_rect(fill = alpha("white", 0.0)), 
          legend.key = element_rect(fill = "transparent"), 
          legend.position = c(0.85, 0.1)) + 
    labs(x = "", y = "")+
    NULL
  
  ## Make the inset map with just CB sites
  inset_plot <- 
    ggplot() + 
    geom_sf(data = wle_states) + 
    geom_point(aes(x = -82.3025, y = 41.22), size = 5, color = "red")+
    geom_text(aes(x = -82.3025, y = 40.9), label = "Old Woman Creek", size = 4)+
    geom_text(aes(x = -81.5, y = 42.5), label = "Lake Erie", size = 4)+
    cowplot::theme_map() + 
    #  theme_kp()+
    theme(legend.position = "none",
          panel.background = element_rect(fill = "white", colour = "black", size = 1),
          axis.text = element_blank()) 
  
  ## Combine into single figure
  base_plot + 
    annotation_custom(
      ggplotGrob(inset_plot), 
      xmin = -76, xmax = -67, ymin = 28, ymax = 48.8)+
    theme_kp()
  
  
  
}
