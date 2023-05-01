

process_troll = function(troll_data){
  troll_processed = 
    troll_data %>% 
    mutate(datetime = lubridate::ymd_hms(datetime))
}

plot_troll = function(troll_processed){
  
  # same x-axis
  gg_do_same_x = 
    troll_processed %>% 
    mutate(location = recode(location, "TR" = "Transition", "WC" = "Wetland")) %>% 
    ggplot(aes(x = hours, y = do_mgl,
               color = interaction(peak, location),
    ))+
    geom_line(size = 1,
              show.legend = F)+
    labs(x = "Hours",
         y = "Dissolved Oxygen, mg/L")+
    scale_color_brewer(palette = "PuOr")+
    facet_wrap(~location, nrow = 1)+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text.x = element_text(size=16, face="bold")
    )+
    NULL
  # save: 550l * 1100w
  
  # different x-axis
  gg_do = 
    troll_processed %>% 
    mutate(location = recode(location, "TR" = "Transition", "WC" = "Wetland")) %>% 
    ggplot(aes(x = hours, y = do_mgl,
               color = interaction(peak, location),
    ))+
    geom_line(size = 1,
              show.legend = F)+
    labs(x = "Hours",
         y = "Dissolved Oxygen, mg/L")+
    scale_color_brewer(palette = "PuOr")+
    facet_wrap(~location, scales = "free_x", 
               nrow = 1)+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text.x = element_text(size=16, face="bold")
    )+
    NULL
  
  
  # just one event - WETLAND
  gg_do_wetland_one_event = 
    troll_processed %>% 
    mutate(location = recode(location, "TR" = "Transition", "WC" = "Wetland")) %>% 
    filter(location == "Wetland" & peak == 1) %>% 
    ggplot(aes(x = hours, y = do_mgl,
               #color = interaction(peak, location),
    ))+
    geom_line(size = 1,
              show.legend = F)+
    labs(x = "Hours",
         y = "Dissolved Oxygen, mg/L",
         #title = "WETLAND"
    )+
    scale_color_brewer(palette = "PuOr")+
    facet_wrap(~location, scales = "free", 
               nrow = 2)+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text.x = element_text(size=16, face="bold")
    )+
    NULL
  # save 500l * 550w
  
  
  # ORP different x-axis
  gg_orp = 
    troll_processed %>% 
    mutate(location = recode(location, "TR" = "Transition", "WC" = "Wetland")) %>% 
    ggplot(aes(x = hours, y = p_h_orp,
               color = interaction(peak, location),
    ))+
    geom_line(size = 1,
              show.legend = F)+
    labs(x = "Hours",
         y = "Redox Potential")+
    scale_color_brewer(palette = "PuOr")+
    facet_wrap(~location, scales = "free_x", 
               nrow = 1)+
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          strip.text.x = element_text(size=16, face="bold")
    )+
    NULL
  
  library(patchwork)
  gg_combined = 
  #  gg_do / gg_orp + plot_annotation(title = "Field data from Aqua TROLLS")
  cowplot::plot_grid(gg_do, gg_orp, nrow = 2)
  
  

  # dual axes
  gg_combined_dual_axis = 
    troll_processed %>% 
    mutate(location = recode(location, "TR" = "Transition", "WC" = "Wetland")) %>% 
    ggplot(aes(x = hours))+
    facet_wrap(~location + peak, nrow = 2, scales = "free_x")+
    geom_line(aes(y = do_mgl), color = "red")+
    geom_line(aes(y = (p_h_orp+250)/50), color = "blue")+
    scale_y_continuous(sec.axis = sec_axis(~(.*50)-250, name = "Redox Potential (blue)"),
                       name = "Dissolved oxygen, mg/L (red)")+
    labs(title = "Field data from Aqua TROLLS")
 
  
    # y = tibble(x = c(0, 10),
    #            y = c(-250, 250))
    # 
    # lm(y~x, data = y)
}

compute_rates = function(troll_processed){
  
  
  troll_rates = 
    troll_processed %>% 
    group_by(location, peak) %>% 
    dplyr::mutate(MAX_DO_TIME = case_when(do_mgl == max(do_mgl) ~ datetime),
                  KEEP = datetime >= max(MAX_DO_TIME, na.rm = TRUE)) %>% 
    filter(KEEP) %>% 
    # dplyr::mutate(MIN_DO_TIME = case_when(do_mgl == min(do_mgl) ~ datetime)) %>% 
    dplyr::summarise(max_do = max(do_mgl),
                     min_do = min(do_mgl),
                     max_do_time = min(hours),
                     min_do_time = max(hours)) %>% 
    #mutate(d_time_hr = as.numeric(difftime(min_do_time, max_do_time))) %>% 
    force()
  
  
  troll_rates_labels = 
    troll_rates %>% 
    mutate(y = case_when(location == "TR" ~ 7,
                         location == "WC" ~ 6),
           label = paste(min_do_time - max_do_time, "hr"))
  
  
  troll_processed %>% 
    ggplot(aes(x = hours, y = do_mgl, color = location))+
    geom_line(size = 1)+
    
    geom_segment(data = troll_rates_labels, 
                 aes(x = max_do_time, xend = min_do_time,
                     y = y, yend = y))+
    geom_text(data = troll_rates_labels, hjust = 0,
              aes(x = max_do_time, y = y + 0.35, label = label))+
    facet_wrap(~location + peak, scales = "free_x")
  
  
  
  
  
  
  
  # comparing rates
  troll_rates2 = 
    troll_rates %>% 
    group_by(location, peak) %>% 
    dplyr::mutate(slope_mg_L_hr = (max_do - min_do)/(min_do_time - max_do_time)) %>% 
    mutate(time_to_anoxia_hr = (min_do_time - max_do_time)) %>% 
    rename(
      transect = location,
      replicate = peak) %>% 
    mutate(replicate = as.character(replicate)) %>% 
    mutate(experiment = "FIELD")
  
  
  
  
  optode2 = 
    optode_do_summary = 
    optode_combined2 %>% 
    filter(!is.na(location)) %>% 
    filter(location != "upland-B") %>% 
    filter(timepoint == "24-hour") %>% 
    group_by(sample_name) %>% 
    filter(slope == 0 & slope2 == 0) %>% 
    group_by(sample_name) %>% 
    slice(which.min(time_hr)) %>% 
    dplyr::select(sample_name, location, timepoint, transect, horizon, time_hr) %>% 
    mutate(time_hr = case_when(sample_name == "anoxia_047" ~ 24, TRUE ~ time_hr)) %>% 
    rename(replicate = sample_name) %>% 
    mutate(replicate = as.character(replicate)) %>% 
    mutate(slope_mg_L_hr = (8.5-0)/time_hr) %>% 
    rename(time_to_anoxia_hr = time_hr) %>% 
    mutate(experiment = "LAB")
  
  all = 
    troll_rates2 %>% 
    bind_rows(optode2) %>% 
    mutate(transect = recode(transect, "TR" = "transition", "WC" = "wetland")) %>% 
    mutate(transect = factor(transect, levels = c("upland", "transition", "wetland")))
  
  
  all %>% 
    ggplot(aes(x = transect, y = slope_mg_L_hr))+
    geom_jitter(width = 0.1, size = 3)+
    facet_wrap(~experiment)
  
  all %>% 
    ggplot(aes(x = transect, y = time_to_anoxia_hr))+
    geom_jitter(width = 0.1, size = 3)+
    facet_wrap(~experiment)
  
}