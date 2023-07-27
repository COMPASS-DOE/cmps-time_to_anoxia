
## sample key ----
sample_key = read.csv("1-data/characterization_data/sample_key.csv")
sample_key_subset = 
  sample_key %>% 
  filter(site == "OWC" & transect != "wte") %>% 
  filter(!(transect == "transition" & horizon == "B")) %>% 
  mutate(transect = recode(transect, "wc" = "wetland"),
         location = paste0(transect, "-", horizon))

#
## characterization chemistry ----

data_combined = read.csv("1-data/characterization_data/owc_characterization_2022-12-28.csv")


recode_levels2 = function(dat){
  dat %>% 
    mutate(location = factor(location, 
                             levels = c("upland-A", "upland-B", "transition-A", "wetland-A")),
           transect = factor(transect,
                             levels = c("upland", "transition", "wetland")))
}
pal_location_4 = c("#cc5c76", "#f9ad2a", "#625a94", "#1d457f")

  
  
data_combined_wide = 
  data_combined %>% 
  separate(name, sep = "_", into = "variable", remove = F) %>% 
  mutate(name = paste0(variable, " (", analysis, ")")) %>% 
  dplyr::select(sample_label, name, value) %>% 
  pivot_wider() %>% 
  left_join(sample_key_subset) %>% 
  filter(!is.na(site)) %>% 
  #  dplyr::select(-c("Ammonia (IC)","Bromide (IC)", "Nitrite (IC)", "Fluoride (IC)")) %>% 
  #  mutate(`Phosphate (IC)` = case_when(is.na(`Phosphate (IC)`) & site != "GCREW" ~ 0,
  #                                      TRUE ~ `Phosphate (IC)`)) %>% 
  recode_levels2() %>% 
  force()


compute_overall_pca = function(data_combined_wide, sample_key){
  library(ggbiplot)
  
  fit_pca_function = function(dat){
    
    dat %>% 
      drop_na()
    
    num = 
      dat %>%       
      dplyr::select(where(is.numeric)) %>%
      dplyr::mutate(row = row_number()) 
    
    num_row_numbers = num %>% dplyr::select(row)
    
    grp = 
      dat %>% 
      dplyr::select(-where(is.numeric)) %>% 
      dplyr::mutate(row = row_number()) 
    
    
    num = num %>% dplyr::select(-row)
    pca_int = prcomp(num, scale. = T)
    
    list(num = num,
         grp = grp,
         pca_int = pca_int)
  }
  
  combined_surface_no_ferrozine = 
    data_combined_wide %>% 
    dplyr::select(-ends_with("(Ferrozine)")) %>% 
    drop_na()
  
  ## PCA input files ----
  pca_overall = fit_pca_function(dat = combined_surface_no_ferrozine %>% dplyr::select(-ends_with("(IC)"), -"S (ICP)"))
  pca_overall_wle = fit_pca_function(combined_surface_no_ferrozine %>% filter(region == "WLE"))
  pca_overall_cb = fit_pca_function(combined_surface %>% filter(region == "CB") %>% dplyr::select(-ends_with("(IC)"), -"S (ICP)"))
  
  
  ## PCA plots overall ----
#  gg_pca_overall = 
    ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
             groups = as.character(x=pca_overall$grp$location), 
             ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1.5, alpha = 1,
               aes(shape = pca_overall$grp$location,
                 color = groups))+ 
    scale_shape_manual(breaks = c("upland-A", "upland-B", "transition-A", "wetland-A"),
                       values = c(1,2,3,4))+
    scale_color_manual(breaks = c("upland-A", "upland-B", "transition-A", "wetland-A"),
                       values = PNWColors::pnw_palette("Sailboat", 4))+
    #scale_shape_manual(values = c(21, 19))+
    #scale_shape_manual(values = c(21, 21, 19), name = "", guide = "none")+
    # xlim(-4,20)+
    # ylim(-8,8)+
    labs(shape="", color = "",
         title = "OWC soil characterization",
         subtitle = "PCA biplot")+
    theme_kp()+
    NULL
  

  
}

#
## fticr ----
icr_data_long = read.csv("1-data/characterization_data/icr_long_all_samples_owc_2023-01-08.csv")
icr_trt = read.csv("1-data/characterization_data/icr_long_treatments_owc_2023-01-08.csv")
icr_meta = read.csv("1-data/characterization_data/icr_meta_2023-01-08.csv")

compute_icr_relabund = function(icr_data_long, icr_meta, sample_key_subset){
  
  icr_data_long %>% 
    # add the Class column to the data
    left_join(dplyr::select(icr_meta, formula, Class), by = "formula") %>% 
    # calculate abundance of each Class as the sum of all counts
    group_by(sample_label, Class) %>%
    dplyr::summarise(abund = sum(presence)) %>%
    ungroup %>% 
    # create a new column for total counts per core assignment
    # and then calculate relative abundance  
    group_by(sample_label) %>% 
    dplyr::mutate(total = sum(abund),
                  relabund  = ((abund/total)*100)) %>% 
    left_join(sample_key_subset)
}
icr_relabund_samples = compute_icr_relabund(icr_data_long, icr_meta, sample_key_subset)

## icr-pca
compute_icr_pca = function(icr_relabund_samples, sample_key_subset){
  
  fit_pca_function = function(icr_relabund_samples, sample_key_subset){
    relabund_pca =
      icr_relabund_samples %>% 
      left_join(sample_key_subset) %>% 
      drop_na() %>% 
      ungroup %>% 
      dplyr::select(-c(abund, total)) %>% 
      spread(Class, relabund) %>% 
      filter(!is.na(region)) %>% 
      replace(.,is.na(.),0)
    
    num = 
      relabund_pca %>% 
      dplyr::select(where(is.numeric))
    
    grp = 
      relabund_pca %>% 
      dplyr::select(where(is.character)) %>% 
      dplyr::mutate(row = row_number())

    pca_int = prcomp(num, scale. = T)
    
    list(num = num,
         grp = grp,
         pca_int = pca_int)
  }
  
  
  pca_overall = fit_pca_function(icr_relabund_samples, sample_key_subset)

  
  # PCA biplots
  #biplot_all = 
  ggbiplot(pca_overall$pca_int, obs.scale = 1, var.scale = 1,
           groups = pca_overall$grp$location, 
           ellipse = TRUE, circle = FALSE, var.axes = TRUE, alpha = 0) +
    geom_point(size=3,stroke=1.5, alpha = 1,
               aes(shape = pca_overall$grp$location,
                   color = groups))+
    scale_shape_manual(breaks = c("upland-A", "upland-B", "transition-A", "wetland-A"),
                       values = c(1,2,3,4))+
    scale_color_manual(breaks = c("upland-A", "upland-B", "transition-A", "wetland-A"),
                       values = PNWColors::pnw_palette("Sailboat", 4))+
    xlim(-4,4)+
    ylim(-3.5,3.5)+
    labs(color = "", shape = "",
         title = "FTICR-MS data",
         subtitle = "PCA biplot")+
    theme(legend.position = "top", legend.box = "vertical")+
    NULL
  
  
}


## nosc
icr_nosc = 
  icr_data_long %>% 
  left_join(icr_meta %>% dplyr::select(formula, NOSC)) %>% 
  left_join(sample_key_subset, by = "sample_label")

icr_nosc %>% 
  ggplot(aes(x = NOSC, fill = sample_label))+
  geom_histogram()+
  facet_wrap(~location)

icr_nosc %>% 
  ggplot(aes(x = NOSC, fill = sample_label))+
  geom_density(alpha = 0)+
  facet_wrap(~location)
