library(tidyverse)
library(fs)
library(sf)
library(lubridate)
library(furrr)
library(stars)
library(ncdf4)
library(ncdf4.helpers)
library(openair)
library(ggpubr)
library(gridExtra)
library(rnaturalearth)
source('emep_qaqc_user_input.R')
source('QAQC_R_files/myquicktext.R')
source('QAQC_R_files/emep_vars_parameters.R')
source('QAQC_R_files/emep_qaqc_funcs.R')

VAR_PARAMS_LIST = get_params_list() # list of parameters for each EMEP var 
OBS_VAR_PARAMS_LIST = get_obs_var_params_list() # list of plotting params for EMEP vars used in obs-mod comparison 


# MAIN --------------------------------------------------------------------
future::plan(multicore)

# Budget calculation + deposition and surface conc (DSC) plots -------------
if (CALCULATE_BUDGET == T | PLOT_DSC == T) {
  emep_budget = select_vars(vars = BUDGET_VARS, var_params_list = VAR_PARAMS_LIST, param = 'budg_factor') %>% 
    load_emep_data(EMEP_BUDGET_FNAME, EMEP_BUDGET_CRS, vars = .)
  
  if (!is.na(BUDGET_MASK_FNAME)) {
    budget_mask = st_read(BUDGET_MASK_FNAME)
  } else {
    budget_mask = NULL
  }
  
  #get test file modelled year to put into output filename
  mod_test_year = emep_budget[[1]] %>% 
    st_get_dimension_values('time') %>% 
    year()
  
  emep_budget2 = emep_budget %>% 
    map(st_as_stars) %>% 
    map(apply_area_mask, area_mask = budget_mask)
  
  budget_df = map_dfr(emep_budget2, calc_budget, VAR_PARAMS_LIST, .id = 'run') %>% 
    arrange(Variable) %>% 
    mutate(run = str_replace(run, '_[^_]+$', '')) %>% #drop characters after the last underscore (e.g. 'fullrun.nc')
    write_csv(path(tables_pth_out, paste0(path_ext_remove(BUDGET_TABLE_FNAME), '_', mod_test_year,
                                          '.', path_ext(BUDGET_TABLE_FNAME))))
  #plot the difference if comparing two budget files
  if (length(EMEP_BUDGET_FNAME) == 2) {
    budget_df2 = budget_df %>%
      mutate(run2 = if_else(run == str_replace(EMEP_BUDGET_FNAME[1], '_[^_]+$', ''), 'test', 'ref')) %>% 
      pivot_longer(cols = any_of(c('Total', 'Mean')), names_to = 'Stat', values_to = 'value') %>%
      pivot_wider(id_cols = c(Variable, Stat, Unit), names_from = run2, values_from = value) %>% 
      drop_na() %>% 
      mutate(abs_diff = test - ref,
             rel_diff = abs_diff/ref*100)
    
    budget_plot = plot_budget_diff(budget_df2, threshold = BUDGET_DIFF_THRESHOLD)
    ggsave(path(plots_pth_out, BUDGET_PLOT_FNAME), height = 10, width = 7, type = 'cairo')
    
  }
  
  if (PLOT_DSC == T) {
    #plot only the test run (the first element in EMEP_BUDGET_FNAME)
    dsc_plots = plot_DSC(emep_budget2[[1]], VAR_PARAMS_LIST)
    
    page_titles = format_maps_page_title(inner_test_pth = EMEP_BUDGET_FNAME[1], run_labels = 'EMEP run') %>% 
      rep(length(dsc_plots))
    
    export = marrangeGrob(grobs = dsc_plots, nrow = 1, ncol = 1, top = substitute(page_titles[g]))
    ggsave(filename = path(maps_pth_out, DSC_MAPS_FNAME), export, paper = 'a4', height = 10, width = 7)
  }
  
  rm(dsc_plots, emep_budget2, export)
  
}


# Comparison maps ---------------------------------------------------------

if (PLOT_COMPARISON_MAPS == T) {
  #collate emep_vars
  comp_map_vars = select_vars(vars = COMP_MAP_VARS, var_params_list = VAR_PARAMS_LIST, param = 'map_levs')
  
  comp_map_data = future_map(comp_map_vars, ~calculate_emep_diff(var = .x, run_labels = RUN_LABELS,
                                                              outer_test_fname = TEST_OUTER_FNAME, outer_ref_fname = REF_OUTER_FNAME,
                                                              inner_test_fname = TEST_INNER_FNAME, inner_ref_fname = REF_INNER_FNAME, emep_crs = EMEP_CRS))
  null_vars_lgl = comp_map_data %>% 
    map_lgl(is_null)
  null_vars = comp_map_vars[null_vars_lgl]
  comp_plots_list = map(comp_map_data[!null_vars_lgl], plot_comp_maps, ncl_palette_dir = PALETTE_DIR, pretty_lab = PRETTY_LABS)

  page_titles = format_maps_page_title(outer_test_pth = TEST_OUTER_FNAME,
                                       outer_ref_pth = REF_OUTER_FNAME,
                                       inner_test_pth = TEST_INNER_FNAME,
                                       inner_ref_pth = REF_INNER_FNAME,
                                       run_labels = str_c(RUN_LABELS, ' run')) %>% 
    rep(length(comp_plots_list))
  
  export = marrangeGrob(grobs = comp_plots_list, nrow = 1, ncol = 1, top = substitute(page_titles[g]))
  ggsave(filename = path(maps_pth_out, COMP_MAPS_FNAME), export, paper = 'a4', height = 10, width = 7)
  
  rm(comp_plots_list, export)
  
}



# Modelled vs Observed ----------------------------------------------------

if(COLLATE_MOBS == T) {
  
  obs_fname = str_replace(TEST_OBS_FNAME, '_[^_]+$', '_hour.nc') #make sure hourly data are read
  
  #determine pollutants to be analysed
  mobs_polls_list = map(VAR_PARAMS_LIST, 'obs') %>% 
    compact()
  
  #create lookup table between pollutants and emep vars
  mobs_polls = names(mobs_polls_list) %>% 
    set_names(flatten_chr(mobs_polls_list)) %>% 
    .[OBSERVED_POLLS] %>% 
    na.omit()
  
  #extract year from emep file 
  mod_dates = get_emep_floordate(obs_fname)
  mod_year = unique(year(mod_dates$date))
  
  #extract latitude and longitude values from emep file
  nc = nc_open(obs_fname)
  nc_lon = ncvar_get(nc, "lon")
  nc_lat = ncvar_get(nc, "lat")
  nc_close(nc)
  
  #determine network of automatic sites or read file with meta of custom measurements
  if (length(AUTO_NETWORK[!file_exists(AUTO_NETWORK)]) > 0) {
    auto_sites = get_auto_meta(network = AUTO_NETWORK[!file_exists(AUTO_NETWORK)],
                               pollutant = names(mobs_polls), year = mod_year) %>% 
      mutate(network = as.character(network)) %>% #must be a string for file_exists function
      mutate(year = mod_year, .after = poll_info) #just for info
  } else auto_sites = NULL
  
  if (length(AUTO_NETWORK[file_exists(AUTO_NETWORK)]) == 1) {
    custom_sites = read_rds(AUTO_NETWORK[file_exists(AUTO_NETWORK)])
  } else custom_sites = NULL
  
  sites_geo = bind_rows(auto_sites, custom_sites) %>% 
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
  
  #get a 2D slice of the EMEP output to check if all sites are in the modelled domain
  #loads quicker to check on the fullrun data
  emep_slice = load_emep_data(str_replace(obs_fname, '_[^_]+$', '_fullrun.nc'),
                              emep_crs = EMEP_CRS, vars = names(mobs_polls)[1])[[1]] %>% 
    st_as_stars() 
  
  sites_in = get_sites_in_domain(sites_geo, emep_slice)
  
  sites_geo = sites_geo %>% 
    filter(code %in% sites_in) %>% 
    mutate(longitude = st_coordinates(.)[ , 1],
           latitude = st_coordinates(.)[ , 2],
           .after = site_type_grp)
  
  #get the 'x' and 'y' indexes for extracting data using ncdf4 functions
  emep_indexes = future_pmap_dfr(list(longitude = sites_geo$longitude,
                                      latitude = sites_geo$latitude),
                                 get_emep_indexes, TEST_INNER_FNAME)
  
  sites_geo = sites_geo %>% 
    mutate(i = emep_indexes$row,
           j = emep_indexes$col,
           .before = poll_info)
  
  #write meta data to files - rds for further use and csv for easier human check
  write_rds(sites_geo, path(data_pth_out, '00Sites_used.rds'))
  # sites_geo %>% 
  #   st_drop_geometry() %>% 
  #   unnest(poll_info) %>% 
  #   write_csv(path(data_pth_out, '00Sites_used.csv'), na ='')
  
  #create a dataframe for site-pollutant iteration (enables parallel processing using furrr)
  iter_df = cross_df(list(code = sites_geo$code, poll = names(mobs_polls))) %>% 
    left_join(dplyr::select(sites_geo, code, network, i, j))
  
  #collate modelled and observed data to a list
  mobs_tlist = future_pmap(list(site_code = iter_df$code,
                                i_index = iter_df$i,
                                j_index = iter_df$j,
                                network = iter_df$network,
                                pollutant = iter_df$poll),
                           safely(collate_obs_mod_nc), nc_pth = obs_fname,
                           poll_name_lookup = mobs_polls,
                           .options = furrr_options(seed = T)) %>%
    transpose()
  
  # and filter successful outcomes
  is_ok <- mobs_tlist$error %>%
    map_lgl(is_null)
  
  #combine data per site
  mobs_list = mobs_tlist$result[is_ok] %>%
    bind_rows() %>%
    split(.$code)
  
  write_rds(mobs_list, 'MOBS.rds')
  
  #output iterations ending in error
  print(iter_df[!is_ok, ])
  
  if ('ox' %in% OBSERVED_POLLS) {
    mobs_list = mobs_list %>%
      future_map(add_ox, .options = furrr_options(seed = T))
  }  
  
  #annual mean summary
  a_means = map_dfr(mobs_list, summarise_mobs, avg_time = 'year') %>% 
    left_join(st_drop_geometry(dplyr::select(sites_geo, code, site_type_grp)))
  mobs_stats_all = modStats(a_means, statistic = MODSTATS_STATS, type = c('pollutant')) %>% 
    mutate(site_type_grp = 'All')
  mobs_stats = modStats(a_means, statistic = MODSTATS_STATS, type = c('pollutant', 'site_type_grp')) %>% 
    bind_rows(mobs_stats_all, .) %>% 
    arrange(pollutant) %>% 
    relocate(site_type_grp, .after = pollutant)
  write_csv(mobs_stats, path(tables_pth_out, MOBS_STATS_FNAME)) 
  
  if (SAVE_MOBS_RDS == T) {
    
    #convert to wide format for writing out to file (smaller file size)
    mobs_wide = mobs_list %>% 
      map(~pivot_wider(.x, id_cols = c(date, code), names_from = c(pollutant), values_from = c(obs, mod)))
    
    mobs_data_pths = path(data_pth_out, paste0(names(mobs_list), '_mod_obs'), ext = 'rds')
    future_walk2(mobs_wide, mobs_data_pths, write_rds, .options = furrr_options(seed = T))
  }
}

if (PLOT_MOBS_PDF == T) {
  if (COLLATE_MOBS != T) {
    
    #recreate lookup table between pollutants and emep vars
    obs_fname = str_replace(TEST_OBS_FNAME, '_[^_]+$', '_hour.nc')
    mobs_polls_list = map(VAR_PARAMS_LIST, 'obs') %>% 
      compact()
    mobs_polls = names(mobs_polls_list) %>% 
      set_names(flatten_chr(mobs_polls_list)) %>% 
      .[OBSERVED_POLLS] %>% 
      na.omit()  
    mobs_polls = mobs_polls[mobs_polls %in% extract_nc_vars(obs_fname)]
    
    #read saved meta data
    sites_geo = read_rds(dir_ls(data_pth_out, regexp = '(S|s)ites.*\\.rds'))
    
    #read saved mobs data
    mobs_pths = dir_ls(data_pth_out, regexp = '\\.rds') %>% 
      str_subset('(S|s)ites', negate = T)
    mobs_list = future_map(mobs_pths, read_rds) %>% 
      future_map(mobs_to_long) %>% 
      #temporary to ensure code column is never NA due to a previous bug
      future_map(~mutate(.x, code = if_else(is.na(code), na.omit(unique(.$code)), code)))
  }
  
  #plot scatter plots of annual means (like for like - missing observations are dropped from modelled data too)
  mobs_plots = plot_annual_scatter(mobs_list = mobs_list, site_meta_df = sites_geo, poll_name_lookup = mobs_polls)
  mobs_plots = mobs_plots[order(match(mobs_plots, mobs_polls))]
  
  #get EMEP run fname for title on pdf plots
  run_info = format_maps_page_title(inner_test_pth = obs_fname, run_labels = 'EMEP run')
  
  page_titles = rep(run_info, length(mobs_plots) %/% PPP + 1)
  export = marrangeGrob(grobs = mobs_plots, nrow = PPP , ncol = 1, top = substitute(page_titles[g]))
  ggsave(filename = path(plots_pth_out, 'Annual_Mean_Scatter_all_sites.pdf'), export, paper = 'a4', height = 10, width = 7)
  
  #plot daily and hourly mobs per site
  future_walk(mobs_list, site_time_series_pdf3, sites_geo, out_dir = dir_create(path(plots_pth_out, 'Individual_sites')),
              ppp = PPP, run_title_info = run_info)
}

# Emissions comparison ---------------------------------------------------------------

if (COMPARE_EMISSIONS == T) {
  
  ###MassBudgetSummary.txt data
  MBS_list = map2(c(TEST_OUTER_FNAME, TEST_INNER_FNAME), 
                  c(REF_OUTER_FNAME, REF_INNER_FNAME), 
                  compare_run_emissions, save_file = T, mbs_table_fname = MBS_TABLE_FNAME)
  
  MBS_domains = map_chr(c(TEST_OUTER_FNAME, TEST_INNER_FNAME), extract_domain_from_fpath)
  MBS_plot_captions = c(str_c('Test: ', str_replace(path_file(TEST_OUTER_FNAME), '_[^_]+$', ''),'\n',
                              'Ref: ', str_replace(path_file(REF_OUTER_FNAME), '_[^_]+$', '')),
                        str_c('Test: ', str_replace(path_file(TEST_INNER_FNAME), '_[^_]+$', ''),'\n',
                              'Ref: ', str_replace(path_file(REF_INNER_FNAME), '_[^_]+$', '')))
  
  MBS_plots = map(MBS_list, plot_MBS_diff, threshold = EMISS_DIFF_THRESHOLD) %>% 
    map2(MBS_plot_captions, ~.x + labs(caption = .y) + theme(plot.caption = element_text(size = 6, face = 'italic') ))
  
  
  walk2(MBS_plots, path(plots_pth_out, str_c(MBS_domains, '_', MBS_PLOT_FNAME)),
        ~ggsave(.y, plot = .x, width = 7, height = 7, type = 'cairo'))
  
  ###RunLog.out emissions
  
  emiss_list = map(c(TEST_OUTER_FNAME, TEST_INNER_FNAME), compare_inv_mod_emissions, webdabEMEP_pth = 'QAQC_R_files/webdabEMEPNationalEmissions2000-2019.txt')
  
  #determine the number of plots per page so that they fit on A4 paper
  n_land = map_int(emiss_list, ~n_distinct(.x$Land))
  ppp = map_int(n_land, ~case_when(.x <=10 ~ 4L, .x >= 11 && .x <= 20 ~ 2L, .x > 20 ~ 1L))
  
  emiss_plot_list = map(emiss_list, plot_emiss_diff, threshold = EMISS_DIFF_THRESHOLD)
  
  emiss_domains = map_chr(c(TEST_OUTER_FNAME, TEST_INNER_FNAME), extract_domain_from_fpath)
  page_titles = map_chr(c(TEST_OUTER_FNAME, TEST_INNER_FNAME), strip_time_res) %>% 
    str_c('\n\n')
  
  emiss_plots_arranged = pmap(list(grobs = emiss_plot_list, nrow = ppp, top = page_titles),
                              marrangeGrob, ncol = 1)
  out_pths = path(plots_pth_out, str_c(emiss_domains, '_', INV_MOD_EMISS_PLOT_FNAME))
  
  walk2(emiss_plots_arranged, out_pths, ~ggsave(filename = .y, plot = .x,paper = 'a4', height = 10, width = 7))
  
}