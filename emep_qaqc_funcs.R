### FUNCTIONS USED FOR EMEP QAQC



# COLLATE and CALCULATE FUNCS -------------------------------------------------------

get_auto_meta = function(network = 'aurn', pollutant = 'no2', year = 2018) {
  
  #sites in multiple networks but with different site codes I'm
  #aware of. there may be more so add to the vector as needed
  DUPLICATED_SITES = c('CD9', 'BL0', 'CD1', 'CHEP', 'TD5', 'NM2', 'NM3', 'TH4')
  
  #pollutants in meta databases are in capitals 
  pollutant = str_to_upper(pollutant)
  network_levs = rev(network)
  names(network) = network
  
  if ('kcl' %in% network_levs) {
    network = network[str_detect(network, 'kcl', negate = T)] #remove kcl from network and treat separately
    
    #get a list of monitoring stations in London from the londonair website - used to handle
    #the kcl network monitors 
    #be aware that in the kcl network and in londonair datasets PM2.5 is named PM25!
    laqn = getMetaLAQN() %>% 
      as_tibble() %>% 
      filter(species_code %in% str_remove(pollutant, '\\.')) %>% 
      dplyr::select(code, poll = species_code)
    
    sites_kcl = importMeta(source = 'kcl', all = T) %>%
      left_join(laqn, ., by = 'code') %>% 
      mutate(network = factor('kcl', levels = network_levs, ordered = T),
             .before = code) %>%
      dplyr::select(network, code, site, site_type, poll, start_date = OpeningDate,
                    end_date = ClosingDate, longitude, latitude)
  }
  
  #networks = c(aurn = 'aurn', saqn = 'saqn', waqn = 'waqn', ni = 'ni', aqe = 'aqe')
  sites = map_dfr(network, ~importMeta(.x, all = T), .id = 'network') %>% 
    mutate(network = factor(network, levels = network_levs, ordered = T),
           end_date = na_if(end_date, 'ongoing'),
           end_date = as_datetime(end_date)) %>% 
    filter(variable %in% pollutant) %>% 
    rename(poll = variable) %>% 
    dplyr::select(-Parameter_name, -ratified_to, -zone, -agglomeration, -local_authority)
  
  if ('kcl' %in% network_levs) {
    #combine the kcl network with other networks to one tibble
    sites = bind_rows(sites, sites_kcl)
  }
  
  sites2 = sites %>% 
    #filter on year, operational sites have end date set to NA
    filter((year(start_date) <= year) & 
             (is.na(end_date) | year(end_date) >= year)) %>%
    mutate(poll = str_to_lower(poll)) %>% 
    #nest on pollutants and monitoring period
    group_by(across(-poll:-end_date)) %>%
    nest() %>%
    rename(poll_info = data) %>% 
    #and remove duplicate sites that occur in multiple networks
    #some sites have a different site code in different networks, 
    #so need to be removed manually 
    arrange(code, desc(network)) %>% 
    distinct(code, .keep_all = T) %>% 
    filter(!code %in% DUPLICATED_SITES) %>% 
    #recode site type into 5 main groups (note airport sites are now classified as industrial!)
    mutate(site_type_grp = recode_site_type(site_type),
           .after = site_type) %>% 
    ungroup()
  sites2
}

get_auto_data = function(site, network, pollutant, start_year, end_year, to_narrow = F) {
  #download automatic data from ricardo servers
  
  #make sure that the requested period is defined
  if (is.na(start_year) | is.na(end_year)) {
    stop('Either start or end of monitoring period is missing')
  } else {
    year_range = start_year:end_year
    
    if (network == 'waqn') {
      vs = importWAQN(site = site, year = year_range)
    } else if (network == 'saqn') {
      vs = importSAQN(site = site, year = year_range)
    } else if (network == 'ni') {
      vs = importNI(site = site, year = year_range)
    } else if (network == 'aurn') {
      vs = importAURN(site = site, year = year_range)
    } else if (network == 'aqe') {
      vs = openair::importAQE(site = site, year = year_range)
    } else if (network == 'kcl') {
      vs = importKCL(site = site, year = year_range)
    } else {
      vs = NULL
    }
  }
  
  #only process non-empty data frames
  if (is.null(vs)) {
    print(paste0('Site ', site, ' has no ', pollutant, ' data in ', start_year, ' \nor the data are currently NOT available.'))
    return(NULL)
  } else {
    if ('pm2.5' %in% pollutant) pollutant = c(pollutant, 'pm25')
    vs = vs %>% 
      select(date, code, any_of(str_to_lower(pollutant)))
    
    if ('pm25' %in% names(vs)) vs = rename(vs, pm2.5 = pm25)
    
    if (to_narrow == T) {
      vs = vs %>%
        pivot_longer(cols = any_of(pollutant), names_to = 'pollutant', values_to = 'conc')
    } 
  }
  vs = vs %>% 
    mutate(code = as.character(code)) # don't want code as factor

  vs
}

compare_file_size = function(test_pth, ref_pth) {
  
  ###tests if domains of test and ref match - temporarily disabled because of Tomas's uEMEP directory
  # domain = map_chr(c(test_pth, ref_pth), extract_domain_from_fpath)
  # if(length(unique(domain)) != 1) {
  #   stop('Comparing EMEP outputs in two different domains')
  # }
  
  d_content = map_dfr(c(test = test_pth, ref = ref_pth), ~dir_info(path_dir(.x)), .id = 'run') %>% 
    select(run, path, size)
  
  d_content2 = d_content %>% 
    filter(str_detect(path, '\\.nc$')) %>%
    mutate(path = path_ext_remove(path)) %>% 
    mutate(fname = str_extract(path, '(\\d{4}_[^\\d{4}]+$)|sites|sondes'),
           fname = str_replace(fname, '\\d{4}_?', ''),
           size = as.double(size)) %>% 
    filter(!is.na(fname)) %>%
    pivot_wider(id_cols = c(fname), names_from = 'run', values_from = 'size') %>%
    
    mutate(abs_diff = fs_bytes(abs(test - ref)),
           rel_diff = round((test - ref)/ref * 100, 1),
           test = fs_bytes(test),
           ref = fs_bytes(ref))
  
  d_content2
}

load_emep_data = function(emep_fname, emep_crs, vars = 'all', time_index = NULL) {
  ###returns a list of stars_proxy objects containing vars
  ###if vars are not in emep data, the function returns NULL
  
  ###!!!WIP - time slicing needs finishing!!!
  
  #This function loads vars data into a stars_proxy object
  emep_fname = set_names(emep_fname)
  
  emep_vars = map(emep_fname, extract_nc_vars)
  
  if(all(vars != 'all')) {
    #check if requested vars are in emep
    emep_vars = map(emep_vars, base::intersect, vars)
    if (any(lengths(emep_vars) == 0)) {
      return(NULL)
    }
  }
  
  emep_data = map2(emep_fname, emep_vars, ~read_stars(.x, sub = .y, proxy = T)) %>% 
    map(st_set_crs, emep_crs)
  
  emep_data
}

calc_budget = function(stars_object, evp_list) {
  #calculates the budget for all vars in stars_object based on parameters in evp_list
  budget_list = vector('list', length(names(stars_object)))
  
  for (i in seq_along(names(stars_object))) {
    emep_var = names(stars_object)[i]
    stars_sub = stars_object %>% 
      select(all_of(c(emep_var, 'Area_Grid_km2')))
    
    if (emep_var == 'Area_Grid_km2') {
      budget_list[[i]] = stars_sub %>% 
        select(Area_Grid_km2) %>% 
        as_tibble() %>% 
        summarise(Mean = as.double(mean(Area_Grid_km2, na.rm = T)))
    } else if (evp_list[[emep_var]][['budg_stat']] == 'sum') {
      budget_list[[i]] = stars_sub %>% 
        as_tibble() %>% 
        mutate(q = .data[[emep_var]] * Area_Grid_km2 * evp_list[[emep_var]][['budg_factor']]) %>% 
        summarise(Total = as.double(sum(q, na.rm = T)))
    } else if (evp_list[[emep_var]][['budg_stat']] == 'mean') {
      budget_list[[i]] = stars_sub %>% 
        as_tibble() %>% 
        summarise(Mean = as.double(mean(.data[[emep_var]], na.rm = T) * evp_list[[emep_var]][['budg_factor']]))
    } else {
      stop('budget_stat can be either "mean" or "sum"')
    }
    
    budget_list[[i]] = budget_list[[i]] %>% 
      mutate(Variable = emep_var,
             Unit = evp_list[[emep_var]][['budg_units']]) %>% 
      select(Variable, everything())
  }
  
  budget_df = bind_rows(budget_list) %>% 
    relocate(Unit, .after = last_col())
  
  budget_df
  
}


collate_obs_mod_nc = function(nc_pth, poll_name_lookup, site_code = 'MY1', i_index = NA_integer_,
                           j_index = NA_integer_, network = 'aurn', pollutant = 'no2') {
  
  #poll_name_lookup needs to be format: obs_name = EMEP_var_name (e.g. no2 = 'SURF_ug_NO2')
  #network is either one of supported networks from Ricardo servers or a full path to the observation file
  
  nc = nc_open(nc_pth)
  
  nc_date = nc.get.time.series(nc) %>%
    as_datetime(tz = 'UTC') %>%
    floor_date(unit = 'hour') %>%
    tibble() %>%
    set_names('date')
  
  nc_year = unique(year(nc_date$date))
  
  var = unname(poll_name_lookup[str_to_lower(pollutant)])
  
  mod_data = ncvar_get(nc,
                       varid = var,
                       start = c(i_index, j_index, 1), count = c(1, 1,-1)) %>%
    as_tibble() %>%
    mutate(code = site_code, .before = value) %>% 
    rename(mod = value) %>%
    bind_cols(nc_date, .)
  if (str_detect(var, 'ppb_O3')) {#convert ozone to ug/m3
    mod_data = mod_data %>%
      mutate(mod = mod * 2)
  }
  
  if (file_exists(network)) {
    #return NULL if pollutant not in the file
    obs_data = tryCatch(
      error = function(cnd) NULL,
      read_csv(network) %>%
        select(date, !!pollutant) %>%
        rename(obs = !!pollutant)
    )
  } else {
    #return NULL if pollutant not measured at auto site on Ricardo servers
    obs_data = tryCatch(
      error = function(cnd) NULL,
      
      get_auto_data(site = site_code, network = network, pollutant = str_to_lower(pollutant),
                    start_year = nc_year, end_year = nc_year, to_narrow = T) %>%
        select(-code) %>% 
        rename(obs = conc)
    )
  }
  
  if (!is.null(obs_data)) {
    both = left_join(mod_data, obs_data, by = 'date') %>%
      #if missing data in observations there will be na values in code and pollutant columns after joining
      #the mutate function makes sure the na values are replaced
      mutate(code = !!site_code,
             pollutant = !!pollutant) %>%
      relocate(mod, .after = obs)
  } else {
    print(paste0('No data for ', pollutant, ' at ', site_code, ' (', network, ')')) #keep track of what's missing
    both = mod_data %>%
      mutate(code = !!site_code,
             pollutant = !!pollutant,
             obs = NA_real_,
             .before = mod)
  }
  
  nc_close(nc)
  both
}

collate_obs_mod_stars = function(site_geo_df, emep_pth, poll_lookup, emep_crs = EMEP_CRS,
                            time_marker = NULL) {
  #site_geo_df MUST be a 1-row sf object (e.g. created by the split function)
  
  if (nrow(site_geo_df) != 1) stop('site_geo_df can only have one row')
  
  print(site_geo_df$code) #to check on progress whilst testing
  
  poll_lookup_rev = names(poll_lookup) %>% 
    set_names(poll_lookup)
  
  if (st_crs(site_geo_df) != emep_crs) {
    site_geo_df = st_transform(site_geo_df, emep_crs)
  }

  #get the site's pollutants for extraction
  emep_vars = extract_nc_vars(emep_pth)
  modelled_vars = base::intersect(poll_lookup, emep_vars)
  
  modelled = read_stars(emep_pth, sub = modelled_vars, proxy = T) %>% 
    st_set_crs(emep_crs) %>% 
    st_extract(site_geo_df) %>% 
    as_tibble() %>% 
    select(-geometry) %>% 
    mutate(across(-time, ~as.double(.x)),
           across(matches('ppb_O3'), ~.x * 2)) %>%  #convert to ug/m3
    pivot_longer(-time, names_to = 'poll', values_to = 'conc') %>%
    mutate(poll = str_c('mod_', poll_lookup_rev[poll])) %>%
    pivot_wider(id_cols = time, names_from = poll, values_from = conc) %>% 
    rename(date = time) %>% 
    mutate(date = floor_date(date, 'hour'))

  #add ox if both o3 and no2 present
  if (all(c('mod_o3', 'mod_no2') %in% names(modelled))) {
    modelled = modelled %>% 
      mutate(mod_ox = mod_o3 + mod_no2)
  }
  
  mod_year = unique(year(modelled$date))
  
  observed = tryCatch(
    error = function(cnd) NULL,
    
    get_auto_data(site = site_geo_df$code, network = site_geo_df$network, pollutant = unname(poll_lookup_rev[modelled_vars]),
                  start_year = mod_year, end_year = mod_year, to_narrow = T) %>%
      mutate(pollutant = str_c('obs_', pollutant)) %>% 
      pivot_wider(id_cols = c(date, code), names_from = pollutant, values_from = conc)
  )
  
  if(!is.null(observed)) {
    if (all(c('obs_o3', 'obs_no2') %in% names(observed))) {
      observed = observed %>% 
        mutate(obs_ox = obs_o3 + obs_no2) 
    }
    
    mod_obs = left_join(modelled, observed, by = 'date') %>% 
      select(date, code, everything())
    return(mod_obs)
    
  } else {
    return(NULL)
  }
  
  # if (save_data == T) {
  #   write_rds(mod_obs, path(data_pth_out, paste0(auto_site_code, '_mod_obs.rds')))
  # }
  # 
  # if (save_plots == T) {
  #   site_time_series_pdf2(auto_site_code, auto_site_meta, merged_df = mod_obs, run_title_info = run_info)
  # }
  # 
}


calculate_emep_diff = function(var, run_labels = c('test', 'ref'),
                               outer_test_fname = NA, outer_ref_fname = NA,
                               inner_test_fname = NA, inner_ref_fname = NA, emep_crs = NA) {
  
  if (is.na(emep_crs)) stop('emep_crs MUST be provided!')
  
  #determine what needs plotting and check if paths are valid
  emep_fnames = c(outer_test_fname, outer_ref_fname, inner_test_fname, inner_ref_fname) %>% 
    set_names()
  if (!all(file_exists(na.omit(emep_fnames)))) stop ('Invalid file path(s)')
  
  plot_mode = case_when(all(!is.na(emep_fnames) == c(T,T,T,T)) ~ 'both_diff',
                        all(!is.na(emep_fnames) == c(T,T,F,F)) ~ 'outer_diff',
                        all(!is.na(emep_fnames) == c(F,F,T,T)) ~ 'inner_diff',
                        TRUE ~ NA_character_
  )
  if (is.na(plot_mode)) stop('invalid combination of paths')
  
  calc_diff_stars = function(stars1, stars2) {
    
    stars_diff = tryCatch(
      error = function(cnd) {
        names(stars1) = run_labels[1]
        names(stars2) = run_labels[2]
        list(stars1, stars2)
      },
      c(stars1, stars2) %>% 
        set_names(run_labels) %>% 
        mutate(abs_diff := !!sym(run_labels[1]) - !!sym(run_labels[2]),
               rel_diff := (!!sym(run_labels[1]) - !!sym(run_labels[2]))/!!sym(run_labels[2]) * 100)
    )
    
    if ('abs_diff' %in% names(stars_diff)) {
      if (all(near(as.numeric(stars_diff$abs_diff), 0))) {
        stars_diff = stars_diff %>% 
          mutate(abs_diff = NA_real_,
                 rel_diff = NA_real_)
      }
      stars_diff = names(stars_diff) %>% 
        map(select, .data = stars_diff)
    }
    stars_diff
  }
  
  #calculate the abs and rel differences between test and reference runs
  if (plot_mode =='both_diff') {
    
    emep_data = load_emep_data(emep_fnames, rep(emep_crs, length(emep_fnames)), vars = var)
    if (is.null(emep_data)) return(NULL)
    
    emep_data = map(emep_data, st_as_stars) %>% 
      map(~set_names(.x, nm = var))
    
    o_t = emep_data[[outer_test_fname]]
    o_r = emep_data[[outer_ref_fname]]
    i_t = emep_data[[inner_test_fname]]
    i_r = emep_data[[inner_ref_fname]]
    
    o_diff = calc_diff_stars(o_t, o_r)
    i_diff = calc_diff_stars(i_t, i_r)
    return(list(o_diff, i_diff) %>% 
             set_names(str_c(var, c('outer', 'inner'), sep = '_')))
  }
  
  if (plot_mode == 'outer_diff') {
    
    emep_data = load_emep_data(na.omit(emep_fnames), rep(emep_crs, length(na.omit(emep_fnames))),
                               vars = var)
    if (is.null(emep_data)) return(NULL)
    emep_data = map(emep_data, st_as_stars) %>% 
      map(~set_names(.x, nm = var))
    
    o_t = emep_data[[outer_test_fname]]
    o_r = emep_data[[outer_ref_fname]]
    o_diff = calc_diff_stars(o_t, o_r)
    return(list(o_diff) %>% 
             set_names(str_c(var, c('outer'), sep = '_')))
    
  }
  
  if (plot_mode == 'inner_diff') {
    
    emep_data = load_emep_data(na.omit(emep_fnames), rep(emep_crs, length(na.omit(emep_fnames))),
                               vars = var)
    if (is.null(emep_data)) return(NULL)
    
    emep_data = map(emep_data, st_as_stars) %>% 
      map(~set_names(.x, nm = var))
    
    i_t = emep_data[[inner_test_fname]]
    i_r = emep_data[[inner_ref_fname]]
    i_diff = calc_diff_stars(i_t, i_r)
    return(list(i_diff) %>% 
             set_names(str_c(var, c('inner'), sep = '_')))
  }
  
}

read_RunLog_emissions = function(RunLog_pth) {
  #pulls emission tables out of RunLog.out file
  
  t_start = str_which(read_lines(RunLog_pth), 'emissions by countries')
  t_end = str_which(read_lines(RunLog_pth), 'road dust emission')
  t_lengths = diff(c(t_start, t_end), 1) - 2
  
  read_RunLog_tables = function(s, n) {
    t_names = names(read_table(RunLog_pth, skip = s, n_max = 0))
    #the following if chunk is needed due to EMEP 4.17 having different RunLog.out formatting
    ##t_data only needed to know the number of columns
    t_data = read_table(RunLog_pth, skip = s + 1, n_max = 0)
    if (length(t_data) == length(t_names) + 1) {
      CC_index = str_which(t_names, 'CC')
      t_names = t_names %>% 
        append('Land', after = CC_index)
    }
    t = read_table(RunLog_pth, skip = s + 1, n_max = n,
                   col_names = t_names, col_types = cols(CC = 'c', Land = 'c', .default = 'd'))
    if ('TOTAL' %in% t$CC) {
      t[t$CC == 'TOTAL', 'Land'] = 'TOT'
      t[t$CC == 'TOTAL', 'CC'] = '999'
    }
    
    if ('EU' %in% t$Land) {
      t[t$Land == 'EU', 'CC'] = '998'
    }
    
    if ('EMTAB' %in% names(t)) t = select(t, -EMTAB)
    
    t = t %>% 
      mutate(CC = parse_integer(CC))
  }
  
  RL_tables = map2_dfr(t_start, t_lengths, read_RunLog_tables) %>% 
    group_by(CC, Land) %>% 
    summarise(across(.fns = ~sum(.x, na.rm = T))) %>% 
    ungroup()
  
  RL_tables
  
}

compare_run_emissions = function(test_pth, ref_pth, save_file = T, mbs_table_fname = NA) {
  # test and ref paths are absolute paths of the emep runs
  #  - they can be either the output nc file or the MassBudgetSummary.txt file
  
  MBS_data = c(test_pth,ref_pth) %>% 
    path_dir() %>% 
    path('MassBudgetSummary.txt') %>% 
    set_names(c('test', 'ref')) %>% 
    map_dfr(read_table, skip = 1, col_names = T, .id = 'run') %>% 
    select(run, species = Spec, emission = emis) %>% 
    pivot_wider(id_cols = species, names_from = run, values_from = emission) %>% 
    mutate(abs_diff = test - ref,
           rel_diff = abs_diff/ref*100)
  if (save_file == T) {
    # - determine output filename based on whether it is an outer or inner domain file
    stopifnot('output file name must be provided' = !is.na(mbs_table_fname))
    out_fname = paste0(extract_domain_from_fpath(test_pth), '_', mbs_table_fname)
    write_lines(paste0('# Test file: ', test_pth), file = path(tables_pth_out, out_fname))
    write_lines(paste0('# Ref file: ', ref_pth), path(tables_pth_out, out_fname), append = T)
    write_lines('# ', path(tables_pth_out, out_fname), append = T)
    write_csv(MBS_data, path(tables_pth_out, out_fname), na = '', col_names = T, append = T)
  }
  MBS_data
}

compare_inv_mod_emissions = function(model_run_pth = TEST_INNER_FNAME,
                                     webdabEMEP_pth = 'webdabEMEPNationalEmissions2000-2019.txt',
                                     save_file = T) {
  
  mod_year = path_file(model_run_pth) %>% 
    str_split('_') %>% 
    flatten_chr() %>% 
    .[length(.) - 1] %>% 
    as.integer()
  
  # - determine output filename based on whether it is an outer or inner domain file
  out_fname = paste0(extract_domain_from_fpath(model_run_pth), '_', INV_MOD_EMISS_TABLE_FNAME)
  
  RL_emiss = c(model_run_pth) %>% 
    path_dir() %>% 
    path('RunLog.out') %>% 
    read_RunLog_emissions() %>% 
    mutate(type = 'model')
  
  inventory = read_delim(webdabEMEP_pth, comment = '#', delim = ';',
                         col_names = c('Land', 'year', 'sector', 'pollutant', 'unit', 'value'),
                         col_types = 'cicccd') %>% 
    filter(year == mod_year) %>% 
    select(Land, pollutant, value) %>% 
    pivot_wider(id_cols = Land, names_from = pollutant, values_from = value) %>% 
    rename(co = CO, nh3 = NH3, voc = NMVOC, nox = NOx, pm25 = PM2.5, pmco = PMcoarse, sox = SOx) %>% 
    mutate(type = 'inventory')
  
  merged = bind_rows(inventory, RL_emiss) %>% 
    group_by(Land) %>% 
    filter(n() > 1) %>% 
    ungroup() %>% 
    pivot_longer(cols = co:sox, names_to = 'pollutant', values_to = 'emiss') %>%
    pivot_wider(id_cols = c(Land, pollutant), names_from = type, values_from = emiss) %>% 
    mutate(abs_diff = model - inventory,
           rel_diff = abs_diff/inventory*100) %>% 
    arrange(Land)
  
  if (save_file == T) {
    write_lines(paste0('# EMEP file: ', model_run_pth), file = path(tables_pth_out, out_fname))
    write_lines('# ', path(tables_pth_out, out_fname), append = T)
    write_csv(merged, path(tables_pth_out, out_fname), na = '', col_names = T, append = T)
  }
  
  merged
  
}

summarise_mobs = function(mobs_lframe, pollutant = 'all', avg_time = 'day',
                             summary_stat = 'mean', data_thresh = 75) {
  
  #mobs_lframe is mobs dataframe in the long format
  mobs = mobs_lframe %>% 
    timeAverage(avg.time = avg_time, data.thresh = data_thresh, type = c('code', 'pollutant')) %>% 
    ungroup() %>% 
    mutate(across(where(is.factor), ~as.character(.x)))
  
  if (pollutant != 'all') {
    mobs = mobs %>% 
      filter(pollutant %in% !!pollutant)
  }
  mobs
  
}

add_ox = function(site_dframe) {
  #adds ox concentrations to the dataframe if both no2 and o3 present
  if (all(c('no2', 'o3') %in% unique(site_dframe$pollutant))) {
    site_dframe = site_dframe %>%
      filter(pollutant %in% c('no2', 'o3')) %>%
      pivot_wider(id_cols = c(date, code), names_from = pollutant, values_from = c(obs, mod)) %>%
      mutate(pollutant = 'ox',
             obs = obs_o3 + obs_no2,
             mod = mod_o3 + mod_no2) %>%
      dplyr::select(date, code, pollutant, obs, mod) %>%
      bind_rows(site_dframe)
  } else {

  }
  site_dframe
}


# PLOTTING FUNCS ----------------------------------------------------------

create_blank_plot = function() {
  blank_plot = ggplot() +
    geom_blank(aes(1,1)) +
    theme(
      plot.background = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      axis.line = element_blank()
    )
  blank_plot
}

plot_comp_maps = function(diff_list, ncl_palette_dir = getwd(), pretty_lab = F) {
  
  var = str_replace(names(diff_list)[1], '_[^_]+$', '')
  #download country borders for plotting
  boundaries = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
    st_cast("MULTILINESTRING") %>%  
    st_transform(st_crs(diff_list[[1]][[1]]))
  
  #breaks and labels for colorbars
  map_breaks = VAR_PARAMS_LIST[[var]][['map_levs']]
  map_labs = as.character(map_breaks)[c(-1,-length(map_breaks))] 
  
  
  diff_breaks = c(-1e6, VAR_PARAMS_LIST[[var]][['map_difflevs']], 1e6)
  diff_labels = sprintf(VAR_PARAMS_LIST[[var]][['map_diffprecision']], diff_breaks)
  diff_labels[seq(1:length(diff_labels)) %% 2 == 1] = ''
  diff_labels = diff_labels[c(-1, -length(diff_breaks))]
  #labels for relative difference - hardcoded to -100, -80,...,0,...,80, 100 percent
  diff_rel_labels = if_else(seq_along(c(-1e6, seq(-100, 100, 10), 1e6)) %% 2 == 1,
                            '', as.character(c(-1e6, seq(-100, 100, 10), 1e6))) %>% 
    .[c(-1, -length(c(-1e6, seq(-100, 100, 10), 1e6)))]
  
  ncl_rainbow = read_color(path(ncl_palette_dir, 'WhiteBlueGreenYellowRed.rgb')) %>%
    get_color(n = length(VAR_PARAMS_LIST[[var]][['map_levs']]))
  
  ncl_rwb = read_color(path(ncl_palette_dir,'NCV_blu_red.rgb')) %>% 
    get_color(n = length(diff_breaks) - 1)
  
  if (pretty_lab == T) {
    fill_labs = c(myquickText(paste0(VAR_PARAMS_LIST[[var]][['pretty_lab']], ' (', VAR_PARAMS_LIST[[var]][['units']], ')')),
                  myquickText(paste0(VAR_PARAMS_LIST[[var]][['pretty_lab']], ' (', VAR_PARAMS_LIST[[var]][['units']], ')')),
                  myquickText(paste0('Delta ', VAR_PARAMS_LIST[[var]][['pretty_lab']], ' (', VAR_PARAMS_LIST[[var]][['units']], ')')),
                  myquickText(paste0('Delta ', VAR_PARAMS_LIST[[var]][['pretty_lab']], ' (%)')))
    top_title = myquickText(VAR_PARAMS_LIST[[var]][['pretty_lab']])
  } else {
    fill_labs = c(paste0(var, ' (', VAR_PARAMS_LIST[[var]][['units']], ')'),
                  paste0(var, ' (', VAR_PARAMS_LIST[[var]][['units']], ')'),
                  paste0('Delta ', var, ' (', VAR_PARAMS_LIST[[var]][['units']], ')'),
                  paste0('Delta ', var, ' (%)'))
    top_title = var
  }
  
  p1_list = map(diff_list, 1)
  p1 = ggplot()
  for (i in seq_along(p1_list)) {
    p1 = p1 +
      geom_stars(data = cut(p1_list[[i]], breaks = VAR_PARAMS_LIST[[var]][['map_levs']]))
  }
  p1 = p1 + 
    geom_sf(data = st_crop(boundaries, p1_list[[1]]) , color = "black", fill = NA, size = 0.07) +
    scale_fill_manual(values = ncl_rainbow, drop = F, 
                      labels = map_labs,
                      guide = guide_coloursteps(title.position = 'top',
                                                title.hjust = 0.5, frame.colour = 'black',
                                                frame.linewidth = 0.1,
                                                barwidth = unit(6.7, 'inches'))) +
    labs(title = names(diff_list[[1]][[1]]),
         fill = fill_labs[1])
  
  p1 = theme_emep_diffmap(p1)
  
  p2_list = map(diff_list, 2)
  p2 = ggplot()
  for (i in seq_along(p2_list)) {
    p2 = p2 +
      geom_stars(data = cut(p2_list[[i]], breaks = VAR_PARAMS_LIST[[var]][['map_levs']]))
  }
  p2 = p2 + 
    geom_sf(data = st_crop(boundaries, p2_list[[1]]) , color = "black", fill = NA, size = 0.07) +
    scale_fill_manual(values = ncl_rainbow, drop = F, 
                      labels = map_labs,
                      guide = guide_coloursteps(title.position = 'top',
                                                title.hjust = 0.5, frame.colour = 'black',
                                                frame.linewidth = 0.1,
                                                barwidth = unit(6.7, 'inches'))) +
    labs(title = names(diff_list[[1]][[2]]),
         fill = fill_labs[2])
  
  p2 = theme_emep_diffmap(p2)
  
  p3_list = map(diff_list, 3) %>% 
    compact()
  if (length(p3_list) > 0) {
    p3 = ggplot()
    for (i in seq_along(p3_list)) {
      p3 = p3 +
        geom_stars(data = cut(p3_list[[i]], breaks = diff_breaks))
    }
    p3 = p3 +
      geom_sf(data = st_crop(boundaries, p3_list[[1]]), color = "black", fill = NA, size = 0.07) + 
      scale_fill_manual(values = ncl_rwb, drop = F, 
                        labels = diff_labels,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(3.2, 'inches'))) +
      labs(title = paste0(names(diff_list[[1]][[1]]), ' - ', names(diff_list[[1]][[2]]), ' (abs)'),
           fill = fill_labs[3])
    
    p3 = theme_emep_diffmap(p3)
  } else {
    p3 = create_blank_plot()
  }
  
  p4_list = map(diff_list, 4) %>% 
    compact()
  if (length(p4_list) > 0) {
    p4 = ggplot()
    for (i in seq_along(p4_list)) {
      p4 = p4 +
        geom_stars(data = cut(p4_list[[i]], breaks = c(-1e6, seq(-100, 100, 10), 1e6)))
    }
    p4 = p4 +
      geom_sf(data = st_crop(boundaries, p4_list[[1]]), color = "black", fill = NA, size = 0.07) + 
      scale_fill_manual(values = ncl_rwb, drop = F, 
                        labels = diff_rel_labels,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(3.2, 'inches'))) +
      labs(title = paste0(names(diff_list[[1]][[1]]), ' - ', names(diff_list[[1]][[2]]), ' (rel)'),
           fill = fill_labs[4])
    
    p4 = theme_emep_diffmap(p4)
  } else {
    p4 = create_blank_plot()
  }
  
  #check if all abs_diff are NA (= test and ref values are identical)
  identical_data = map(p3_list, as_tibble) %>%
    map(drop_na) %>%
    map_dbl(nrow)
  
  if (all(identical_data == 0)) {
    p3 = p3 +
      annotation_custom(grid::roundrectGrob(width = 0.5, height = 0.08)) +
      annotation_custom(grid::textGrob('Data are identical'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme(legend.position = 'none')
    p4 = p4 +
      annotation_custom(grid::roundrectGrob(width = 0.5, height = 0.08)) +
      annotation_custom(grid::textGrob('Data are identical'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme(legend.position = 'none')
  }
  
  pp = ggarrange(ggarrange(p1, p2, ncol = 2,
                           common.legend = T, legend = 'bottom'),
                 ggarrange(p3, p4, ncol = 2, align = 'h'),
                 nrow = 2, align = c('hv')) %>% 
    annotate_figure(top = text_grob(top_title, size = 20, color = '#473C8B'))
  
  pp

}

plot_DSC = function(stars_object, evp_list, pretty_lab = F, display_summary = T) {
  
  if (display_summary == T) {
    budget_summary = calc_budget(stars_object, evp_list) %>% 
      mutate(across(where(is.double), ~round(.x, 3))) %>% 
      pivot_longer(any_of(c('Total', 'Mean')), names_to = 'stat', values_to = 'value') %>%
      drop_na()
  }
  
  country_borders = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
    st_cast("MULTILINESTRING") %>% 
    st_transform(st_crs(stars_object))
  
  DSC_plotlist = vector('list', length(names(stars_object)))
  
  for (i in seq_along(names(stars_object))) {
    emep_var = names(stars_object)[i]
    stars_sub = stars_object %>% 
      select(all_of(c(emep_var, 'Area_Grid_km2')))
    
    #get_colorscale values and labels
    dsc_levs = evp_list[[emep_var]][['dsc_levs']] 
    # if too many levels show every other label
    if(length(dsc_levs) > 14) {
      dsc_labs = dsc_levs
      dsc_labs[seq(1:length(dsc_labs)) %% 2 == 0] = ''
      dsc_labs = dsc_labs[c(-1, -length(dsc_labs))]
    } else {
      dsc_labs = as.character(dsc_levs[c(-1, -length(dsc_levs))])
    }
    ncl_rainbow = read_color(path(PALETTE_DIR, 'WhiteBlueGreenYellowRed.rgb')) %>%
      get_color(n = length(dsc_levs), show = F)
    
    if (pretty_lab == T) {
      title_string = evp_list[[emep_var]][['lab']]
    } else {
      title_string = emep_var
    }
    
    title_string = paste0(title_string,
                          ' (',
                          evp_list[[emep_var]][['budg_units']],
                          ')')
    
    if (display_summary == T) {
      budget_info = filter(budget_summary, Variable == emep_var) 
      
      summary_string = paste0(budget_info$stat, ' = ',
                              budget_info$value, ' ',
                              budget_info$Unit)
    } else {
      summary_string = ''
    }
    
    p1 = ggplot() +
      geom_stars(data = cut(stars_sub, breaks = dsc_levs)) + 
      geom_sf(data = (st_crop(country_borders, stars_sub)),
              color = "black", fill = NA, size = 0.1) +
      scale_fill_manual(values = ncl_rainbow, drop = F, na.value = 'gray90', labels = dsc_labs,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(6.5, 'inches')))
    if (pretty_lab == T) {
      p1 = p1 +
        labs(title = myquickText(title_string),
             subtitle = myquickText(summary_string),
             fill = NULL)
    } else {
      p1 = p1 + 
        labs(title = title_string,
             subtitle = summary_string,
             fill = NULL)
    }
    
    p1 = theme_DSC(p1)
    DSC_plotlist[[i]] = p1    
  }
  DSC_plotlist
}

plot_time_series2 = function(dframe, time_res = 'day', month = NA, dc_threshold = 0.75) {
  #plots time series of observed and modelled concentrations
  #if time_res is 'day' the whole (annual) dataframe is used but only days with data
  #capture >= dc_threshold are plotted
  #if time_res is 'hour', only one month which must be specified by the 'month' kwarg is plotted
  #both abbreviated and full month names are allowed
  
  poll = unique(dframe$pollutant)
  dc_obs = sum(!is.na(dframe$obs)) #calculate observation data capture
  
  if (time_res == 'day') {
    dframe = dframe %>% 
      timeAverage(data.thresh = dc_threshold * 100, type = c('code', 'pollutant')) %>% 
      ungroup()
    date_breaks = '1 month'
    date_labels = '%b'
    plt_name = str_c(unique(dframe$code), poll, 'year', sep = '_')
  } else if (time_res == 'hour' & str_to_title(month) %in% c(month.abb, month.name)) {
    month_num = coalesce(match(month, month.name), match(month, month.abb))
    dframe = dframe %>% 
      filter(month(date) == month_num)
    date_breaks = '2 days'
    date_labels = '%e'
    plt_name = str_c(unique(dframe$code), poll, month.abb[month_num], sep = '_')
  } else {
    stop('Time_res must be either "day" or "hour". If the latter a month name (e.g. "Jan" or "January") must be provided.')
  }
  
  
  df2 = dframe %>% 
    mutate(ymin = 0,
           pollutant = as.character(pollutant)) %>% 
    pivot_longer(obs:mod, names_to = 'scenario', values_to = 'conc')
  
  #for correct labelling of the plot strips (won't accept myquicktext) - accepts named vector
  label_vector = OBS_VAR_PARAMS_LIST[[poll]][['lab_unit']] %>% 
    set_names(poll)
  
  if (dc_obs == 0) {
    p = ggplot()
    
  } else {
    p = ggplot() +
      geom_ribbon(data = filter(df2, scenario == 'obs'),
                  aes(date, ymin = ymin, ymax = conc, fill = scenario), color = NA, alpha = 0.9) +
      scale_fill_manual(values = c(obs = unname(OBS_VAR_PARAMS_LIST[[poll]][['poll_fill']])),
                        labels = c(obs = 'Observed'), guide = guide_legend(override.aes = list(size = 3)))
  }
  
  p = p +
    geom_line(data = filter(df2, scenario == 'mod'),
              aes(date, conc, color = scenario), size = 0.3) +
    scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels, expand = expansion(c(0,0))) +
    scale_y_continuous(labels = function(x) {str_pad(x, width = 4, side = 'left', pad = ' ')},
                       expand = expansion(mult = c(0, 0.15), add = c(0, 0))) +
    scale_color_manual(values = c(mod = unname(OBS_VAR_PARAMS_LIST[[poll]][['poll_col']])),
                       labels = c(mod = 'Modelled'), guide = guide_legend(override.aes = list(size = .75))) +
    labs(x = NULL,
         y = NULL) +
    facet_wrap(~pollutant, strip.position = 'left',
               labeller = as_labeller(label_vector, default = label_parsed)) +
    theme_bw() +
    theme(strip.placement = 'outside',
          strip.background = element_rect(fill = unname(OBS_VAR_PARAMS_LIST[[poll]][['poll_fill']])),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(size = 0.1),
          axis.ticks = element_line(size = 0.1),
          legend.title = element_blank(),
          legend.direction = 'horizontal',
          legend.position = c(0.5, 0.9),
          legend.box = 'horizontal',
          legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
          legend.background = element_rect(fill = 'transparent'),
          legend.key = element_rect(fill = 'transparent'),
          legend.key.height = unit(0.5, 'lines'))
  
  if (dc_obs == 0) {
    p = p +
      labs(caption = '* No observations during the period shown.') +
      theme(legend.position = c(0.4, 0.9), #the horizontal positions needs tweaking
            plot.caption = element_text(size = 8))
    
  } else {
    p = p +
      labs(caption = 'No observations during the period shown.') +
      theme(plot.caption = element_text(color = 'transparent',
                                        size = 8))
  }
  
  p
}


site_time_series_pdf3 = function(merged_df_long, auto_sites_df, merged_df_pth = NA, out_dir = '.',
                                 plot_all_polls = T, ppp = 4, run_title_info = '') {
  #outputs pdf for a site
  #ppp = plots per page
  
  
  #check either a dataframe or a path to it is given
  if (all(is.na(c(merged_df_long, merged_df_pth)))) {
    print(paste0('No dataframe or a path to it was given for ', site_code))
    return(NULL)
  }
  if (!is.na(merged_df_pth)) {
    merged_df_long = read_rds(merged_df_pth) %>% 
      mobs_to_long()
  }
  
  site_meta = auto_sites_df %>% 
    filter(code == unique(merged_df_long$code))
  
  fname_out = str_c(str_replace_all(site_meta$site, "[^[:alnum:]]", ""), site_meta$code,
                    site_meta$site_type_grp, site_meta$year, sep = '_')
  #if only measured vars plotted add _p suffix to file name
  if (plot_all_polls == F) {
    fname_out = paste0(fname_out, '_p')
  }
  
  plot_pth_out = path(out_dir, fname_out, ext = 'pdf')
  
  page_daily_title = str_c('Daily concentrations at ', site_meta$site, ' site (',
                           site_meta$site_type_grp, ')\n in ', site_meta$year, sep = '')
  
  page_hourly_title = map_chr(str_c(' ', month.name, ' '), ~str_c('Hourly concentrations at ', site_meta$site, ' site (',
                                                                  site_meta$site_type_grp, ')\n in', .x, site_meta$year, sep = ''))

  #if run_title_info provided, add it to the page titles
  page_daily_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_daily_title)
  page_hourly_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_hourly_title)
  
  if (plot_all_polls == T) {
    n_polls = n_distinct(merged_df_long$pollutant)
    page_titles = rep(c(page_daily_title, page_hourly_title),
                      each = n_polls %/% ppp + 1)
    
    #sort pollutants so that they are plotted in order of polls in OBS_VAR_PARAMS_LIST 
    plot_polls = unique(merged_df_long$pollutant)
    sorted_plot_polls = plot_polls[order(match(plot_polls, names(OBS_VAR_PARAMS_LIST)))]
    base_poll_vector = c(sorted_plot_polls, rep(NA_character_, times = ppp * (n_polls %/% ppp + 1) - n_polls))
    
  } else {
    measured_polls = merged_df_long %>% 
      drop_na() %>% 
      distinct(pollutant) %>% 
      pull()
    n_polls = length(measured_polls)
    
    page_titles = rep(c(page_daily_title, page_hourly_title),
                      each = n_polls %/% ppp + 1)
    sorted_measured_polls = measured_polls[order(match(measured_polls, names(OBS_VAR_PARAMS_LIST)))]
    base_poll_vector = c(sorted_measured_polls, rep(NA_character_, times = ppp * (n_polls %/% ppp + 1) - n_polls))
    
  }
  
  poll_vector = rep(base_poll_vector, times = length(unique(page_titles)))
  averaging_vector = rep(c('day', rep('hour', 12)), each = length(base_poll_vector))
  month_vector = rep(c(NA_character_, month.abb), each = length(base_poll_vector))
  
  blank_plot = create_blank_plot() #to fill in gaps on page
  
  plot_list = vector('list', length(poll_vector))
  
  for (j in 1:length(poll_vector)) {
    if (is.na(poll_vector[j])) {
      plot_list[[j]] = blank_plot
    } else {
      merged_data_sub = merged_df_long%>% 
        filter(pollutant == !!poll_vector[j])
      p = plot_time_series2(dframe = merged_data_sub, time_res = averaging_vector[j],
                            month = month_vector[j])
      plot_list[[j]] = p
    }
    
  }
  
  export = marrangeGrob(grobs = plot_list, nrow = ppp, ncol = 1, top = substitute(page_titles[g]))
  ggsave(filename = plot_pth_out, export, paper = 'a4', height = 10, width = 7)
}


plot_annual_scatter = function(mobs_list, site_meta_df, poll_name_lookup, pollutants = 'all',
                               mobs_pths = NA) {
  #plots modelled vs observed scatter plots for data obtained by collate_mod_obs function
  # mobs_list is a list of mod_obs dataframes
  #can be used either within the collate_mod_obs function (if mobs_list is provided) or
  #on its outputted files (set mobs_list to NA and provide a vector of paths to the saved files) 
  
  if('sf' %in% class(site_meta_df)) site_meta_df = st_drop_geometry(site_meta_df)
  
  if(all(is.na(mobs_list))) {
    if(all(!is.na(mobs_pths))) {
      
      mobs_list = map(mobs_pths, read_rds) %>%
        map(mobs_to_long)

    } else {
      stop('Either mobs_list or mobs_pths must be given')
    }
  }

  mobs_means = future_map_dfr(mobs_list, summarise_mobs, avg_time = 'year') %>% 
    left_join(site_meta_df, by = 'code') %>% 
    mutate(site_type_grp = factor(site_type_grp, levels = c('Road', 'Urban', 'Industrial', 'Rural'), ordered = T))

  
  #calculate max concentrations to determine x and y upper limits for plotting
  mobs_max = mobs_means %>% 
    group_by(pollutant) %>% 
    pivot_longer(cols = c(mod, obs), names_to = 'scenario', values_to = 'conc') %>% 
    summarise(max_conc = max(conc, na.rm = T)) %>% 
    ungroup() 
  
  if(pollutants != 'all') {
    mobs_max = mobs_max %>% 
      filter(pollutant %in% pollutants)
  }
  
  plots = vector('list', nrow(mobs_max))
  
  for (i in seq_along(mobs_max$pollutant)) {
    
    poll = mobs_max$pollutant[i]
    
    #get mex obs or mod concentrations to getermine x and y limits
    max_value = filter(mobs_max, pollutant == !!poll) %>% 
      pull()
    
    mobs_means_sub = mobs_means %>% 
      filter(pollutant == !!poll)
    
    p1 = ggplot(mobs_means_sub) +
      geom_abline(slope = 0.5, intercept = 0, linetype = 'longdash', size = 0.3) +
      geom_abline(slope = 2, intercept = 0, linetype = 'longdash', size = 0.3) +
      geom_abline(slope = 1, intercept = 0, linetype = 'solid', size = 0.3) +
      geom_point(aes(obs, mod, shape = site_type_grp, color = site_type_grp),alpha = 0.8, size = 2) +
      scale_x_continuous(limits = c(0, max_value * 1.02)) +
      scale_y_continuous(limits = c(0, max_value * 1.02)) +
      scale_shape_manual(values = c(Road = 1, Urban = 2, Industrial = 4, Rural = 0)) +
      scale_color_manual(values = c(Road = '#e7298a', Urban = '#7570b3', Industrial = '#d95f02', Rural = '#1b9e77'),
                         guide = guide_legend(override.aes = list(alpha = 1, size = 3))) +
      facet_wrap(~site_type_grp, ncol = 4) +
      labs(title = parse(text = paste0('Annual~Mean~', OBS_VAR_PARAMS_LIST[[poll]][['lab_unit']])),
           x = myquickText(paste0('Observed')),
           y = myquickText(paste0('Modelled')), 
           shape = 'Site Type',
           color = 'Site Type') +
      
      theme_bw() +
      theme(strip.background = element_rect(fill = unname(OBS_VAR_PARAMS_LIST[[poll]][['poll_fill']])),
            strip.text = element_text(size = 9),
            aspect.ratio = 1,
            axis.text = element_text(size = 8),
            axis.title = element_text(size = 9),
            legend.position = 'none',
            legend.text = element_text(size = 9),
            legend.title = element_text(size = 10),
            panel.grid.major = element_line(size = 0.02),
            panel.grid.minor = element_line(size = 0.01),
            plot.title = element_text(hjust = 0.5, size = 11))# color = '#473C8B'))
    
    plots[[i]] = p1
  }
  names(plots) = mobs_max$pollutant
  plots
}


plot_budget_diff = function(budget_dframe, threshold = 5) {
  #plots difference in budget vars between two EMEP runs
  # data must be given in a wide dataframe

  budget = budget_dframe %>% 
    filter(abs_diff != 0) %>% 
    select(-test, -ref)
  
  budget2 = budget %>% 
    pivot_longer(cols = -Variable:-Unit, names_to = 'diff', values_to = 'value') %>% 
    mutate(diff = str_replace(diff, '_diff', '')) 

  
  plot_list = vector('list', 2)
  n = vector('integer', 2)
  budget_stats = c('Total', 'Mean')
  for (i in seq_along(budget_stats)) {
    budget2_sub = budget2 %>% 
      filter(Stat == budget_stats[i])
    n[[i]] = nrow(budget2_sub)
    
    if (budget_stats[i] == 'Total') {
      budget_strip = c(abs = paste0('"Test - Reference"', ' ~(Gg~yr^-1)'), rel = paste0('"Test - Reference (%)"'))
    } else {
      budget_strip = c(abs = paste0('"Test - Reference"', '~(mu*g~m^-3)'), rel = paste0('"Test - Reference (%)"'))
    }
    
    p1 = ggplot(budget2_sub) +
      geom_hline(yintercept = 0, color = 'black', size = 0.2) +
      geom_bar(aes(Variable, value), fill = '#d8b365', color = 'black', size = 0.1,
               stat = 'identity', position = 'dodge', width = 0.8) +
      geom_rect(data = (filter(budget2_sub, diff == 'rel') %>% slice(1)),
                aes(xmin = -Inf, xmax = Inf, ymin = -threshold, ymax = threshold), fill = 'red', alpha = 0.2) +
      facet_wrap(~diff, scales = 'free_x', labeller = as_labeller(budget_strip, default = label_parsed)) +
      labs(x = NULL,
           y = NULL) + 
      coord_flip() +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'gray50'),
            strip.text = element_text(size = 10, color = 'black', face = 'bold'),
            panel.grid.major = element_line(size = 0.1),
            axis.text = element_text(size = 8),
            legend.position = 'none')
    
    plot_list[[i]] = p1
  }
  
  # attempt to make bars similar width
  n_ratio = n[1]/n[2] 
  n_factor = case_when(n_ratio <= 0.4 ~ 1.25,
                       n_ratio <= 0.7 ~ 1.1,
                       n_ratio <= 0.95 ~ 1.05,
                       n_ratio <= 1.05 ~ 1,
                       n_ratio <= 1.3 ~ 0.9)
  
  budget_plot = ggarrange(plotlist = plot_list, nrow = 2, heights = c(n[1] * n_factor, n[2]),
                          align = 'v')
  
  budget_plot
}


plot_MBS_diff = function(MBS_dframe, MBS_table_pth = NULL, threshold = 5) {
  #plots difference in MassBudgetSummary between two EMEP runs
  # data must be given either as a dataframe output from 'compare_run_emission' function OR
  # a path where the dataframe is saved
  
  if (!any(class(MBS_dframe) == 'data.frame')) {
    MBS_dframe = read_csv(MBS_table_pth, skip = 3)
  }
  
  MBS = MBS_dframe %>% 
    filter(abs_diff != 0) %>% 
    select(-test, -ref)
  
  species_order = MBS$species #to maintain the species order in the MBS file
  
  MBS2 = MBS %>% 
    pivot_longer(cols = -species, names_to = 'diff', values_to = 'value') %>% 
    mutate(diff = str_replace(diff, '_diff', ''),
           species = factor(species, levels = rev(species_order), ordered = T))
  
  MBS2_strip = c(abs = 'Test - Reference (kg)', rel = 'Test - Reference (%)')
  
  if(nrow(MBS2) == 0) { #emissions in both runs are identical
    p1 = ggplot(MBS2, aes(species, value)) +
      annotation_custom(grid::textGrob('Emission data are identical'),
                        xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      labs(x = NULL,
           y = NULL) +
      coord_flip() +
      theme_bw()
  } else {
    p1 = ggplot(MBS2) +
      geom_hline(yintercept = 0, color = 'black', size = 0.2) +
      geom_bar(aes(species, value), fill = '#5ab4ac', color = 'black', size = 0.1,
               stat = 'identity', position = 'dodge', width = 0.8) +
      geom_rect(data = (filter(MBS2, diff == 'rel') %>% slice(1)),
                aes(xmin = -Inf, xmax = Inf, ymin = -threshold, ymax = threshold), fill = 'red', alpha = 0.2) +
      facet_wrap(~diff, scales = 'free_x', labeller = as_labeller(MBS2_strip)) +
      labs(x = NULL,
           y = NULL) + 
      coord_flip() +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'gray50'),
            strip.text = element_text(size = 10, color = 'black', face = 'bold'),
            panel.grid.major = element_line(size = 0.1),
            axis.text = element_text(size = 8))
  }
  
  p1
}


plot_emiss_diff = function(emiss_dframe, emiss_table_pth = NULL, threshold = 5) {
  #plots difference in emissions between EMEP/NAEI inventory and the model emissions in RunLog.out
  # data must be given either as a dataframe output from 'compare_inv_mod_emission' function OR
  # the path where the dataframe is saved
  
  emiss_bar_fills = c(nox = '#b3cde3',
                      nh3 = '#fbb4ae',
                      co = '#ccebc5',
                      voc = '#8dd3c7',
                      pm25 = '#fed9a6',
                      pmco = '#e5d8bd',
                      sox = '#fddaec')
  
  if (!any(class(emiss_dframe) == 'data.frame')) {
    emiss_dframe = read_csv(emiss_table_pth, skip = 2)
  }
  
  emiss = emiss_dframe %>% 
    select(-inventory, -model)
  
  Land_order = unique(emiss$Land) #to maintain the Land order in the emiss file
  
  emiss = emiss %>% 
    pivot_longer(cols = c(-Land, -pollutant), names_to = 'diff', values_to = 'value') %>% 
    mutate(diff = str_replace(diff, '_diff', ''),
           Land = factor(Land, levels = rev(Land_order), ordered = T))
  
  emiss_strip = c(abs = 'Model - Inventory (Gg)', rel = 'Model - Inventory (%)')
  
  polls = unique(emiss$pollutant)
  emiss_plots = vector('list', length(polls))
  
  for (i in seq_along(polls)) {
    
    emiss_sub = emiss %>% 
      filter(pollutant == polls[[i]])
    
    p1 = ggplot(emiss_sub) +
      geom_hline(yintercept = 0, color = 'black', size = 0.2) +
      geom_bar(aes(Land, value), fill = emiss_bar_fills[polls[[i]]], color = 'black', size = 0.1,
               stat = 'identity', position = 'dodge', width = 0.8) +
      geom_rect(data = (filter(emiss_sub, diff == 'rel') %>% slice(1)),
                aes(xmin = -Inf, xmax = Inf, ymin = -threshold, ymax = threshold), fill = 'red', alpha = 0.2) +
      facet_wrap(~diff, scales = 'free_x', labeller = as_labeller(emiss_strip)) +
      labs(x = NULL,
           y = NULL, 
           title = myquickText(paste0(polls[[i]], ' Emissions Difference'))) + 
      coord_flip() +
      theme_bw() +
      theme(strip.background = element_rect(fill = 'gray80'),
            strip.text = element_text(size = 10, color = 'black'),
            panel.grid.major = element_line(size = 0.1),
            axis.text = element_text(size = 8),
            plot.title = element_text(size = 12, hjust = 0.5, face = 'bold'))
    emiss_plots[[i]] = p1
    names(emiss_plots)[i] = polls[[i]]
    
  }
  
  emiss_plots
  
}

plot_blank = function() {
  
  p1 = ggplot() +
    geom_blank(aes(1,1)) +
    theme(
      plot.background = element_blank(), 
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      axis.line = element_blank()
      )
  p1
} 

format_file_size_tbl = function(file_size_df) {
  gt_tbl = gt(file_size_df) %>% 
    #tab_header(title = md('**File Size Difference**')) %>% 
    tab_style(style = list(cell_text(color = 'red')),
              locations = cells_body(columns = rel_diff,
                                     rows = abs(rel_diff) > 5)) %>% 
    fmt_missing(columns = everything()) %>% 
    cols_label(fname = 'File',
               test = 'Test Run',
               ref = 'Reference Run',
               abs_diff = 'Size difference (B)',
               rel_diff = 'Size difference (%)') %>% 
    cols_align(align = 'right',
               columns = -fname)
  gt_tbl
}

# THEME FUNCS -------------------------------------------------------------

theme_emep_diffmap = function(emep_plot) {
  emep_plot = emep_plot +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.key.height = unit(3, 'mm'),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          panel.background = element_rect(fill = 'white', color = NA),
          panel.border = element_rect(color = 'black', size = 0.3, fill = NA),
          plot.title = element_text(size = 10, hjust = 0.5, face = 'bold'),
          plot.margin = margin(8, 5.5, 5.5, 5.5, unit = 'pt'))
}

theme_DSC = function(emep_plot) {
  emep_plot = emep_plot + 
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) + 
    theme_void() +
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal',
          legend.key.height = unit(3, 'mm'),
          #legend.key.width = unit(dev.size()[1] / 4.5, "inches"),
          plot.title = element_text(hjust = 0.5, size = 20, color = '#473C8B'),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          panel.background = element_rect(fill = 'gray90', color = 'black'),
          panel.border = element_rect(color = 'black', size = 0.3, fill = NA)
    )
}

theme_site_map = function(map_plot) {
  map_plot = map_plot +
    scale_y_continuous(expand = c(0,0)) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_shape_manual(values = c(Road = 1, Urban = 2, Industrial = 4, Rural = 0)) +
    scale_color_manual(values = c(Road = '#e7298a', Urban = '#7570b3', Industrial = '#d95f02', Rural = '#1b9e77')) +
    labs(color = 'Site Type',
         shape = 'Site Type') +
    theme(panel.background = element_rect(fill = '#deebf7', color = 'black'),
          panel.border = element_rect(fill = NA, color = 'black'),
          panel.grid.major = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.99, 0.99),
          legend.justification = c(1, 1),
          legend.background = element_rect(fill = 'transparent'),
          legend.key = element_rect(fill = '#deebf7'))
}


# COLORSCALE FUNCS --------------------------------------------------------

#from rcolors package
read_color <- function(file) {
  tryCatch({
    d = read.table(file, skip = 1) %>% set_names(c("r", "g", "b"))
    max_value = max(sapply(d, max))
    max_value = ifelse(max_value <= 1, 1, 255)
    # pmax(d$r, d$g, d$b) %>% print()
    colors = with(d, rgb(r, g, b, maxColorValue = max_value))
    colors
  }, error = function(e){
    # print(file)
    message(sprintf("[%s]: %s", basename(file), e$message))
  })
  # colors
}

#from rcolors package
get_color <- function(col, n = NULL, show = FALSE) {
  cols = if (length(col) > 1) col else rcolors::rcolors[[col]]
  if (is.null(n)) n = length(cols)
  
  cols = colorRampPalette(cols)(n)
  if (show) show_col(cols)
  cols
}


# MISC --------------------------------------------------------------------

extract_domain_from_fpath = function(fpath) {
  v = path_split(fpath) %>% 
    flatten_chr()
  domain = v[length(v) - 1]
  domain
}

strip_time_res = function(emep_pth, return_full_pth = F) {
  if (return_full_pth == T) {
    s = emep_pth %>% 
      str_replace('_[^_]+$', '')
  } else {
    s = path_file(emep_pth) %>% 
      str_replace('_[^_]+$', '')
  }
  s
}

extract_nc_vars = function(emep_file) {
  
  nc = nc_open(emep_file)
  nc_vars = nc.get.variable.list(nc)
  nc_vars = nc_vars[!nc_vars %in% c('i_EMEP', 'j_EMEP', 'hyam', 'hybm', 'hyai', 'hybi')]  
  nc_close(nc)
  nc_vars
}

select_vars = function(vars = 'all', var_params_list, param) {
  
  if (all(vars == 'all')) {
    vars_out = map(var_params_list, param) %>% 
      compact() %>% 
      names()
  } else {
    vars_out = map(var_params_list, param) %>% 
      compact() %>% 
      names() %>% 
      base::intersect(vars)
  }
  
  if (param == 'budg_factor') {
    vars_out = c(vars_out, 'Area_Grid_km2') %>% 
      unique(.)
  }
  vars_out
}

categorise_emep_vars = function(emep_vars) {
  #takes a vector of emep_vars, splits them into 'emissions', 'surface concentrations',
  #'deposition' and 'misc' categories and puts them in a tibble
  emiss_vars = str_subset(emep_vars, '^Emis')
  surf_vars = str_subset(emep_vars, '^SURF')
  dep_vars = str_subset(emep_vars, '^(W|D)DEP')
  misc_vars = base::setdiff(emep_vars, c(emiss_vars, surf_vars, dep_vars))
  
  emep_var_table = tibble(emiss_vars = list(sort(emiss_vars)),
                          surf_vars = list(sort(surf_vars)),
                          dep_vars = list(sort(dep_vars)),
                          misc_vars = list(sort(misc_vars))) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = map(value, `length<-`, max(lengths(value)))) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    unnest(everything())
  emep_var_table
}


get_emep_floordate = function(emep_fname) {
  
  emep_fname_t_res = path_ext_remove(emep_fname) %>% 
    str_extract('_[^_]+$') %>% 
    str_sub(2)
  
  datetime_unit = case_when(emep_fname_t_res == 'fullrun' ~ 'year',
                            emep_fname_t_res == 'month' ~ 'month',
                            emep_fname_t_res == 'day' ~ 'day',
                            emep_fname_t_res == 'hour' ~ 'hour',
                            TRUE ~ NA_character_)
  nc = nc_open(emep_fname)
  nc_date = nc.get.time.series(nc) %>% 
    as_datetime(tz = 'UTC') %>% 
    floor_date(unit = datetime_unit) %>% 
    tibble() %>% 
    set_names('date')
  nc_close(nc)
  
  nc_date
}

get_sites_in_domain = function(site_geo, emep_2d_slice) {
  site_geo = site_geo %>% 
    st_transform(st_crs(emep_2d_slice))
  sites_in = st_extract(emep_2d_slice, site_geo) %>% 
    as_tibble() %>% 
    mutate(code = site_geo$code) %>% 
    drop_na() %>% 
    pull(code)
  sites_in
}

get_emep_indexes = function(longitude, latitude, emep_pth) {
  #determines the grid x and y indexes of a point given the point longitude and a
  #matrix of longitudes in the NetCDF
  #!!! at present doesn't handle points outside of the modelled domain
  #so it's necessary to check prior using this function
  nc = nc_open(emep_pth)
  nc_lon = ncvar_get(nc, "lon")
  nc_lat = ncvar_get(nc, "lat")
  nc_close(nc)
  
  dist_lat = abs(nc_lat - latitude)
  dist_lon = abs(nc_lon - longitude)
  
  if(length(dim(nc_lon)) == 2){
    dist = sqrt(dist_lat ^ 2 + dist_lon ^ 2)
    index = which(dist == min(dist), arr.ind = TRUE)
    return(as_tibble(index))
  } else {
    return(tibble(row = which(dist_lon == min(dist_lon)),
                  col = which(dist_lat == min(dist_lat))))
  }
}

apply_area_mask = function(stars_object, area_mask = NULL) {
  if (!is.null(area_mask)) {
    stars_object = stars_object[st_transform(area_mask, st_crs(stars_object))]
  }
  stars_object
}

recode_site_type = function(site_type) {
  site_type_grp = case_when({{site_type}} %in% c("Urban Traffic","roadside","Roadside",
                                         "Kerbside","kerbside") == T ~ 'Road',
                        {{site_type}} %in% c("Urban Background","Suburban Background", "urban",
                                         "urban_background","suburban","urban_centre","Suburban") == T ~ 'Urban',
                        #site_type %in% c('airport', 'Airport') == T ~ 'Airport',
                        {{site_type}} %in% c('rural', 'Rural Background') == T ~ 'Rural',
                        {{site_type}} %in% c('Suburban Industrial', 'Urban Industrial', 'urban_industrial', 'airport', 'Airport',
                                         'Rural Industrial', 'Industrial') == T ~ 'Industrial')
  site_type_grp
}

mobs_to_long = function(mobs_dframe) {
  #converts wide format of mobs_dframe to long format
  mobs_long = mobs_dframe %>% 
    pivot_longer(c(-date, -code), names_to = 'pollutant', values_to = 'conc') %>% 
    separate(pollutant, into = c('scenario', 'pollutant'), sep = '_') %>% 
    pivot_wider(c(date, code, pollutant), names_from = scenario, values_from = conc)
  mobs_long
}

format_maps_page_title = function(outer_test_pth = NA, outer_ref_pth = NA,
                                  inner_test_pth = NA, inner_ref_pth = NA,
                                  run_labels = c('test_label', 'ref_label')) {
  
  ### outputs EMEP run (and domain) name for page titles in pdf outputs
  
  # use just the filename portion of the path
  runs = c(outer_test_pth, outer_ref_pth, inner_test_pth, inner_ref_pth)
  
  if (length(na.omit(runs)) == 0) {
    stop('No EMEP file path provided')
  }
  if (length(na.omit(runs)) == 4) {
    domains = runs %>% 
      map_chr(~str_sub(.x, end = -4) %>% 
                str_split('(emiss\\d{4}_)|(_\\d{4}_)') %>% 
                flatten_chr() %>% 
                .[2]) 

    test_title = outer_test_pth %>% 
      str_sub(end = -4) %>% 
      str_replace(paste0('_', domains[1], '_'), paste0('_', domains[1], '(+', domains[3], ')' , '_')) %>% 
      str_replace('_[^_]+$', '') #strip everything after the last underscore from filename
    ref_title = outer_ref_pth %>% 
      str_sub(end = -4) %>% 
      str_replace(paste0('_', domains[2], '_'), paste0('_', domains[2], '(+', domains[4], ')' , '_')) %>% 
      str_replace('_[^_]+$', '')
    
  } else {
    test_title = coalesce(outer_test_pth, inner_test_pth) %>% 
      str_replace('_[^_]+$', '') #strip everything after the last underscore from filename
    ref_title = coalesce(outer_ref_pth, inner_ref_pth) %>% 
      str_replace('_[^_]+$', '')
    
  }
  if (is.na(ref_title)) {
    pg_title = str_c(str_wrap(str_c(run_labels[1], ': ', path_file(test_title)), 50), '\n')
  } else if(is.na(test_title)) {
    pg_title = str_c(str_wrap(str_c(run_labels[1], ': ', path_file(ref_title)), 50), '\n')
  } else {
    pg_title = str_c(str_wrap(str_c(run_labels[1], ': ', path_file(test_title)), 50),
                     '\n\n',
                     str_wrap(str_c(run_labels[2],': ', path_file(ref_title)), 50),
                     '\n') 
  }
  pg_title
}
