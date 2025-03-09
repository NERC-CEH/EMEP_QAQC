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

get_isd_sites = function(year, CC_file = NULL) {
  assert_choice(year, choices = 1901:2023)
  isd_sites = read_html(str_c('https://www.ncei.noaa.gov/data/global-hourly/access/', year)) %>% 
    html_elements('a') %>% 
    html_text() %>% 
    str_subset('\\.csv') %>% 
    path_ext_remove()
  
  if (!is.null(CC_file)) {
    assert_file_exists(CC_file)
    country_codes = read_rds(CC_file)
  } else {
    f0 = 'https://www1.ncdc.noaa.gov/pub/data/noaa/country-list.txt'
    country_codes =  read_csv(f0) %>%
      set_names('dummy') %>%
      separate_wider_delim(cols = dummy, delim = rep('          '),
                           names = c('country_code', 'country_name')) %>%
      distinct()
  }
  
  sites_meta = getMeta(plot = F, end.year = 'all') %>%
    mutate(code2 = str_remove(code, '-'))
  sites_avail = sites_meta %>%
    filter(code2 %in% isd_sites) %>%
    select(-code2) %>%
    left_join(country_codes, by = c('ctry' = 'country_code')) %>%
    rename(country_code = ctry) %>%
    relocate(country_name, .before = country_code)
  
  sites_avail
}

sample_sites = function(meta_df, type = NULL, selector = NULL, n = NULL) {
  #samples sites from a tibble with site meta data
  #type  - sf, random, stratified, manual, subset, exclude 
  
  #selector arg is dependent on type arg
  #if type is 'sf' selector must be either an sf polygon(s) or a path to a geospatial file containing such polygons
  #if type is 'random' selector is redundant but n (number of sites) must be given
  #if type is 'stratified' selector is a name of a grouping column in meta_df and n the number of sites
  #if type is 'subset' selector is either 'airport' (extracts airports) or a subsetting expression -e.g. expression(latitude > 50)
  #if type is 'manual' selector is either a vector of site codes or a path to an rds file with a dataframe which has a column 'code'
  #if type is 'exclude' selector is either a vector of site codes or a path to an rds file with a dataframe which has a column 'code'

  assert_choice(type, c('sf', 'random', 'stratified', 'subset', 'manual', 'exclude'), null.ok = T)
  
  if (is.null(type)) {
    return(meta_df)
  }
  
  if ('sf' %in% type) {
    assert(check_class(selector, 'sf'),
           check_file_exists(selector))
    assert_choice('code', names(meta_df))
    meta_df2 = meta_df %>% 
      st_as_sf(coords = c('longitude', 'latitude'), crs = 4326)
    
    
    if(file_exists(selector)) {
      selector = st_read(selector)
    }
    
    if (st_crs(selector) != st_crs(meta_df2)) {
      selector = selector %>% 
        st_transform(crs = st_crs(meta_df2))
    }
    meta_df2 = meta_df2 %>% 
      st_filter(selector) %>% 
      st_drop_geometry()
    meta_df = meta_df %>% 
      semi_join(select(meta_df2, code))
  } else if ('random' %in% type) {
    assert_count(n, positive = T)
    meta_df = meta_df %>% 
      slice_sample(n = n)
  } else if ('stratified' %in% type) {
    assert_choice(selector, names(meta_df))
    n_groups = n_distinct(meta_df[[selector]])
    
    meta_df = meta_df %>% 
      slice_sample(n = ceiling(n/n_groups), by = one_of(selector)) %>% 
      slice_sample(n = n)
  } else if ('manual' %in% type) {
    assert(check_choice('code', names(meta_df)),
           check_file_exists(selector),
           check_character(selector))
    if (file_exists(selector)) {
      selector = read_rds(selector) %>% 
        pull(code)
    }
    meta_df = meta_df %>% 
      filter('code' %in% selector)
  } else if ('exclude' %in% type) {
    assert(check_choice('code', names(meta_df)),
           check_file_exists(selector),
           check_character(selector))
    if (file_exists(selector)) {
      selector = read_rds(selector) %>% 
        pull(code)
    }
    meta_df = meta_df %>% 
      filter(!'code' %in% selector)
  } else if ('subset' %in% type) {
    if (is.expression(selector)) {
      meta_df = meta_df %>% 
        filter(eval(selector))
    } else if('airport' %in% selector) {
      meta_df = meta_df %>% 
        filter(!is.na(call))
    } else {
    }
  }
  
  meta_df
}

extract_wrf_var_point = function(wrf_file_pth, wrf_var, code, xr_index, index_level = NULL) {
  
  wrf_file = ncpy$Dataset(wrf_file_pth)
  
  #get datetime from file
  date = wrf_file %>%
    wrf$extract_times(timeidx = wrf$ALL_TIMES)
  
  #extract the wrf variable
  if (wrf_var == 'ws') {
    value = wrf_file %>%
      wrf$getvar('uvmet10_wspd_wdir', timeidx = wrf$ALL_TIMES)
    value = value$sel(south_north = xr_index[1], west_east = xr_index[0], wspd_wdir = 'wspd')
  } else if (wrf_var == 'wd') {
    value = wrf_file %>%
      wrf$getvar('uvmet10_wspd_wdir', timeidx = wrf$ALL_TIMES)
    value = value$sel(south_north = xr_index[1], west_east = xr_index[0], wspd_wdir = 'wdir')
  } else {
    value = wrf_file %>% 
      wrf$getvar(wrf_var, timeidx = wrf$ALL_TIMES)
    #value = value$sel(south_north = xr_index[1], west_east = xr_index[0])
    
    # check if the variable has a vertical dimension
    dims = value$dims
    
    # index_level must be integer
    index_level = as.integer(index_level)
    if ('bottom_top' %in% dims) {
      if (!is.null(index_level)) {
        value = value$isel(bottom_top = index_level)
      } else {
        stop('The variable has a vertical coordinate. Please specify a numeric level.')
      }
    } else if ('bottom_top_stag' %in% dims) {
      if (!is.null(index_level)) {
        value = value$isel(bottom_top_stag = index_level)
      } else {
        stop('The variable has a staggered vertical coordinate. Please specify a numeric level.')
      }
    } else if ('low_mid_high' %in% dims) {
      if (!is.null(index_level)) {
        value = value$isel(low_mid_high = index_level)
      } else {
        stop("The variable has named vertical levels. Please specify 'low', 'mid', or 'high'.")
      }
    }
    
    value = value$sel(south_north = xr_index[1], west_east = xr_index[0])
  }
  
  value = value %>% 
    wrf$to_np()
  
  #convert air temp to degC
  if(wrf_var == 'T2') {
    value = value - 273.15
  }
  #in case just one datetime redimension the vector to matrix
  if(length(date) == 1) {
    value = t(value)
  }
  #in case data for just one site 
  if (length(code) == 1){
    value = t(t(value))
  }
  #combine date, wrf_var and extracted data into a long tibble
  value = value %>% 
    as_tibble(.name_repair = 'minimal') %>%
    set_names(code) 
  wrf_var = rep(wrf_var, length(date))
  
  dframe = tibble(date, wrf_var, value) %>% 
    pivot_longer(cols = -c(date, wrf_var), names_to = 'code', values_to = 'value')
}

calculate_wrf_precip = function(wrf_frame) {
  #calculates hourly precip in mm from RAINC and RAINCC
  wrf_frame = wrf_frame %>% 
    pivot_wider(id_cols = c(date, code), names_from = wrf_var, values_from = value) %>% 
    mutate(precip = RAINNC + RAINC,
           precip = precip -lag(precip)) %>% 
    select(-RAINNC, -RAINC) %>% 
    pivot_longer(cols = c(-date, -code), names_to = 'var', values_to = 'value')
}

compare_file_size = function(test_dir, ref_dir) {
  
  ###tests if domains of test and ref match - temporarily disabled because of Tomas's uEMEP directory
  # domain = map_chr(c(test_pth, ref_pth), extract_domain_from_fpath)
  # if(length(unique(domain)) != 1) {
  #   stop('Comparing EMEP outputs in two different domains')
  # }
  if (any(is.na(c(test_dir, ref_dir)))) return(NULL)
  d_content = map_dfr(c(test = test_dir, ref = ref_dir), dir_info, .id = 'run') %>% 
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

read_emep = function(emep_fname, emep_crs, var = 'all', dims = c('i', 'j', 'time'), proxy = T, time_index = NULL) {
  #reads emep data from a provided file name - value is a star_proxy object!!!
  #crs needs to be given as stars automatically expects Earth to be an ellipsoid
  #if var == 'all' it loads those vars which have dimensions set in dims
  #can subset on time dimension but only for vars with 2 spatial and 1 time dimension
  
  if (is.null(emep_fname)) return(NULL)
  
  emep_tidync = emep_fname %>% 
    tidync()
  emep_vars = emep_tidync$variable$name

  if (all(var != 'all')) {
    selected_var = base::intersect(emep_vars, var)
    if (length(selected_var) == 0) {
      return(NULL)
    }
  } else {
    selected_dims = emep_tidync$dimension %>% 
      filter(name %in% dims) %>% 
      pull(id) %>% 
      sort() %>% 
      str_c('D', .) %>% 
      str_c(collapse = ',')
    
    selected_var = emep_tidync$grid %>% 
      unnest(col = variables) %>% 
      filter(grid == selected_dims) %>% 
      pull(variable)
  }
  
  emep_data = read_stars(emep_fname, sub = selected_var, proxy = T, RasterIO = list(nXOff = 1, nYOff = 1)) %>% 
    st_set_crs(emep_crs)
  
  if (!is.null(time_index)) {
    if (all(names(dim(emep_data)) == c('x', 'y' ,'time'))) {
      emep_data = emep_data[selected_var, , , time_index]
    }
  }
  
  if (proxy == F) {
    emep_data = emep_data %>% 
      st_as_stars(curvilinear = NULL) #we don't want the output in curvilinear grid
  }
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


collate_obs_mod_nc = function(nc_pth, var_name_lookup, site_code = 'MY1', i_index = NA_integer_,
                           j_index = NA_integer_, network = 'aurn', var = 'no2') {
  
  #var_name_lookup needs to be format: obs_name = EMEP_var_name (e.g. no2 = 'SURF_ug_NO2')
  #network is either one of supported networks from Ricardo servers or a full path to the observation file
  
  nc = nc_open(nc_pth)
  
  nc_date = nc.get.time.series(nc) %>%
    as_datetime(tz = 'UTC') %>%
    floor_date(unit = 'hour') %>%
    tibble() %>%
    set_names('date')
  
  nc_year = unique(year(nc_date$date))
  
  var_mod = unname(var_name_lookup[str_to_lower(var)])
  
  mod_data = ncvar_get(nc,
                       varid = var_mod,
                       start = c(i_index, j_index, 1), count = c(1, 1,-1)) %>%
    as_tibble() %>%
    mutate(code = site_code, .before = value) %>% 
    rename(mod = value) %>%
    bind_cols(nc_date, .)
  if (str_detect(var_mod, 'ppb_O3')) {#convert ozone to ug/m3
    mod_data = mod_data %>%
      mutate(mod = mod * 2)
  }
  
  if (file_exists(network)) {
    #return NULL if pollutant not in the file
    obs_data = tryCatch(
      error = function(cnd) NULL,
      read_csv(network) %>%
        select(date, !!var) %>%
        rename(obs = !!var)
    )
  } else {
    #return NULL if pollutant not measured at auto site on Ricardo servers
    obs_data = tryCatch(
      error = function(cnd) NULL,
      
      get_auto_data(site = site_code, network = network, pollutant = str_to_lower(var),
                    start_year = min(nc_year), end_year = max(nc_year), to_narrow = T) %>%
        select(-code) %>% 
        rename(obs = conc, var = pollutant)
    )
  }
  
  if (!is.null(obs_data)) {
    both = left_join(mod_data, obs_data, by = 'date') %>%
      #if missing data in observations there will be na values in code and pollutant columns after joining
      #the mutate function makes sure the na values are replaced
      mutate(code = !!site_code,
             var = !!var) %>%
      relocate(mod, .after = obs)
  } else {
    log_warn('No data for {var} at {site_code}')
    both = mod_data %>%
      mutate(code = !!site_code,
             var = !!var,
             obs = NA_real_,
             .before = mod)
  }
  
  nc_close(nc)
  both
}

calc_diff_stars = function(stars1, stars2) {
  if (is.null(names(stars1)) || is.null(names(stars2))) {
    warn('both stars objects must be named. returning NULL')
    return(NULL)
  }
  stars_diff = tryCatch(
    error = function(cnd) {
      list(stars1, stars2)
    },
    c(stars1, stars2) %>% 
      mutate(abs_diff := !!sym(names(stars1)) - !!sym(names(stars2)),
             rel_diff := (!!sym(names(stars1)) - !!sym(names(stars2)))/!!sym(names(stars2)) * 100)
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

calculate_emep_diff = function(emep_var,
                               outer_test_fname = NULL, outer_ref_fname = NULL,
                               inner_test_fname = NULL, inner_ref_fname = NULL,
                               test_crs = NULL, ref_crs = NULL, time_index = NULL,
                               run_labels = c('test', 'ref')) {
  
  if (any(is.null(c(test_crs, ref_crs)))) stop('Both test_crs and ref_crs MUST be provided!')
  
  #determine what needs plotting and check if paths are valid
  emep_fnames = list(outer_test_fname, outer_ref_fname, inner_test_fname, inner_ref_fname)

  
  emep_stars = pmap(list(emep_fname = emep_fnames, emep_crs = c(test_crs, ref_crs, test_crs, ref_crs)),
                   read_emep, var = emep_var, proxy = F, time_index = time_index)
  
  if (length(compact(emep_stars)) == 0) return(NULL)
  
  out = vector('list', length = 2)
  
  for (i in 1:2) {
    stars1 = emep_stars[[i*2 - 1]]
    stars2 = emep_stars[[i*2]]
    if (is.null(stars1) || is.null(stars2)) {
      out[[i]] = NULL
    } else {
      names(stars1) = run_labels[1]
      names(stars2) = run_labels[2]
      stars_diff = tryCatch(
        error = function(cnd) {
          list(stars1, stars2)
        },
        c(stars1, stars2) %>% 
          mutate(abs_diff := !!sym(names(stars1)) - !!sym(names(stars2)),
                 rel_diff := (!!sym(names(stars1)) - !!sym(names(stars2)))/!!sym(names(stars2)) * 100)
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
      out[[i]] = stars_diff
      names(out)[[i]] = str_c(emep_var, c('outer', 'inner')[i], sep = '_')
    } 
    
  }
  if (all(map_lgl(out, is.null))) {
    return(NULL)
  } else {
    out = compact(out)
    out
  }
  
}

calculate_emep_diff2 = function(outer_test_stars = NULL, outer_ref_stars = NULL,
                                inner_test_stars = NULL, inner_ref_stars = NULL,
                                run_labels = c('test', 'ref')) {
  
  #extract var name
  emep_var = map_chr(compact(list(outer_test_stars, outer_ref_stars, inner_test_stars, inner_ref_stars)), names) %>% 
    na.omit() %>% 
    unique()
  
  emep_stars = list(outer_test_stars, outer_ref_stars, inner_test_stars, inner_ref_stars)

  out = vector('list', length = 2)
  
  for (i in 1:2) {
    stars1 = emep_stars[[i*2 - 1]]
    stars2 = emep_stars[[i*2]]
    if (is.null(stars1) || is.null(stars2)) {
      out[[i]] = NULL
    } else {
      names(stars1) = run_labels[1]
      names(stars2) = run_labels[2]
      stars_diff = tryCatch(
        error = function(cnd) {
          list(stars1, stars2)
        },
        c(stars1, stars2) %>% 
          mutate(abs_diff := !!sym(names(stars1)) - !!sym(names(stars2)),
                 rel_diff := (!!sym(names(stars1)) - !!sym(names(stars2)))/!!sym(names(stars2)) * 100)
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
      out[[i]] = stars_diff
      names(out)[[i]] = str_c(emep_var, c('outer', 'inner')[i], sep = '_')
    } 
    
  }

  out = compact(out)  

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
    summarise(across(.cols = everything(), .fns = ~sum(.x, na.rm = T))) %>% 
    ungroup()
  
  RL_tables
  
}

compare_run_emissions = function(test_dir, ref_dir, save_file = T, mbs_table_fname = NA) {

  MBS_files = c(test_dir,ref_dir) %>% 
    path('MassBudgetSummary.txt') %>% 
    set_names(c('test', 'ref'))
  
  MBS_data = MBS_files %>% 
    map_dfr(read_table, skip = 1, col_names = T, .id = 'run') %>% 
    select(run, species = Spec, emission = emis) %>% 
    pivot_wider(id_cols = species, names_from = run, values_from = emission) %>% 
    mutate(abs_diff = test - ref,
           rel_diff = abs_diff/ref*100)
  if (save_file == T) {
    # - determine output filename based on whether it is an outer or inner domain file
    stopifnot('output file name must be provided' = !is.na(mbs_table_fname))
    out_fname = paste0(map_chr(path_split(test_dir), last), '_', mbs_table_fname)
    write_lines(paste0('# Test file: ', MBS_files[1]), file = path(tables_pth_out, out_fname))
    write_lines(paste0('# Ref file: ', MBS_files[2]), path(tables_pth_out, out_fname), append = T)
    write_lines(paste0('#', str_c(rep('-', 50), collapse = '-')), path(tables_pth_out, out_fname), append = T)
    write_csv(MBS_data, path(tables_pth_out, out_fname), na = '', col_names = T, append = T)
  }
  MBS_data
}

compare_inv_mod_emissions = function(model_run_dir, emiss_inv_pth, save_file = T) {
  
  if (!file_exists(emiss_inv_pth)) {
    stop('Emission Inventory filepath is not valid.')
  }
  runlog_file = read_lines(path(model_run_dir, 'RunLog.out'))
  
  mod_year = runlog_file[str_detect(runlog_file, 'emissions by countries')] %>% 
    str_extract('\\d{4}') %>%
    as.integer() %>% 
    unique()
  stopifnot('Could not determine the model emission year - check RunLog.out' = length(mod_year) == 1)
  
  # - determine output filename based on whether it is an outer or inner domain file
  out_fname = paste0(map_chr(path_split(model_run_dir), last), '_', INV_MOD_EMISS_TABLE_FNAME)
  
  runlog_emiss = model_run_dir %>% 
    path('RunLog.out') %>% 
    read_RunLog_emissions() %>% 
    mutate(type = 'model')
  
  inventory = read_delim(emiss_inv_pth, comment = '#', delim = ';',
                         col_names = c('Land', 'year', 'sector', 'pollutant', 'unit', 'value'),
                         col_types = 'cicccd') %>% 
    filter(year == mod_year) %>% 
    select(Land, pollutant, value) %>% 
    pivot_wider(id_cols = Land, names_from = pollutant, values_from = value) %>% 
    rename(co = CO, nh3 = NH3, voc = NMVOC, nox = NOx, pm25 = PM2.5, pmco = PMcoarse, sox = SOx) %>% 
    mutate(type = 'inventory')
  
  merged = bind_rows(inventory, runlog_emiss) %>% 
    group_by(Land) %>% 
    filter(n() > 1) %>% 
    ungroup() %>% 
    pivot_longer(cols = co:sox, names_to = 'pollutant', values_to = 'emiss') %>%
    pivot_wider(id_cols = c(Land, pollutant), names_from = type, values_from = emiss) %>% 
    mutate(abs_diff = model - inventory,
           rel_diff = abs_diff/inventory*100) %>% 
    arrange(Land)
  
  if (save_file == T) {
    write_lines(paste0('# EMEP file: ', path(model_run_dir, 'RunLog.out')), file = path(tables_pth_out, out_fname))
    write_lines(paste0('#', str_c(rep('-', 50), collapse = '-')), path(tables_pth_out, out_fname), append = T)
    write_csv(merged, path(tables_pth_out, out_fname), na = '', col_names = T, append = T)
  }
  
  merged
  
}

summarise_mobs = function(mobs_lframe, var = 'all', avg_time = 'day',
                          summary_stat = 'mean', data_thresh = 75, drop_na = T) {
  
  #mobs_lframe is mobs dataframe in the long format
  if (drop_na == T) {
    mobs = mobs_lframe %>% 
      mutate(mod = if_else(is.na(obs), NA_real_, mod)) 
  } else {
    mobs = mobs_lframe
  }
  mobs = mobs %>%
    timeAverage(avg.time = avg_time, data.thresh = data_thresh, type = c('code', 'var'), statistic = summary_stat) %>% 
    ungroup() %>% 
    mutate(across(where(is.factor), ~as.character(.x)))
  
  if (var != 'all') {
    mobs = mobs %>% 
      filter(var %in% !!var)
  }
  mobs
  
}

calculate_modstats = function(dframe, modstats = MODSTATS_STATS, type = 'default', pretty_format = T) {
  #workaround for modStats crashing when calculating r if n < 3
  #if pretty_format == T the output is rounded to two or one decimal place depending on stat
  
  if(is.null(type)) type = 'default' #modstats doesn't accept NULL in type
  type = compact(type)
  
  if ('r' %in% modstats) {
    modstats2 = setdiff(modstats, 'r')
    modstats2 = c('n', modstats2) %>% #make sure n is in modstats2
      unique()
    
    mobs_stats = modStats(dframe, statistic = modstats2, type = type)
    
    ms_var = mobs_stats %>%
      group_by(across(all_of(type))) %>% 
      filter(n > 2) %>%
      ungroup() %>% 
      select(all_of(type))
    
    if (nrow(ms_var) > 0) {
      mobs_stats = dframe %>% 
        semi_join(ms_var) %>% 
        modStats(statistic = 'r', type = type) %>% 
        left_join(mobs_stats, .) %>% 
        mutate(across(where(is.factor), ~as.character(.x)))
    }
  }
  
  if (pretty_format == T) {
    mobs_stats = mobs_stats %>% 
      mutate(across(any_of(c('FAC2', 'NMB', 'r')), ~round(.x, 2)),
             across(any_of(c('MB', 'RMSE')), ~round(.x, 1)),
             across(any_of(c('p', 'P')), ~round(.x, 4)))
  }
  
  mobs_stats
}

add_ox = function(site_dframe, df_format = c('long', 'wide'), units = 'ug/m3') {
  #adds ox concentrations to the dataframe if both no2 and o3 (must be in ug/m3) are present
  
  if (all(df_format == 'wide')) {
    site_dframe = mobs_to_long(site_dframe)
  }
  if (all(c('no2', 'o3') %in% unique(site_dframe$var))) {
    site_dframe_ox = site_dframe %>%
      filter(var %in% c('no2', 'o3')) %>%
      pivot_wider(id_cols = c(date, code), names_from = var, values_from = c(obs, mod))
    
    if (units == 'ug/m3') {
      site_dframe_ox = site_dframe_ox %>% 
        mutate(var = 'ox',
               obs = obs_o3 + obs_no2,
               mod = mod_o3 + mod_no2) %>%
        dplyr::select(date, code, var, obs, mod)
    } else if (units == 'ppb') {
      site_dframe_ox = site_dframe_ox %>%
        mutate(var = 'ox(ppb)',
               obs = obs_o3/2 + obs_no2/1.9125,
               mod = mod_o3/2 + mod_no2/1.9125) %>%
        dplyr::select(date, code, var, obs, mod)
    } else {
      stop('Units for Ox calculation can either be "ppb" or "ug/m3"')
    }
    site_dframe = site_dframe %>%
      bind_rows(site_dframe_ox) %>% 
      distinct() %>%
      arrange(date)
  } else {

  }
  site_dframe
}

my_importNOAA = function(code, year, pth = NULL) {
  # function to supress timeAverage printing
  # (can't see option to turn it off)
  quiet <- function(x) { 
    sink(tempfile()) 
    on.exit(sink()) 
    invisible(force(x)) 
  } 
  
  ## location of data
  file.name <- paste0(
    "https://www.ncei.noaa.gov/data/global-hourly/access/",
    year, "/", gsub(pattern = "-", "", code), ".csv"
  )
  
  # suppress warnings because some fields might be missing in the list
  # Note that not all available data is returned - just what I think is most useful
  met_data <- try(suppressWarnings(read_csv(
    file.name,
    col_types = cols_only(
      STATION = col_character(),
      DATE = col_datetime(format = ""),
      SOURCE = col_double(),
      LATITUDE = col_double(),
      LONGITUDE = col_double(),
      ELEVATION = col_double(),
      NAME = col_character(),
      REPORT_TYPE = col_character(),
      CALL_SIGN = col_double(),
      QUALITY_CONTROL = col_character(),
      WND = col_character(),
      CIG = col_character(),
      VIS = col_character(),
      TMP = col_character(),
      DEW = col_character(),
      SLP = col_character(),
      AA1 = col_character(),
      AW1 = col_character(),
      GA1 = col_character(),
      GA2 = col_character(),
      GA3 = col_character()
    ),
    progress = FALSE
  )), silent = TRUE
  )
  
  if (class(met_data)[1] == "try-error") {
    message(paste0("Missing data for site ", code, " and year ", year))
    met_data <- NULL
    return()
  }
  
  met_data <- rename(met_data,
                     code = STATION,
                     station = NAME,
                     date = DATE,
                     latitude = LATITUDE,
                     longitude = LONGITUDE,
                     elev = ELEVATION
  )
  
  met_data$code <- code
  
  # separate WND column
  
  if ("WND" %in% names(met_data)) {
    met_data <- separate(met_data, WND, into = c("wd", "wd_qc", "wo_tc", "ws", "ws_qc"))
    
    met_data <- mutate(met_data,
                       across(c(wd, wd_qc, ws, ws_qc), ~as.numeric(.x)),
                       wd = if_else(wd == 999, NA, wd),
                       ws = if_else(ws == 9999, NA, ws),
                       ws = ws / 10
    )
  }
  
  # separate TMP column
  if ("TMP" %in% names(met_data)) {
    met_data <- separate(met_data, TMP, into = c("air_temp", "air_temp_qc"), sep = ",")
    
    met_data <- mutate(met_data,
                       air_temp = as.numeric(air_temp),
                       air_temp = if_else(air_temp == 9999, NA, air_temp),
                       air_temp = air_temp / 10
    )
  }

  # separate DEW column
  if ("DEW" %in% names(met_data)) {
    met_data <- separate(met_data, DEW, into = c("dew_point", "dew_qc"), sep = ",")
    
    met_data <- mutate(met_data,
                       dew_point = as.numeric(dew_point),
                       dew_point = if_else(dew_point == 9999, NA, dew_point),
                       dew_point = dew_point / 10
    )
  }
  # separate SLP column
  if ("SLP" %in% names(met_data)) {
    met_data <- separate(met_data, SLP,
                         into = c("atmos_pres", "pres_qc"), sep = ",",
                         fill = "right"
    )
    
    met_data <- mutate(met_data,
                       atmos_pres = as.numeric(atmos_pres),
                       atmos_pres = if_else(atmos_pres %in% c(99999, 999999), NA, atmos_pres),
                       atmos_pres = atmos_pres / 10
    )
  }

  ## relative humidity - general formula based on T and dew point
  met_data$RH <- 100 * ((112 - 0.1 * met_data$air_temp + met_data$dew_point) /
                          (112 + 0.9 * met_data$air_temp))^8
  
  
  # PRECIP AA1
  if ("AA1" %in% names(met_data)) {
    met_data <- separate(met_data, AA1,
                         into = c("precip_code", "precip_raw", "precip_cc", "precip_qc"),
                         sep = ","
    )
    
    met_data <- mutate(met_data,
                       precip_raw = as.numeric(precip_raw),
                       precip_raw = if_else(precip_raw == 9999, NA, precip_raw),
                       precip_raw = precip_raw / 10,
                       precip_code = if_else(precip_code =='99', NA, precip_code) 
    )
  }

  ## select the variables we want
  met_data <- select(met_data, any_of(c(
    "date", "code", "report_type" = "REPORT_TYPE",
    "ws", "wd", "wo_tc", "wd_qc",
    "air_temp", "air_temp_qc",
    "atmos_pres", "pres_qc",
    "dew_point", "dew_qc", "RH",
    "precip_code", "precip_raw",
    "precip_cc", "precip_qc"
  )))
  
  if (!is.null(pth)) {
    write_rds(met_data, pth)
  }
  met_data
}

clean_noaa = function(noaa_dframe) {
  #filters out problematic data based on quality flags
  #please see isd_format_document.pdf for flag explanation
  noaa_dframe = noaa_dframe %>%
    mutate(wd = if_else(wd_qc %in% c(1, 5), wd, NA),
           wd = if_else(wo_tc %in% c('C', 'N', 'V', '9'), wd, NA),
           ws = if_else(wd_qc %in% c(1, 5), ws, NA),
           air_temp = if_else(air_temp_qc %in% c('1', '5'), air_temp, NA),
           dew_point = if_else(dew_qc %in% c('1', '5'), dew_point, NA),
           atmos_pres = if_else(pres_qc %in% c('1', '5'), atmos_pres, NA),
           RH = if_else(!(is.na(air_temp) | is.na(dew_point)), RH, NA))
  
  if ('precip_code' %in% names(noaa_dframe)) {
    noaa_dframe = noaa_dframe %>%
      mutate(precip_raw = if_else(precip_qc %in% c('1', '5') & precip_cc %in% c('2', '3', '9') & !precip_code %in% c('00', '99'), precip_raw, NA),
             precip_code = if_else(precip_qc %in% c('1', '5') & precip_cc %in% c('2', '3', '9') & !precip_code %in% c('00', '99'), precip_code, NA))
  }
  
  noaa_dframe = noaa_dframe %>% 
    mutate(obs_minute = minute(date), .after = report_type) %>% 
    select(-any_of(c('wo_tc', 'wd_qc', 'air_temp_qc', 'pres_qc', 'dew_qc', 'precip_cc', 'precip_qc')))
  
  noaa_dframe
}

assess_noaa = function(noaa_dframe) {
  #outputs a tibble with data capture summary per report type and minute of observation
  #use this to select suitable data
  #see isd_format_document.pdf for report_type description
  
  #make sure minute of observation is a column
  if (!'obs_minute' %in% names(noaa_dframe)) {
    noaa_dframe = noaa_dframe %>% 
      mutate(obs_minute = minute(date))
  }
  
  nonprecip_summary0 = noaa_dframe %>%
    select(-matches('precip'))
  
  met_vars = intersect(names(nonprecip_summary0), c('ws', 'wd', 'air_temp', 'atmos_pres', 'dew_point', 'RH'))
  nonprecip_summary = vector('list', length(met_vars))
  for (i in seq_along(met_vars)) {
    nonprecip_summary[[i]] = nonprecip_summary0 %>% 
      select(date, code, report_type, obs_minute, !!met_vars[i]) %>%
      drop_na() %>% 
      group_by(code, report_type, obs_minute) %>% 
      tally(name = glue::glue('{met_vars[i]}_n')) %>% 
      mutate('{met_vars[i]}_dc' := round(.data[[glue::glue('{met_vars[i]}_n')]]/Hmisc::yearDays(noaa_dframe$date[1])/24*100, 1)) %>% 
      select(-matches('_n$'))
  }  
  
  obs_summary = nonprecip_summary %>% 
    map(~ungroup(.x)) %>% 
    reduce(left_join, by = c('code', 'report_type', 'obs_minute'))
  
  if ('precip_code' %in% names(noaa_dframe)) {
    precip_summary = noaa_dframe %>%
      select(date, code, report_type, obs_minute, matches('precip')) %>% 
      group_by(code, report_type, obs_minute, precip_code) %>% 
      drop_na(precip_raw) %>% 
      tally(name = 'precip_n')
    
    obs_summary = full_join(obs_summary, precip_summary, by = c('code', 'report_type', 'obs_minute'))
  }
  
  obs_summary
}

format_noaa = function(noaa_dframe) {
  
  #ensure obs_minute is the actual value in the data and is not already there from previous assessment
  #as it can be change by e.g. rounding up to the top of the hour
  noaa_dframe = noaa_dframe %>% 
    mutate(obs_minute = minute(date))
  
  assert(check_number(unique(noaa_dframe$obs_minute)),
         check_number(length(unique(noaa_dframe$report_type))),
         combine = 'and')
  
  noaa_dframe = noaa_dframe %>% 
    select(-report_type)
  
  met_var_lookup = c(air_temp = 'T2',
                     dew_point  = 'td2',
                     atmos_pres = 'slp', 
                     RH = 'rh2')
  
  full_year_range = seq.POSIXt(from = as_datetime(str_c(unique(year(noaa_dframe$date))[1], '-01-01 00:', str_pad(unique(noaa_dframe$obs_minute), width = 2, side = 'left', pad = '0'), ':00')),
                               to = as_datetime(str_c(unique(year(noaa_dframe$date))[1], '-12-31 23:', str_pad(unique(noaa_dframe$obs_minute), width = 2, side = 'left', pad = '0'), ':00')),
                               by = 'hour') %>% 
    as_datetime() %>% 
    as_tibble() %>% 
    set_names(nm = 'date')
  
  noaa_dframe2 = left_join(full_year_range, noaa_dframe, by = 'date') %>% 
    mutate(code = replace_na(code, na.omit(unique(noaa_dframe$code))))#,
  #report_type = replace_na(report_type, na.omit(unique(noaa_dframe$report_type))))
  
  if ('precip_code' %in% names(noaa_dframe)) {
    noaa_dframe2 = noaa_dframe2 %>% 
      #select(date, obs_minute, code, precip_code, precip_raw) %>% 
      mutate(precip_code = as.numeric(precip_code)) %>% 
      arrange(date)
    
    precip_codes = unique(noaa_dframe2$precip_code) %>%
      sort(decreasing = T)
    for (i in seq_along(precip_codes)) {
      slice_ends = which(noaa_dframe2$precip_code == precip_codes[i])
      #remove indeces that would result in slicing out of bounds
      slice_logical = !(slice_ends - precip_codes[i]) < 1
      for (j in seq_along(slice_ends)) {
        if (slice_logical[j] == F) {
          #erase any other precip_code values within the precip_code period
          noaa_dframe2$precip_code[1:(slice_ends[j] - 1)] = NA
          #redistribute accumulated rainfall equally within the precip_code period
          noaa_dframe2$precip_raw[1:(slice_ends[j])] = noaa_dframe2$precip_raw[(slice_ends[j])]/precip_codes[i]
        } else {
          #erase any other precip_code values within the precip_code period
          noaa_dframe2$precip_code[(slice_ends[j] - precip_codes[i] + 1):(slice_ends[j] - 1)] = NA
          #redistribute accumulated rainfall equally within the precip_code period
          noaa_dframe2$precip_raw[(slice_ends[j] - precip_codes[i] + 1):(slice_ends[j])] = noaa_dframe2$precip_raw[(slice_ends[j])]/precip_codes[i]
        }
      }
    }
    
    noaa_dframe2 = noaa_dframe2 %>%
      rename(precip = precip_raw)
  } else {
    #create empty precip columns needed for merging with model data
    noaa_dframe2 = noaa_dframe2 %>% 
      mutate(precip = NA_real_,
             precip_code = NA_real_)
  }
  
  noaa_dframe3 = noaa_dframe2 %>% 
    select(-obs_minute) %>% 
    pivot_longer(cols = c(-date, -code), names_to = 'var', values_to = 'value') %>% 
    mutate(var = if_else(var %in% names(met_var_lookup), met_var_lookup[var], var),
           scenario = 'obs')
  
  noaa_dframe3
  
}

format_mobs_to_plot = function(mobs_lframe, ...) {
  #creates a nested tibble with daily means for the full year (month == -1) and
  #separate hourly data for each month
  #this format is needed to plot annual and monthly time series plots#
  #... for summarise_mobs_function
  
  #discard data from the subsequent year (e.g. in wrf modelling)
  mobs_lframe = mobs_lframe %>% 
    filter(year(date) == min(year(date)))

  #for precip data we use cumulative sum
  precip_data = mobs_lframe %>% 
    filter(str_detect(var, 'precip'))
  
  if (nrow(precip_data) > 0) {

    precip_data_wide = precip_data %>% 
      pivot_wider(id_cols = c(date, code), names_from = var, values_from = c(mod, obs))
    
    pdw_y = precip_data_wide %>% 
      mutate(date = floor_date(date, 'day')) %>% 
      group_by(code, date) %>% 
      #when summarising, output NA if all values are NA, in all other cases remove NAs
      summarise(mod_precip = sum(mod_precip, na.rm = T),
                mod_subprecip = ifelse(all(is.na(obs_precip)), NA_real_, sum(mod_subprecip, na.rm = T)),
                obs_subprecip = ifelse(all(is.na(obs_precip)), NA_real_, sum(obs_subprecip, na.rm = T)),
                obs_precip = ifelse(all(is.na(obs_precip)), NA_real_, sum(obs_precip, na.rm = T))) %>% 
      ungroup() %>% 
      mutate(across(-c(code, date), ~calculate_cumsum(.x)),
             month = -1)
    
    pdw_m = precip_data_wide %>% 
      group_by(code, month = month(date)) %>% 
      mutate(across(-date, ~calculate_cumsum(.x))) %>% 
      ungroup()
    
    precip_data = bind_rows(pdw_y, pdw_m) %>% 
      pivot_longer(cols = -c(code, date, month), names_to = c('scenario', 'var'), names_sep = '_',  values_to = 'value' ) %>% 
      pivot_wider(names_from = scenario, values_from = value)
    
    mobs_lframe = mobs_lframe %>% 
      filter(str_detect(var, 'precip', negate = T))
    
  }
  
  data_daily_means = mobs_lframe %>% 
    summarise_mobs(...) %>% 
    mutate(month = -1)
  mobs_out = mobs_lframe %>% 
    mutate(month = month(date)) %>% 
    bind_rows(data_daily_means, precip_data) %>%
    group_by(code, month) %>% 
    nest() %>% 
    arrange(month)
  mobs_out
}

check_run_errors = function(run_file, error_msg = NULL, error_msg_file = NULL) {
  assert()
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

plot_comp_maps2_WIP = function(diff_list, var_params_list, pal_dir = PALETTE_DIR,
                           var_pal = 'WhiteBlueGreenYellowRed.rgb',
                           adiff_pal = 'NCV_blu_red.rgb',
                           rdiff_pal = 'NCV_blu_red.rgb',
                           autoscale = F,
                           autobreak_type = c('linear', 'percentile'),
                           n_bins = 10,
                           var_breaks = NULL,
                           adiff_breaks = NULL,
                           rdiff_breaks = NULL,
                           gg_basemap = NULL,
                           pretty_lab = F) {
  
  var = str_replace(names(diff_list)[1], '_[^_]+$', '')
  
  if (pretty_lab == T) {
    fill_labs = c(myquickText(paste0(var_params_list[[var]][['pretty_lab']], ' (', var_params_list[[var]][['units']], ')')),
                  myquickText(paste0(var_params_list[[var]][['pretty_lab']], ' (', var_params_list[[var]][['units']], ')')),
                  myquickText(paste0('Delta ', var_params_list[[var]][['pretty_lab']], ' (', var_params_list[[var]][['units']], ')')),
                  myquickText(paste0('Delta ', var_params_list[[var]][['pretty_lab']], ' (%)')))
    top_title = myquickText(var_params_list[[var]][['pretty_lab']])
  } else {
    fill_labs = c(paste0(var, ' (', var_params_list[[var]][['units']], ')'),
                  paste0(var, ' (', var_params_list[[var]][['units']], ')'),
                  paste0('Delta ', var, ' (', var_params_list[[var]][['units']], ')'),
                  paste0('Delta ', var, ' (%)'))
    top_title = var
  }
  
  if (is.null(gg_basemap)) {
    #download country borders for plotting
    boundaries0 = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
      st_cast("MULTILINESTRING")
    boundaries_test = boundaries0 %>%  
      st_transform(st_crs(diff_list[[1]][[1]]))
    boundaries_ref = boundaries0%>%  
      st_transform(st_crs(diff_list[[1]][[2]]))
  } else {
    # needs finishing
  }
  
  p1_list = map(diff_list, 1)
  p2_list = map(diff_list, 2)
  p3_list = map(diff_list, 3) %>% 
    compact() #make sure no nulls present (in case diff couldn't be calculated)
  p4_list = map(diff_list, 4) %>% 
    compact() # as above
  
  var_pal = read_color(path(pal_dir, var_pal))
  adiff_pal = read_color(path(pal_dir, adiff_pal))
  rdiff_pal = read_color(path(pal_dir, rdiff_pal))
  
  extract_extreme = function(stars_list, extreme) {
    assertChoice(extreme, c('min', 'max'))
    if (extreme == 'max') {
      out = stars_list %>% 
        map_dbl(~map_dbl(.x, ~max(.x, na.rm = T), .y = .x)) %>% 
        max()
    } else if (extreme == 'min') {
      out = stars_list %>% 
        map_dbl(~map_dbl(.x, ~min(.x, na.rm = T), .y = .x)) %>% 
        min()
    }
    out
  }
  
  
  #calculate color breaks
  #first limits
  max_var = extract_extreme(c(p1_list, p2_list), 'max')
  
  max_vals = c(extract_extreme(c(p1_list, p2_list), 'max'),
               extract_extreme(p3_list, 'max'),
               extract_extreme(p4_list, 'max'))
  
  min_vals = c(extract_extreme(c(p1_list, p2_list), 'min'),
               extract_extreme(p3_list, 'min'),
               extract_extreme(p4_list, 'min'))

  if (autoscale == T) {
    if (autobreak_type[1] == 'percentile') {
      var_breaks = last(p1_list) %>% 
        pull() %>% 
        as.numeric() %>% 
        quantile(probs = seq(0, 1, 1/n_bins))
    } else {
      var_breaks = seq(min_vals[1], max_vals[1], length.out = n_bins + 1)
    }
  } else {
    var_breaks = var_params_list[[var]][['map_levs']]
  }
  
  #make sure all var values are within the colorscale range
  if (min_vals[1] < var_breaks[1]) {
    var_breaks[1] = min_vals[1]
  }
  
  if (max_vals[1] > last(var_breaks)) {
    var_breaks[length(var_breaks)] = max_vals[1]
  }

  n_round = min(var_breaks - lag(var_breaks), na.rm = T) %>% 
    log10() %>% 
    floor()
  n_round = ifelse(n_round <= 0, n_round * -1 + 1, n_round -1)

  var_breaks[1] = floor_dec(var_breaks[1], n_round)
  var_breaks[length(var_breaks)] = ceiling_dec(last(var_breaks), n_round)
  var_breaks = round(var_breaks, digits = n_round)
  
  
  #diff breaks WIP
  adiff_abs_max = max(abs(max_vals[2]), abs(min_vals[2]))
  
  adiff_data = p3_list %>%
    flatten() %>%
    map(as.numeric) %>%
    flatten_dbl()
  adiff_pretty = adiff_data %>% 
    pretty()
  
  if (0 %in% adiff_pretty) {
    adiff_step = adiff_pretty - lag(adiff_pretty)
    min_adiff_step = adiff_step %>% 
      na.omit() %>% 
      unique()
  }
  
  rdiff_range = max_vals[3] - min_vals[3]
  
  diff_breaks = c(-1e6, var_params_list[[var]][['map_difflevs']], 1e6)
  diff_labels = sprintf(var_params_list[[var]][['map_diffprecision']], diff_breaks)
  diff_labels[seq(1:length(diff_labels)) %% 2 == 1] = ''
  diff_labels = diff_labels[c(-1, -length(diff_breaks))]
  #labels for relative difference - hardcoded to -100, -80,...,0,...,80, 100 percent
  diff_rel_labels = if_else(seq_along(c(-1e6, seq(-100, 100, 10), 1e6)) %% 2 == 1,
                            '', as.character(c(-1e6, seq(-100, 100, 10), 1e6))) %>% 
    .[c(-1, -length(c(-1e6, seq(-100, 100, 10), 1e6)))]
  
  ncl_rainbow = read_color(path(ncl_palette_dir, 'WhiteBlueGreenYellowRed.rgb')) %>%
    get_color(n = length(map_breaks) -1)
  
  ncl_rwb = read_color(path(ncl_palette_dir,'NCV_blu_red.rgb')) %>% 
    get_color(n = length(diff_breaks) - 1)
  
  p1 = ggplot()
  for (i in seq_along(p1_list)) {
    p1 = p1 +
      geom_stars(data = cut(p1_list[[i]], breaks = var_breaks, include.lowest = T))
  }
  p1 = p1 + 
    geom_sf(data = st_crop(boundaries_test, p1_list[[1]]) , color = "black", fill = NA, size = 0.07) +
    scale_fill_manual(values = get_color(var_pal, n = length(var_breaks - 1)), drop = F, 
                      labels = scales::label_number(scale_cut = c(0, 'k' = 1e3, 'M' = 1e6, 'G' = 1e9, 'T' = 1e12)),
                      guide = guide_coloursteps(show.limits = T,
                                                title.position = 'top',
                                                title.hjust = 0.5, frame.colour = 'black',
                                                frame.linewidth = 0.1,
                                                barwidth = unit(6.7, 'inches'))) +
    labs(title = names(diff_list[[1]][[1]]),
         fill = fill_labs[1])
  
  p1 = theme_emep_diffmap(p1)
  
  p2 = ggplot()
  for (i in seq_along(p2_list)) {
    p2 = p2 +
      geom_stars(data = cut(p2_list[[i]], breaks = var_breaks, include.lowest = T))
  }
  p2 = p2 + 
    geom_sf(data = st_crop(boundaries_ref, p2_list[[1]]) , color = "black", fill = NA, size = 0.07) +
    scale_fill_manual(values = get_color(var_pal, n = length(var_breaks - 1)), drop = F, 
                      labels = scales::label_number(scale_cut = c(0, 'k' = 1e3, 'M' = 1e6, 'G' = 1e9, 'T' = 1e12)),
                      guide = guide_coloursteps(show.limits = T,
                                                title.position = 'top',
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
        geom_stars(data = cut(p3_list[[i]], breaks = diff_breaks, include.lowest = T))
    }
    p3 = p3 +
      geom_sf(data = st_crop(boundaries_test, p3_list[[1]]), color = "black", fill = NA, size = 0.07) + 
      scale_fill_manual(values = ncl_rwb, drop = F, 
                        labels = diff_labels,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(3.2, 'inches'))) +
      labs(title = paste0(names(diff_list[[1]][[1]]), ' - ', names(diff_list[[1]][[2]]), ' (abs)'),
           fill = fill_labs[3])
    
    p3 = theme_emep_diffmap(p3)
    
    #check if all abs_diff are NA (= test and ref values are identical)
    identical_data = map(p3_list, as_tibble) %>%
      map(drop_na) %>%
      map_dbl(nrow)
    
    if (all(identical_data == 0)) {
      p3 = p3 +
        annotation_custom(grid::roundrectGrob(width = 0.5, height = 0.08)) +
        annotation_custom(grid::textGrob('Data are identical'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        theme(legend.position = 'none')
    }
    
  } else {
    p3 = create_blank_plot() +
      annotation_custom(grid::roundrectGrob(width = 0.9, height = 0.08)) +
      annotation_custom(grid::textGrob('Absolute difference not calculable'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme(legend.position = 'none')
  }
  
  p4_list = map(diff_list, 4) %>% 
    compact()
  if (length(p4_list) > 0) {
    p4 = ggplot()
    for (i in seq_along(p4_list)) {
      p4 = p4 +
        geom_stars(data = cut(p4_list[[i]], breaks = c(-1e6, seq(-100, 100, 10), 1e6), include.lowest = T))
    }
    p4 = p4 +
      geom_sf(data = st_crop(boundaries_test, p4_list[[1]]), color = "black", fill = NA, size = 0.07) + 
      scale_fill_manual(values = ncl_rwb, drop = F, 
                        labels = diff_rel_labels,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(3.2, 'inches'))) +
      labs(title = paste0(names(diff_list[[1]][[1]]), ' - ', names(diff_list[[1]][[2]]), ' (rel)'),
           fill = fill_labs[4])
    
    p4 = theme_emep_diffmap(p4)
    
    #check if all abs_diff are NA (= test and ref values are identical)
    identical_data = map(p4_list, as_tibble) %>%
      map(drop_na) %>%
      map_dbl(nrow)
    
    if (all(identical_data == 0)) {
      p4 = p4 +
        annotation_custom(grid::roundrectGrob(width = 0.5, height = 0.08)) +
        annotation_custom(grid::textGrob('Data are identical'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        theme(legend.position = 'none')
    }
  } else {
    p4 = create_blank_plot() +
      annotation_custom(grid::roundrectGrob(width = 0.9, height = 0.08)) +
      annotation_custom(grid::textGrob('Relative difference not calculable'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme(legend.position = 'none')
  }
  
  pp = ggarrange(ggarrange(p1, p2, ncol = 2,
                           common.legend = T, legend = 'bottom'),
                 ggarrange(p3, p4, ncol = 2, align = 'h'),
                 nrow = 2, align = c('hv')) %>% 
    annotate_figure(top = text_grob(top_title, size = 20, color = '#473C8B'))
  
  pp

}

plot_comp_maps = function(diff_list, ncl_palette_dir = getwd(), pretty_lab = F) {
  
  var = str_replace(names(diff_list)[1], '_[^_]+$', '')
  #download country borders for plotting
  boundaries0 = rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
    st_cast("MULTILINESTRING")
  boundaries_test = boundaries0 %>%  
    st_transform(st_crs(diff_list[[1]][[1]]))
  boundaries_ref = boundaries0%>%  
    st_transform(st_crs(diff_list[[1]][[2]]))
  
  extract_extreme = function(stars_list, extreme) {
    assertChoice(extreme, c('min', 'max'))
    if (extreme == 'max') {
      out = stars_list %>% 
        map_dbl(~map_dbl(.x, ~max(.x, na.rm = T), .y = .x)) %>% 
        max()
    } else if (extreme == 'min') {
      out = stars_list %>% 
        map_dbl(~map_dbl(.x, ~min(.x, na.rm = T), .y = .x)) %>% 
        min()
    }
    out
  }
  
  p1_list = map(diff_list, 1)
  p2_list = map(diff_list, 2)
  p3_list = map(diff_list, 3) %>% 
    compact()
  p4_list = map(diff_list, 4) %>% 
    compact()
  
  
  #make sure all values are included in the colorscale range
  max_val = extract_extreme(c(p1_list, p2_list), 'max')
  min_val = extract_extreme(c(p1_list, p2_list), 'min')
  max_abs_diff = extract_extreme(p3_list, 'max')
  min_abs_diff = extract_extreme(p3_list, 'min')
  max_rel_diff = extract_extreme(p4_list, 'max')
  min_rel_diff = extract_extreme(p4_list, 'min')
  

  #breaks and labels for colorbars
  map_breaks = VAR_PARAMS_LIST[[var]][['map_levs']]
  
  map_breaks_accuracy_exp = decimal_count(map_breaks) %>% 
    max()
  
  if (max_val > last(map_breaks)) {
    map_breaks[length(map_breaks)] = ceiling(max_val)
  } else {
  }
  
  if (min_val < first(map_breaks)) {
    map_breaks[1] = floor(min_val)
  } else {
  }
  
  #map_breaks_accuracy = 10^(-map_breaks_accuracy_exp)
  #map_labs = as.character(map_breaks)[c(-1,-length(map_breaks))] 
  
  
  diff_breaks = c(min(-1e6, floor(min_abs_diff)), VAR_PARAMS_LIST[[var]][['map_difflevs']], max(1e6, ceiling(max_abs_diff)))
  diff_labels = sprintf(VAR_PARAMS_LIST[[var]][['map_diffprecision']], diff_breaks)
  diff_labels[seq(1:length(diff_labels)) %% 2 == 1] = '' #use every other label
  diff_labels = diff_labels[c(-1, -length(diff_breaks))] # don't label min and max
  rel_diff_breaks = c(min(-1e6, floor(min_rel_diff)), seq(-100, 100, 10), max(1e6, ceiling(max_rel_diff)))
  diff_rel_labels = if_else(seq_along(c(-1e6, seq(-100, 100, 10), 1e6)) %% 2 == 1,
                            '', as.character(c(-1e6, seq(-100, 100, 10), 1e6))) %>% 
    .[c(-1, -length(c(-1e6, seq(-100, 100, 10), 1e6)))]
  
  ncl_rainbow = read_color(path(ncl_palette_dir, 'WhiteBlueGreenYellowRed.rgb')) %>%
    get_color(n = length(map_breaks) -1)
  
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
  
  p1 = ggplot()
  for (i in seq_along(p1_list)) {
    p1 = p1 +
      geom_stars(data = cut(p1_list[[i]], breaks = map_breaks, include.lowest = T))
  }
  p1 = p1 + 
    geom_sf(data = st_crop(boundaries_test, p1_list[[1]]) , color = "black", fill = NA, size = 0.07) +
    scale_fill_manual(values = ncl_rainbow, drop = F, 
                      labels = scales::label_number(scale_cut = c(0, 'k' = 1e3, 'M' = 1e6, 'G' = 1e9, 'T' = 1e12)),
                      guide = guide_coloursteps(show.limits = T,
                                                title.position = 'top',
                                                title.hjust = 0.5, frame.colour = 'black',
                                                frame.linewidth = 0.1,
                                                barwidth = unit(6.7, 'inches'))) +
    labs(title = names(diff_list[[1]][[1]]),
         fill = fill_labs[1])
  
  p1 = theme_emep_diffmap(p1)
  
  p2 = ggplot()
  for (i in seq_along(p2_list)) {
    p2 = p2 +
      geom_stars(data = cut(p2_list[[i]], breaks = map_breaks, include.lowest = T))
  }
  p2 = p2 + 
    geom_sf(data = st_crop(boundaries_ref, p2_list[[1]]) , color = "black", fill = NA, size = 0.07) +
    scale_fill_manual(values = ncl_rainbow, drop = F, 
                      labels = scales::label_number(scale_cut = c(0, 'k' = 1e3, 'M' = 1e6, 'G' = 1e9, 'T' = 1e12)),
                      guide = guide_coloursteps(show.limits = T,
                                                title.position = 'top',
                                                title.hjust = 0.5, frame.colour = 'black',
                                                frame.linewidth = 0.1,
                                                barwidth = unit(6.7, 'inches'))) +
    labs(title = names(diff_list[[1]][[2]]),
         fill = fill_labs[2])
  
  p2 = theme_emep_diffmap(p2)
  
  if (length(p3_list) > 0) {
    p3 = ggplot()
    for (i in seq_along(p3_list)) {
      p3 = p3 +
        geom_stars(data = cut(p3_list[[i]], breaks = diff_breaks, include.lowest = T))
    }
    p3 = p3 +
      geom_sf(data = st_crop(boundaries_test, p3_list[[1]]), color = "black", fill = NA, size = 0.07) + 
      scale_fill_manual(values = ncl_rwb, drop = F, 
                        labels = diff_labels,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(3.2, 'inches'))) +
      labs(title = paste0(names(diff_list[[1]][[1]]), ' - ', names(diff_list[[1]][[2]]), ' (abs)'),
           fill = fill_labs[3])
    
    p3 = theme_emep_diffmap(p3)
    
    #check if all abs_diff are NA (= test and ref values are identical)
    identical_data = map(p3_list, as_tibble) %>%
      map(drop_na) %>%
      map_dbl(nrow)
    
    if (all(identical_data == 0)) {
      p3 = p3 +
        annotation_custom(grid::roundrectGrob(width = 0.5, height = 0.08)) +
        annotation_custom(grid::textGrob('Data are identical'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        theme(legend.position = 'none')
    }
    
  } else {
    p3 = create_blank_plot() +
      annotation_custom(grid::roundrectGrob(width = 0.9, height = 0.08)) +
      annotation_custom(grid::textGrob('Absolute difference not calculable'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme(legend.position = 'none')
  }
  
  if (length(p4_list) > 0) {
    p4 = ggplot()
    for (i in seq_along(p4_list)) {
      p4 = p4 +
        geom_stars(data = cut(p4_list[[i]], breaks = rel_diff_breaks, include.lowest = T))
    }
    p4 = p4 +
      geom_sf(data = st_crop(boundaries_test, p4_list[[1]]), color = "black", fill = NA, size = 0.07) + 
      scale_fill_manual(values = ncl_rwb, drop = F, 
                        labels = diff_rel_labels,
                        guide = guide_coloursteps(title.position = 'top',
                                                  title.hjust = 0.5, frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barwidth = unit(3.2, 'inches'))) +
      labs(title = paste0(names(diff_list[[1]][[1]]), ' - ', names(diff_list[[1]][[2]]), ' (rel)'),
           fill = fill_labs[4])
    
    p4 = theme_emep_diffmap(p4)
    
    #check if all abs_diff are NA (= test and ref values are identical)
    identical_data = map(p4_list, as_tibble) %>%
      map(drop_na) %>%
      map_dbl(nrow)
    
    if (all(identical_data == 0)) {
      p4 = p4 +
        annotation_custom(grid::roundrectGrob(width = 0.5, height = 0.08)) +
        annotation_custom(grid::textGrob('Data are identical'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
        theme(legend.position = 'none')
    }
  } else {
    p4 = create_blank_plot() +
      annotation_custom(grid::roundrectGrob(width = 0.9, height = 0.08)) +
      annotation_custom(grid::textGrob('Relative difference not calculable'), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
      theme(legend.position = 'none')
  }
  
  pp = ggarrange(ggarrange(p1, p2, ncol = 2,
                           common.legend = T, legend = 'bottom'),
                 ggarrange(p3, p4, ncol = 2, align = 'h'),
                 nrow = 2, align = c('hv')) %>% 
    annotate_figure(top = text_grob(top_title, size = 20, color = '#473C8B'))
  
  pp

}

plot_DSC = function(stars_object, evp_list, palette_dir = PALETTE_DIR, pretty_lab = F, display_summary = T) {
  
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
    ncl_rainbow = read_color(path(palette_dir, 'WhiteBlueGreenYellowRed.rgb')) %>%
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
      geom_stars(data = cut(stars_sub, breaks = dsc_levs, include.lowest = T)) + 
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

plot_mobs_tseries = function(dframe) {
  #plots a time series of a tibble which has obs and mod as columns
  #outputs a list of plots for each 'var' in dframe
  plot_vars = dframe %>% 
    distinct(var) %>% 
    pull()
  
  if( 'subprecip' %in% plot_vars) {
    plot_vars = str_subset(plot_vars, 'subprecip', negate = T)
  }
  
  out_list = vector('list', length(plot_vars))
  for (i in seq_along(plot_vars)) {
    plot_var = plot_vars[i]
    
    if (plot_var == 'precip') {
      tbl_sub = dframe %>% 
        filter(str_detect(var, 'precip'))
    } else {
      tbl_sub = dframe %>% 
        filter(var == plot_var) 
    }
    
    dc_obs = sum(!is.na(tbl_sub$obs)) #calculate observation data capture
    #determine time res and time span of data to select appropriate datetime breaks and labels
    dates = tbl_sub %>% 
      distinct(date) %>% 
      pull()
    data_res = dates %>%
      int_diff() %>% 
      int_length() %>% 
      unique()
    data_span = interval(dates[1], last(dates)) %>% 
      int_length()
    
    if (data_res <= 3600) {
      if (data_span <= 86400 * 7) {
        date_breaks = '12 hours'
        date_labels = '%e %b %H:%M'
      } else if (data_span <= 86400 * 14) {
        date_breaks = '1 day'
        date_labels = '%e %b'
      } else if (data_span <= 86400 * 32) {
        date_breaks = '2 days'
        date_labels = '%e'
      } else {
        date_breaks = '1 week'
        date_labels = '%e %b'
      }
    } else {
      date_breaks = '1 month'
      date_labels = '%b'
    } 
    
    if (plot_var == 'slp') {
      ymin = min(c(tbl_sub$mod, tbl_sub$obs), na.rm = T) 
    } else {
      ymin = 0
    }
    
    tbl_sub2 = tbl_sub %>% 
      mutate(ymin = ymin,
             var = as.character(var)) %>% 
      pivot_longer(c(mod, obs), names_to = 'scenario', values_to = 'conc')
    
    #for correct labelling of the plot strips (won't accept myquicktext) - accepts named vector
    label_vector = OBS_VAR_PARAMS_LIST[[plot_var]][['lab_unit']] %>% 
      set_names(plot_var)
    
    
    if (dc_obs == 0) {
      p = ggplot()
      
    } else {
      obs_tbl = tbl_sub2 %>% 
        filter(scenario == 'obs') %>% 
        distinct(date, .keep_all = T) #precip will have two vars so use just one
      p = ggplot() +
        geom_ribbon(data = obs_tbl,
                    aes(date, ymin = ymin, ymax = conc, fill = scenario), color = NA, alpha = 0.9) +
        scale_fill_manual(values = c(obs = unname(OBS_VAR_PARAMS_LIST[[plot_var]][['var_fill']])),
                          labels = c(obs = 'Observed'), guide = guide_legend(override.aes = list(size = 3)))
    }
    mod_tbl = tbl_sub2 %>% 
      filter(scenario == 'mod') %>% 
      mutate(var2 = var,
             var = plot_var)
    linetype_vals = c('solid', 'dashed') %>%
      set_names(nm = c(plot_var, paste0('sub', plot_var)))
    
    p = p +
      geom_line(data = mod_tbl,
                aes(date, conc, color = scenario, linetype = var2), linewidth = 0.3) +
      scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels, expand = expansion(c(0,0))) +
      scale_color_manual(values = c(mod = unname(OBS_VAR_PARAMS_LIST[[plot_var]][['var_col']])),
                         labels = c(mod = 'Modelled'), guide = guide_legend(override.aes = list(size = .75))) +
      scale_linetype_manual(values = linetype_vals) +
      guides(linetype = 'none') +
      labs(x = NULL,
           y = NULL) +
      facet_wrap(~var, strip.position = 'left',
                 labeller = as_labeller(label_vector, default = label_parsed)) +
      theme_bw() +
      theme(strip.placement = 'outside',
            strip.background = element_rect(fill = unname(OBS_VAR_PARAMS_LIST[[plot_var]][['var_fill']])),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(linewidth = 0.1),
            axis.ticks = element_line(linewidth = 0.1),
            legend.title = element_blank(),
            legend.direction = 'horizontal',
            legend.position = c(0.5, 0.9),
            legend.box = 'horizontal',
            legend.box.background = element_rect(fill = 'transparent', color = 'transparent'),
            legend.background = element_rect(fill = 'transparent'),
            legend.key = element_rect(fill = 'transparent'),
            legend.key.height = unit(0.5, 'lines'))
    
    #adjust y_scale params
    y_params_default = list(breaks = waiver(),
                            labels = function(x) {str_pad(x, width = 4, side = 'left', pad = ' ')},
                            expand = expansion(mult = c(0, 0.15), add = c(0, 0)),
                            limits = NULL)
    y_params_var = list('wd' = list(breaks = c(0, 90, 180, 270, 360),
                                    limits = c(0, 390)),
                        'rh2' = list(breaks = seq(0,100,20),
                                     limits = c(0, 110)))
    
    if (plot_var %in% names(y_params_var)) {
      y_params_intersect = intersect(names(y_params_default), names(y_params_var[[plot_var]]))
      y_params = y_params_default
      for (j in seq_along(y_params_intersect)) {
        y_params[[y_params_intersect[j]]] = y_params_var[[plot_var]][[y_params_intersect[j]]]
      }
    } else {
      y_params = y_params_default
    }
    
    if (dc_obs == 0) {
      if (plot_var == 'precip') {
        p = p +
          labs(caption = '* Insufficient or no observations during the period shown. Data shown are a cummulative sum. ') +
          theme(legend.position = c(0.4, 0.9), #the horizontal positions needs tweaking
                plot.caption = element_text(size = 8))
      } else {
        p = p +
          labs(caption = '* Insufficient or no observations during the period shown.') +
          theme(legend.position = c(0.4, 0.9), #the horizontal positions needs tweaking
                plot.caption = element_text(size = 8))
      }
      
      modelled_min = tbl_sub2 %>%
        filter(scenario == 'mod') %>%
        summarise(mod_min = min(conc, na.rm = T)) %>%
        pull()
      
      if (modelled_min > 0) {
        if (plot_var == 'slp') {
          
        } else {
          p = p +
            scale_y_continuous(breaks = y_params[['breaks']],
                               labels = y_params[['labels']],
                               expand = y_params[['expand']],
                               limits = c(0, ifelse(is.null(y_params[['limits']]), NA, y_params[['limits']][2])))
        }
        
      } else {
        p = p +
          scale_y_continuous(breaks = y_params[['breaks']],
                             labels = y_params[['labels']],
                             expand = y_params[['expand']],
                             limits = c(modelled_min, ifelse(is.null(y_params[['limits']]), NA, y_params[['limits']][2])))
      }
      
    } else {
      p = p +
        scale_y_continuous(breaks = y_params[['breaks']],
                           labels = y_params[['labels']],
                           expand = y_params[['expand']],
                           limits = y_params[['limits']])
      
      if(plot_var != 'precip') {
        #plot a transparent caption so that the plots are the same size with or without caption text
        p = p +
          labs(caption = 'Insufficient or no observations during the period shown.') +
          theme(plot.caption = element_text(color = 'transparent',
                                            size = 8))
      } else {
        #explain dashed line
        p = p +
          labs(caption = '* Dashed line shows modelled data when observations exist. Data shown are a cummulative sum. ') +
          theme(plot.caption = element_text(size = 8))
      } 

    }
    
    out_list[[i]] = p
  }
  names(out_list) = plot_vars
  out_list
}

plot_time_series2 = function(dframe, time_res = 'day', month = NA, dc_threshold = 0.75) {
  #plots time series of observed and modelled concentrations
  #if time_res is 'day' the whole (annual) dataframe is used but only days with data
  #capture >= dc_threshold are plotted
  #if time_res is 'hour', only one month which must be specified by the 'month' kwarg is plotted
  #both abbreviated and full month names are allowed
  
  var = unique(dframe$var)
  dc_obs = sum(!is.na(dframe$obs)) #calculate observation data capture
  
  if (time_res == 'day') {
    dframe = dframe %>% 
      timeAverage(data.thresh = dc_threshold * 100, type = c('code', 'var')) %>% 
      ungroup()
    date_breaks = '1 month'
    date_labels = '%b'
    plt_name = str_c(unique(dframe$code), var, 'year', sep = '_')
  } else if (time_res == 'hour' & str_to_title(month) %in% c(month.abb, month.name)) {
    month_num = coalesce(match(month, month.name), match(month, month.abb))
    dframe = dframe %>% 
      filter(month(date) == month_num)
    date_breaks = '2 days'
    date_labels = '%e'
    plt_name = str_c(unique(dframe$code), var, month.abb[month_num], sep = '_')
  } else {
    stop('Time_res must be either "day" or "hour". If the latter a month name (e.g. "Jan" or "January") must be provided.')
  }
  
  
  df2 = dframe %>% 
    mutate(ymin = 0,
           var = as.character(var)) %>% 
    pivot_longer(obs:mod, names_to = 'scenario', values_to = 'conc')
  
  #for correct labelling of the plot strips (won't accept myquicktext) - accepts named vector
  label_vector = OBS_VAR_PARAMS_LIST[[var]][['lab_unit']] %>% 
    set_names(var)
  
  if (dc_obs == 0) {
    p = ggplot()
    
  } else {
    p = ggplot() +
      geom_ribbon(data = filter(df2, scenario == 'obs'),
                  aes(date, ymin = ymin, ymax = conc, fill = scenario), color = NA, alpha = 0.9) +
      scale_fill_manual(values = c(obs = unname(OBS_VAR_PARAMS_LIST[[var]][['var_fill']])),
                        labels = c(obs = 'Observed'), guide = guide_legend(override.aes = list(size = 3)))
  }
  
  p = p +
    geom_line(data = filter(df2, scenario == 'mod'),
              aes(date, conc, color = scenario), size = 0.3) +
    scale_x_datetime(date_breaks = date_breaks, date_labels = date_labels, expand = expansion(c(0,0))) +
    scale_color_manual(values = c(mod = unname(OBS_VAR_PARAMS_LIST[[var]][['var_col']])),
                       labels = c(mod = 'Modelled'), guide = guide_legend(override.aes = list(size = .75))) +
    labs(x = NULL,
         y = NULL) +
    facet_wrap(~var, strip.position = 'left',
               labeller = as_labeller(label_vector, default = label_parsed)) +
    theme_bw() +
    theme(strip.placement = 'outside',
          strip.background = element_rect(fill = unname(OBS_VAR_PARAMS_LIST[[var]][['var_fill']])),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.1),
          axis.ticks = element_line(linewidth = 0.1),
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
      labs(caption = '* Insufficient or no observations during the period shown.') +
      theme(legend.position = c(0.4, 0.9), #the horizontal positions needs tweaking
            plot.caption = element_text(size = 8))
    
    modelled_min = df2 %>% 
      filter(scenario == 'mod') %>% 
      summarise(mod_min = min(conc, na.rm = T)) %>% 
      pull()
    
    if (modelled_min > 0) {
      p = p +
        scale_y_continuous(labels = function(x) {str_pad(x, width = 4, side = 'left', pad = ' ')},
                           expand = expansion(mult = c(0, 0.15), add = c(0, 0)),
                           limits = c(0, NA))
    } else {
      p = p +
        scale_y_continuous(labels = function(x) {str_pad(x, width = 4, side = 'left', pad = ' ')},
                           expand = expansion(mult = c(0, 0.15), add = c(0, 0)))
    }
    
  } else {
    p = p +
      scale_y_continuous(labels = function(x) {str_pad(x, width = 4, side = 'left', pad = ' ')},
                         expand = expansion(mult = c(0, 0.15), add = c(0, 0))) +
      labs(caption = 'Insufficient or no observations during the period shown.') +
      theme(plot.caption = element_text(color = 'transparent',
                                        size = 8))
  }
  
  p
}

plot_mobs_sites_map = function(sites_df) {
  pal = colorFactor(c('#7570b3', '#1b9e77', '#d95f02', '#e7298a', '#666666' ),
                    domain = c('Urban', 'Rural', 'Industrial', 'Road', 'Unknown'), ordered = T)
  
  m = leaflet(data = sites_df) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    addCircleMarkers(color = ~pal(site_type_grp), opacity = 0.8,
                     radius = 5, fill = F, weight = 3, popup = ~site, label = ~code) %>%
    addLegend('topright', pal = pal, values = ~site_type_grp,
              opacity = 1, title = 'Site Type Group')
  
  m
}

#this has been taken from stackoverflow regarding making the legend key to be round circles like the symbols
# it needs more work os currently not used
addLegendCustom <- function(map, colors, labels, sizes, opacity = 1){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}


plot_mobs_sites_map2 = function(sites_df, basemap = c('world_topo', 'satellite', 'terrain'), colours = '#7570b3',
                               group_column = NA, legend_label = '', legend_title = 'Legend') {
  #if group_column isn't na then colours are is a named vector of colours with names the values in group_column
  info_vars0 = c('code', 'station', 'site', group_column, 'network', 'elev(m)', 'year') 
  info_vars = info_vars0 %>% 
    str_to_sentence() %>% 
    set_names(nm = info_vars0)
  info_vars = base::intersect(names(info_vars), names(sites_df)) %>% 
    info_vars[.]
  
  content = map2(info_vars, names(info_vars), ~str_c('<b>', .x, ':</b> ', sites_df[[.y]])) %>% 
    transpose() %>% 
    stringi::stri_join_list(sep = '<br/>')
  
  
  m = leaflet(data = sites_df)
  
  #select tile type
  if (basemap[1] == 'satellite') {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldImagery)
  } else if (basemap[1] == 'terrain') {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldTerrain)
  } else {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldTopoMap)
  }
  
  
  if (is.na(group_column)) {
    m = m %>% 
      addCircleMarkers(color = colours[1], opacity = 0.8, popup = content,
                       radius = 5, fill = F, weight = 3) %>% 
      addLegend('topright', colors = colours[1],
                opacity = 1, labels = legend_label, title = legend_title)
  } else {
    pal = colorFactor(palette = colours, domain = names(colours), ordered = T)
    m = m %>% 
      addCircleMarkers(color = pal(sites_df[[group_column]]), opacity = 0.8, popup = content,
                       radius = 5, fill = F, weight = 3) %>%
      addLegend('topright', pal = pal, values = sites_df[[group_column]],
                opacity = 1, title = legend_title)
  }
  
  m
}

plot_mobs_stats_map = function(sites_df, mobs_stat = 'MB', basemap = c('world_topo', 'satellite', 'terrain'),
                                group_column = NULL, legend_title = 'Legend') {
  
  sites_df = sites_df %>%
    drop_na({{mobs_stat}})
  
  #create leaflet popups with appropriate capitalisation of letters - it's convoluted but works
  stat_vars = c('n', 'FAC2', 'MB', 'NMB', 'RMSE', 'r', 'p', 'P')
  info_vars0 = c('code', 'station', 'site', group_column, 'network', 'elev(m)', 'year') 
  info_vars = info_vars0 %>% 
    str_to_sentence() %>% 
    c(stat_vars) %>% 
    set_names(nm = c(info_vars0, stat_vars))
  info_vars = base::intersect(names(info_vars), names(sites_df)) %>% 
    info_vars[.]
  
  content = map2(info_vars, names(info_vars), ~str_c('<b>', .x, ':</b> ', sites_df[[.y]])) %>% 
    transpose() %>% 
    stringi::stri_join_list(sep = '<br/>')
  
  m = leaflet()
  
  #select tile type
  if (basemap[1] == 'satellite') {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldImagery)
  } else if (basemap[1] == 'terrain') {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldTerrain)
  } else {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldTopoMap)
  }
  
  #get breaks, colors and limits for plotting with the help of ggplot :)
  p = ggplot(sites_df) +
    geom_point(aes(longitude, latitude, color = .data[[mobs_stat]]))
  if (mobs_stat %in% c('MB', 'NMB', 'r')) {
    p = p +
      scale_color_steps2(low = '#053061', high = '#67001f', nice.breaks = T)
  } else {
    p = p +
      scale_color_steps(low = '#fff7ec', high = '#7f0000')
  }
  
  p_build = ggplot_build(p)
  p_scale = p_build$plot$scales$get_scales("colour")
  p_breaks = p_scale$get_breaks()
  p_colors = p_scale$palette.cache
  p_limits = p_scale$limits
  
  p_breaks = c(p_limits[1], p_breaks, p_limits[2])
  p_labels = str_c(na.omit(lag(p_breaks)), na.omit(lead(p_breaks)), sep = ' - ')
  p_breaks[1] = p_breaks[1] - abs(p_breaks[1]*0.001)
  p_breaks[length(p_breaks)] = p_breaks[length(p_breaks)] * 1.001
  
  pal = colorFactor(palette = rev(p_colors), domain = rev(p_labels), ordered = T)
  
  sites_df = sites_df %>%
    mutate(stat_cut = cut(.data[[mobs_stat]], right= F,  breaks = p_breaks, labels = p_labels, include.lowest = T))
  
  m = m %>% 
    addCircleMarkers(data = sites_df, fillColor = ~pal(stat_cut), fillOpacity = 1, opacity = 1,
                     stroke = T, color = 'black', radius = 5, weight = 0.1, popup = content) %>%
    addLegend('topright', pal = pal, values = sites_df$stat_cut, opacity = 1, title = legend_title)
  m
}

plot_mobs_stats_map2_WIP = function(sites_df, mobs_stat = 'MB', basemap = c('world_topo', 'satellite', 'terrain'),
                               group_column = NULL, legend_title = 'Legend') {
  #WIP for rain
  
  #create leaflet popups with appropriate capitalisation of letters - it's convoluted but works
  stat_vars = c('n', 'FAC2', 'MB', 'NMB', 'RMSE', 'r', 'p', 'P')
  info_vars0 = c('code', 'station', 'site', group_column, 'network', 'elev(m)', 'year') 
  info_vars = info_vars0 %>% 
    str_to_sentence() %>% 
    c(stat_vars) %>% 
    set_names(nm = c(info_vars0, stat_vars))
  info_vars = base::intersect(names(info_vars), names(sites_df)) %>% 
    info_vars[.]
  
  content = map2(info_vars, names(info_vars), ~str_c('<b>', .x, ':</b> ', sites_df[[.y]])) %>% 
    transpose() %>% 
    stringi::stri_join_list(sep = '<br/>')
  
  m = leaflet()
  
  #select tile type
  if (basemap[1] == 'satellite') {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldImagery)
  } else if (basemap[1] == 'terrain') {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldTerrain)
  } else {
    m = m %>% 
      addProviderTiles(providers$Esri.WorldTopoMap)
  }
  
  #get breaks, colors and limits for plotting with the help of ggplot :)
  p = ggplot(sites_df) +
    geom_point(aes(longitude, latitude, color = .data[[mobs_stat]]))
  if (mobs_stat %in% c('MB', 'NMB', 'r')) {
    p = p +
      scale_color_steps2(low = '#053061', high = '#67001f', nice.breaks = T)
  } else {
    p = p +
      scale_color_steps(low = '#fff7ec', high = '#7f0000')
  }
  
  p_build = ggplot_build(p)
  p_scale = p_build$plot$scales$get_scales("colour")
  p_breaks = p_scale$get_breaks()
  p_colors = p_scale$palette.cache
  p_limits = p_scale$limits
  
  p_breaks = c(p_limits[1], p_breaks, p_limits[2])
  p_labels = str_c(na.omit(lag(p_breaks)), na.omit(lead(p_breaks)), sep = ' - ')
  p_breaks[1] = p_breaks[1] - abs(p_breaks[1]*0.001)
  p_breaks[length(p_breaks)] = p_breaks[length(p_breaks)] * 1.001
  
  sites_df = sites_df %>%
    drop_na({{mobs_stat}}) %>% 
    mutate(stat_cut = cut(.data[[mobs_stat]], right= F,  breaks = p_breaks, labels = p_labels, include.lowest = T))
  
  pal = colorFactor(palette = rev(p_colors), domain = rev(p_labels), ordered = T)
  
  m = m %>% 
    addCircleMarkers(data = sites_df, fillColor = ~pal(stat_cut), fillOpacity = 1, opacity = 1,
                     stroke = T, color = 'black', radius = 5, weight = 0.1, popup = content) %>%
    addLegend('topright', pal = pal, values = sites_df$stat_cut, opacity = 1, title = legend_title)
  m
}

mobs_tseries_to_pdf = function(mobs_tbl, out_dir = getwd(), fname_out = NULL,
                               run_title_info = NULL, ppp = 4, plot_all_vars = T) {
  #plots tseries from a mobs dataframe (only one site allowed) and saves the plots in a pdf
  #fname_out if not given is constructed from a combination of site name, code and year of mobs
  #info_columns are names of columns in mobs_tbl with additional site info that will be inserted in brackets 
  #in the pdf page titles
  #run_title_info is a string for the PDF output title
  #ppp = plots per page (max 4 recommended)
  #plot_all_vars if FALSE only plots observed vars
  
  info_tbl = mobs_tbl %>% 
    ungroup() %>% 
    select(-any_of(c('data', 'month'))) %>% 
    distinct()
  
  if ('geometry' %in% names(info_tbl)) {
    info_tbl = info_tbl %>% 
      st_as_sf() %>% 
      mutate(longitude = str_c('lon: ', round(st_coordinates(.)[ , 1], 4)),
             latitude = str_c('lat: ', round(st_coordinates(.)[ , 2], 4))) %>% 
      st_drop_geometry()
  }
  
  mobs_year = mobs_tbl %>% 
    unnest(cols = data) %>% 
    ungroup() %>% 
    distinct(year(date)) %>% 
    pull()

  mobs_tbl = mobs_tbl %>% 
    mutate(plots = map(data, plot_mobs_tseries),
           var = map(plots, names)) %>% 
    unnest(cols = c(plots, var)) %>% 
    arrange(month, match(var, names(OBS_VAR_PARAMS_LIST))) %>% 
    ungroup()
  
  #extract year from data if not already pulled out
  if (!'year' %in% names(info_tbl)) {
    info_tbl = info_tbl %>% 
      mutate(year = mobs_year)
  }
  
  if (is.null(fname_out)) {
    fname_out = str_c(c(str_replace_all(textclean::replace_non_ascii(info_tbl[['site']]), "[^[:alnum:]]", ""),
                                str_replace_all(textclean::replace_non_ascii(info_tbl[['station']]), "[^[:alnum:]]", ""),
                                info_tbl[['code']],
                                info_tbl[['year']]),
                      collapse = ' ') %>% 
      str_squish() %>% 
      str_replace_all(' ', '_')
    
    #if only measured vars plotted add _p suffix to file name
    if (plot_all_vars == F) {
      fname_out = paste0(fname_out, '_p')
    }
  }
  
  plot_pth_out = path(out_dir, fname_out, ext = 'pdf')
  
  #create page titles for pdf
  
  extra_info_tbl = info_tbl %>% 
    select(-any_of(c('code', 'site', 'station', 'year'))) %>% 
    discard(~all(is.na(.x)))
  if (length(extra_info_tbl) > 0) { #put brackets around info_columns if they exist
    extra_info = extra_info_tbl %>% 
      unite(col = 'dummy', sep = ', ') %>% 
      pull() %>% 
      na.omit(.)
    extra_info = str_glue("({extra_info})")
  } else {
    extra_info = ''
  }
  # if no site or station name is provided use site/station code in title
  if(!any(c('site', 'station') %in% names(info_tbl))) {
    s_name = info_tbl[['code']]
  } else {
    s_name = NULL
  }

  page_daily_title = str_c(c('Daily means at ',
                             textclean::replace_non_ascii(info_tbl[['site']]), 
                             textclean::replace_non_ascii(info_tbl[['station']]),
                             s_name,
                             extra_info,
                             ' in ',
                             info_tbl[['year']]),
                           collapse = ' ') %>% 
    str_squish() %>% 
    str_wrap(width = 80)
  
  page_hourly_title = map_chr(str_c(' ', month.name, ' '),
                              ~str_c(c('Hourly data at ',
                                     textclean::replace_non_ascii(info_tbl[['site']]), 
                                     textclean::replace_non_ascii(info_tbl[['station']]),
                                     s_name,
                                     extra_info,
                                     ' in ',
                                     .x,
                                     info_tbl[['year']]), collapse = ' ') %>% 
                                str_squish()) %>% 
    map_chr(str_wrap, width = 80)
  
  #if run_title_info provided, add it to the page titles
  if (!is.na(run_title_info)) {
    page_daily_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_daily_title)
    page_hourly_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_hourly_title)
  }
  
  if (plot_all_vars == T) {
    n_vars = n_distinct(mobs_tbl$var)
    page_titles = rep(c(page_daily_title, page_hourly_title),
                      each = (n_vars - 1) %/% ppp + 1)
    
    #sort vars so that they are plotted in order of polls in OBS_VAR_PARAMS_LIST 
    plot_vars = unique(mobs_tbl$var)
    sorted_plot_vars = plot_vars[order(match(plot_vars, names(OBS_VAR_PARAMS_LIST)))]
    
    #create a base vector of vars 
    if (n_vars %% ppp == 0) {
      base_var_vector = sorted_plot_vars
    } else {
      base_var_vector = c(sorted_plot_vars, rep(NA_character_, times = ppp * (n_vars %/% ppp + 1) - n_vars))
    }
    
  } else { #NEEDS SORTING OUT
    # measured_vars = plt %>% 
    #   drop_na() %>% 
    #   distinct(pollutant) %>% 
    #   pull()
    # n_polls = length(measured_polls)
    # 
    # page_titles = rep(c(page_daily_title, page_hourly_title),
    #                   each = (n_polls - 1) %/% ppp + 1)
    # sorted_measured_polls = measured_polls[order(match(measured_polls, names(OBS_VAR_PARAMS_LIST)))]
    # base_poll_vector = c(sorted_measured_polls, rep(NA_character_, times = ppp * (n_polls %/% ppp + 1) - n_polls))
    
  }
  
  plot_list = mobs_tbl %>% 
    split(.$month) %>%
    map(~right_join(.x, tibble(var = base_var_vector), by = 'var')) %>% 
    bind_rows() %>% 
    pull(plots)

  # remove any non-ggplot objects from the list
  plot_list = plot_list[map_lgl(plot_list, ~ inherits(.x, "gg"))]

  
  export = marrangeGrob(grobs = plot_list, nrow = ppp, ncol = 1, top = substitute(page_titles[g]))
  ggsave(filename = plot_pth_out, export, paper = 'a4', height = 10, width = 7)
  
}


mobs_tseries_to_pdf_old = function(plotlist_tbl, out_dir = getwd(), fname_out = NA,
                               run_title_info = NA, ppp = 4, plot_all_vars = T) {
  
  plt = plotlist_tbl %>% 
    unnest(c(plots, var))
  
  
  #extract all meta data for fname_out
  info_vars = c('code', 'station', 'network', 'site', 'site_type_grp', 'year') %>% 
    set_names()
  missing_info_vars = base::setdiff(info_vars, names(plt)) %>% 
    set_names() %>% 
    map_chr(~'')
  
  info_vars = base::intersect(info_vars, names(plt)) %>% 
    set_names()
  for (i in seq_along(info_vars)) {
    info_vars[i] = unique(plt[[info_vars[i]]])
  }
  info_vars = c(info_vars, missing_info_vars)
  
  if ('year' %in% names(missing_info_vars)) {
    info_vars['year'] = plt %>% 
      filter(month == 1) %>% 
      pull(data) %>% 
      .[[1]] %>% 
      distinct(year(date)) %>% 
      pull() %>% 
      as.character()
  }
  if (is.na(fname_out)) {
    fname_out = str_c(str_replace_all(textclean::replace_non_ascii(info_vars['site']), "[^[:alnum:]]", ""),
                      str_replace_all(textclean::replace_non_ascii(info_vars['station']), "[^[:alnum:]]", ""),
                      info_vars['code'],
                      info_vars['site_type_grp'],
                      info_vars['year'], sep = ' ') %>% 
      str_squish() %>% 
      str_replace_all(' ', '_')
    
    #if only measured vars plotted add _p suffix to file name
    if (plot_all_vars == F) {
      fname_out = paste0(fname_out, '_p')
    }
  }
  
  plot_pth_out = path(out_dir, fname_out, ext = 'pdf')
  
  #create page titles for pdf
  if (info_vars[['site_type_grp']] != '') { #put brackets around site_type_grp if it exists
    info_vars[['site_type_grp']] = str_glue("({info_vars[['site_type_grp']]})")
  }
  # if no site or station name is provided use site/station code in title
  s_name = ifelse(length(info_vars[c('site', 'station')] %>% .[nzchar(.)]) == 0,
                  info_vars['code'],
                  info_vars[c('site', 'station')] %>% .[nzchar(.)] %>% names()) 
  page_daily_title = str_c('Daily means at ',
                           textclean::replace_non_ascii(info_vars['site']), 
                           textclean::replace_non_ascii(info_vars['station']),
                           s_name,
                           info_vars[['site_type_grp']],
                           '\n in ',
                           info_vars['year'], sep = ' ') %>% 
    str_squish()
  
  page_hourly_title = map_chr(str_c(' ', month.name, ' '),
                              ~str_c('Hourly data at ',
                                     textclean::replace_non_ascii(info_vars['site']), 
                                     textclean::replace_non_ascii(info_vars['station']),
                                     s_name,
                                     info_vars[['site_type_grp']],
                                     '\n in ',
                                     .x,
                                     info_vars['year'], sep = ' ') %>% 
                                str_squish())
  
  #if run_title_info provided, add it to the page titles
  if (!is.na(run_title_info)) {
    page_daily_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_daily_title)
    page_hourly_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_hourly_title)
  }
  
  if (plot_all_vars == T) {
    n_vars = n_distinct(plt$var)
    page_titles = rep(c(page_daily_title, page_hourly_title),
                      each = (n_vars - 1) %/% ppp + 1)
    
    #sort vars so that they are plotted in order of polls in OBS_VAR_PARAMS_LIST 
    plot_vars = unique(plt$var)
    sorted_plot_vars = plot_vars[order(match(plot_vars, names(OBS_VAR_PARAMS_LIST)))]
    
    #create a base vector of vars 
    if (n_vars %% ppp == 0) {
      base_var_vector = sorted_plot_vars
    } else {
      base_var_vector = c(sorted_plot_vars, rep(NA_character_, times = ppp * (n_vars %/% ppp + 1) - n_vars))
    }
    
  } else { #NEEDS SORTING OUT
    # measured_vars = plt %>% 
    #   drop_na() %>% 
    #   distinct(pollutant) %>% 
    #   pull()
    # n_polls = length(measured_polls)
    # 
    # page_titles = rep(c(page_daily_title, page_hourly_title),
    #                   each = (n_polls - 1) %/% ppp + 1)
    # sorted_measured_polls = measured_polls[order(match(measured_polls, names(OBS_VAR_PARAMS_LIST)))]
    # base_poll_vector = c(sorted_measured_polls, rep(NA_character_, times = ppp * (n_polls %/% ppp + 1) - n_polls))
    
  }
  
  plot_list = plt %>% 
    arrange(month, match(var, names(OBS_VAR_PARAMS_LIST))) %>% 
    ungroup() %>% 
    split(.$month) %>%
    map(~right_join(.x, tibble(var = base_var_vector), by = 'var')) %>% 
    bind_rows() %>% 
    pull(plots)
  
  export = marrangeGrob(grobs = plot_list, nrow = ppp, ncol = 1, top = substitute(page_titles[g]))
  ggsave(filename = plot_pth_out, export, paper = 'a4', height = 10, width = 7)
  
}

site_time_series_pdf3 = function(merged_df_long, auto_sites_df, merged_df_pth = NA, out_dir = '.',
                                 plot_all_vars = T, ppp = 4, run_title_info = '') {
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
  
  fname_out = str_c(str_replace_all(textclean::replace_non_ascii(site_meta$site), "[^[:alnum:]]", ""), site_meta$code,
                    site_meta$site_type_grp, site_meta$year, sep = '_')
  #if only measured vars plotted add _p suffix to file name
  if (plot_all_vars == F) {
    fname_out = paste0(fname_out, '_p')
  }
  
  plot_pth_out = path(out_dir, fname_out, ext = 'pdf')
  
  page_daily_title = str_c('Daily concentrations at ', textclean::replace_non_ascii(site_meta$site), ' site (',
                           site_meta$site_type_grp, ')\n in ', site_meta$year, sep = '')
  
  page_hourly_title = map_chr(str_c(' ', month.name, ' '), ~str_c('Hourly concentrations at ', textclean::replace_non_ascii(site_meta$site),
                                                                  ' site (', site_meta$site_type_grp, ')\n in', .x, site_meta$year, sep = ''))

  #if run_title_info provided, add it to the page titles
  page_daily_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_daily_title)
  page_hourly_title = str_c(str_wrap(run_title_info, 50), '\n\n', page_hourly_title)
  
  if (plot_all_vars == T) {
    n_vars = n_distinct(merged_df_long$var)
    page_titles = rep(c(page_daily_title, page_hourly_title),
                      each = (n_polls - 1) %/% ppp + 1)
    
    #sort vars so that they are plotted in order of vars in OBS_VAR_PARAMS_LIST 
    plot_vars = unique(merged_df_long$var)
    sorted_plot_vars = plot_vars[order(match(plot_vars, names(OBS_VAR_PARAMS_LIST)))]
    base_var_vector = c(sorted_plot_vars, rep(NA_character_, times = ppp * (n_vars %/% ppp + 1) - n_vars))
    
  } else {
    measured_vars = merged_df_long %>% 
      drop_na() %>% 
      distinct(var) %>% 
      pull()
    n_vars = length(measured_vars)
    
    page_titles = rep(c(page_daily_title, page_hourly_title),
                      each = (n_vars - 1) %/% ppp + 1)
    sorted_measured_vars = measured_vars[order(match(measured_vars, names(OBS_VAR_PARAMS_LIST)))]
    base_var_vector = c(sorted_measured_vars, rep(NA_character_, times = ppp * (n_vars %/% ppp + 1) - n_vars))
    
  }
  
  var_vector = rep(base_var_vector, times = length(unique(page_titles)))
  averaging_vector = rep(c('day', rep('hour', 12)), each = length(base_var_vector))
  month_vector = rep(c(NA_character_, month.abb), each = length(base_var_vector))
  
  blank_plot = create_blank_plot() #to fill in gaps on page
  
  plot_list = vector('list', length(var_vector))
  
  for (j in 1:length(var_vector)) {
    if (is.na(var_vector[j])) {
      plot_list[[j]] = blank_plot
    } else {
      merged_data_sub = merged_df_long%>% 
        filter(var == !!var_vector[j])
      p = plot_time_series2(dframe = merged_data_sub, time_res = averaging_vector[j],
                            month = month_vector[j])
      plot_list[[j]] = p
    }
    
  }
  
  export = marrangeGrob(grobs = plot_list, nrow = ppp, ncol = 1, top = substitute(page_titles[g]))
  ggsave(filename = plot_pth_out, export, paper = 'a4', height = 10, width = 7)
}


plot_annual_scatter = function(mobs_ameans, colours = '#7570b3',
                               group_column = NA, facet = T, fix_scales = F, legend_title = '') {
  #plots modelled vs observed scatter plots for data obtained by collate_mod_obs function
  # mobs_ameans is a dataframe with with annual mobs means
  
  
  #calculate max concentrations to determine x and y upper limits for plotting
  mobs_extremes = mobs_ameans %>% 
    group_by(var) %>% 
    pivot_longer(cols = c(mod, obs), names_to = 'scenario', values_to = 'value') %>% 
    summarise(max_val = max(value, na.rm = T),
              min_val = min(value, na.rm = T)) %>% 
    ungroup()
  mobs_max = mobs_extremes %>% 
    pull(max_val)
  mobs_min = mobs_extremes %>% 
    pull(min_val)
  
  #determine plot title
  if (unique(mobs_ameans$var) %in% c('precip', 'subprecip')) {
    p_title = paste0(OBS_VAR_PARAMS_LIST[[unique(mobs_ameans$var)]][['lab_unit']])
  } else {
    p_title = parse(text = paste0('Annual~Mean~', OBS_VAR_PARAMS_LIST[[unique(mobs_ameans$var)]][['lab_unit']]))
  }
  
  p = ggplot() +
    geom_abline(slope = 0.5, intercept = 0, linetype = 'longdash', linewidth = 0.3) +
    geom_abline(slope = 2, intercept = 0, linetype = 'longdash', linewidth = 0.3) +
    geom_abline(slope = 1, intercept = 0, linetype = 'solid', linewidth = 0.3)
  
  if (is.na(group_column)) {
    p = p +
      geom_point(data = mobs_ameans, mapping = aes(obs, mod), color = colours, shape = 1, alpha = 0.8, size = 2) +
      scale_x_continuous(limits = c(mobs_min - abs(mobs_min* 0.02), mobs_max * 1.02)) +
      scale_y_continuous(limits = c(mobs_min - abs(mobs_min* 0.02), mobs_max * 1.02))
    theme_extra = theme()
    
  } else {
    mobs_ameans = mobs_ameans %>% 
      mutate('{group_column}' := factor(.data[[group_column]], levels = names(colours), ordered = T))
    p = p +
      geom_point(data = mobs_ameans, mapping = aes(obs, mod, color = .data[[group_column]]), shape = 1, alpha = 0.8, size = 2) +
      scale_color_manual(values = colours,
                         guide = guide_legend(override.aes = list(alpha = 1, size = 3))) 
    
    if (facet == T) {
      if (fix_scales == T) {
        p = p +
          facet_wrap(~.data[[group_column]], ncol = 4) +
              scale_x_continuous(limits = c(mobs_min - abs(mobs_min* 0.02), mobs_max * 1.02)) +
              scale_y_continuous(limits = c(mobs_min - abs(mobs_min* 0.02), mobs_max * 1.02))
        
      } else {
        mobs_max = mobs_ameans %>% 
          group_by(var, site_type_grp) %>% 
          pivot_longer(cols = c(mod, obs), names_to = 'scenario', values_to = 'value') %>% 
          summarise(max_val = max(value, na.rm = T)) %>% 
          ungroup()
        
        grp_elements = as.character(mobs_max[[group_column]])
        
        plist = list()
        for (i in seq_along(grp_elements)) {
          max_val = mobs_max %>% 
            filter(.data[[group_column]] == grp_elements[i]) %>% 
            pull(max_val)
          
          p1 = ggplot() +
            geom_abline(slope = 0.5, intercept = 0, linetype = 'longdash', linewidth = 0.3) +
            geom_abline(slope = 2, intercept = 0, linetype = 'longdash', linewidth = 0.3) +
            geom_abline(slope = 1, intercept = 0, linetype = 'solid', linewidth = 0.3) +
            geom_point(data = filter(mobs_ameans, .data[[group_column]] == grp_elements[i]),
                       mapping = aes(obs, mod, color = .data[[group_column]]), shape = 1, alpha = 0.8, size = 2) +
            scale_color_manual(values = colours,
                               guide = guide_legend(override.aes = list(alpha = 1, size = 3))) +
            scale_x_continuous(limits = c(mobs_min - abs(mobs_min* 0.02), max_val * 1.02)) +
            scale_y_continuous(limits = c(mobs_min - abs(mobs_min* 0.02), max_val * 1.02)) +
            facet_wrap(~.data[[group_column]]) +
            labs(x = NULL,
                 y = NULL,
                 color = 'Site Type') +
            theme_bw() +
            theme(strip.background = element_rect(fill = unname(OBS_VAR_PARAMS_LIST[[unique(mobs_ameans$var)]][['var_fill']])),
                  strip.text = element_text(size = 9),
                  aspect.ratio = 1,
                  axis.text = element_text(size = 8),
                  axis.title = element_text(size = 9),
                  legend.position = 'none',
                  panel.grid.major = element_line(linewidth = 0.02),
                  panel.grid.minor = element_line(linewidth = 0.01),
                  plot.title = element_text(hjust = 0.5, size = 11))# color = '#473C8B'))
          plist[[i]] = p1
            
        }
        plist = ggarrange(plotlist = plist, nrow = 1) %>% 
          annotate_figure(top = text_grob(parse(text = p_title), size = 11),
                          left = text_grob('Modelled', rot = 90, size = 9),
                          bottom = text_grob('Observed', size = 9))
        return(plist)
      } 
      
    } else {
      theme_extra = theme(legend.position = 'right')
    }
  
  }
  
  p = p +
    labs(title = parse(text = p_title),
         x = myquickText(paste0('Observed')),
         y = myquickText(paste0('Modelled')),
         color = 'Site Type') +
    theme_bw() +
    theme(strip.background = element_rect(fill = unname(OBS_VAR_PARAMS_LIST[[unique(mobs_ameans$var)]][['var_fill']])),
          strip.text = element_text(size = 9),
          aspect.ratio = 1,
          axis.text = element_text(size = 8),
          axis.title = element_text(size = 9),
          legend.position = 'none',
          legend.text = element_text(size = 9),
          legend.title = element_text(size = 10),
          panel.grid.major = element_line(linewidth = 0.02),
          panel.grid.minor = element_line(linewidth = 0.01),
          plot.title = element_text(hjust = 0.5, size = 11)) +
    theme_extra
  
  p
}


plot_budget_diff = function(budget_dframe, threshold = 5) {
  #plots difference in budget vars between two EMEP runs
  # data must be given in a wide dataframe

  budget = budget_dframe %>% 
    filter(abs_diff != 0) %>% 
    select(-test, -ref)
  
  if (nrow(budget) == 0) {
    return(NULL)
  } else {
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
      geom_hline(yintercept = 0, color = 'black', linewidth = 0.2) +
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
            panel.grid.major = element_line(linewidth = 0.1),
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
          panel.border = element_rect(color = 'black', linewidth = 0.3, fill = NA),
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
          panel.border = element_rect(color = 'black', linewidth = 0.3, fill = NA)
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

show_cols <- function(colors_list, margin = 8, fontsize = 1, family = NULL) {
  if (!is.list(colors_list)) colors_list = list(colors_list)
  names = names(colors_list)
  n <- length(colors_list)
  if (is.null(names)) {
    names = if (n == 1) {
      margin = 3
      ""
    } else seq_len(n)
  }
  
  # par(mfrow = c(n, 1), mar = rep(0.25, 4))
  height <- 0.05
  old <- par(mfrow = c(n, 1), mar = c(height, 0.25, height, margin), mgp = c(0, 0, 0))
  on.exit(par(old))
  
  suppressWarnings({
    for(i in seq_along(colors_list)) {
      color = colors_list[[i]]
      name  = names[i]
      
      ncol <- length(color)
      barplot(rep(1, ncol),
              yaxt = "n",
              space = c(0, 0), border = NA,
              col = color,
              xaxs = "i"
              # xlim = c(2, ncol)
              # , ylab = name
      )
      if (n > 10) abline(v = ncol * .05, col = "white", lwd = 0.7)
      # , family = "Times"
      title = sprintf(" %s (n = %s)", paste0(rep(" ", 0), name), ncol)
      mtext(title, 4, las = 1, cex = fontsize, adj = 0, family = family) # , family = "Arial"
      # text(-4, 0.5, name, adj = c(0, 0.5))
      # sprintf("\n\n%s", name)
    }
  })
  invisible()
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

select_file_from_dir = function(EMEP_dir, fname = 'fullrun') {
  #safely extract file name from directory (for comp maps calculations)
  #returns NA if EMEP_dir is NA without crashing
  if(is.null(EMEP_dir)) {
    out = NULL
  } else {
    out = dir_ls(EMEP_dir) %>% 
      str_subset(fname)
  }
  out
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
  site_type_grp = case_when((is.na({{site_type}}) | {{site_type}} == '') ~ 'Unknown',
                            {{site_type}} %in% c("Urban Traffic","roadside","Roadside",
                                         "Kerbside","kerbside","traffic_urban") == T ~ 'Road',
                            {{site_type}} %in% c("Urban Background","Suburban Background", "urban",
                                                 "urban_background","suburban","urban_centre","Suburban","background_urban","background_suburban") == T ~ 'Urban',
                            #site_type %in% c('airport', 'Airport') == T ~ 'Airport',
                            {{site_type}} %in% c('rural', 'Rural Background','background_rural','background_rural_regional') == T ~ 'Rural',
                            {{site_type}} %in% c('Suburban Industrial', 'Urban Industrial', 'urban_industrial', 'airport', 'Airport',
                                                 'Rural Industrial', 'Industrial','industrial_suburban') == T ~ 'Industrial',
                            T ~ NA_character_)
  site_type_grp
}

mobs_to_long = function(mobs_dframe) {
  #converts wide format of mobs_dframe to long format
  mobs_long = mobs_dframe %>% 
    pivot_longer(c(-date, -code), names_to = 'var', values_to = 'value') %>% 
    separate(var, into = c('scenario', 'var'), sep = '_') %>% 
    pivot_wider(id_cols = c(date, code, var), names_from = scenario, values_from = value)
  mobs_long
}

format_maps_page_title = function(outer_test_pth = NULL, outer_ref_pth = NULL,
                                  inner_test_pth = NULL, inner_ref_pth = NULL,
                                  run_labels = c('test_label', 'ref_label')) {
  
  ### outputs EMEP run (and domain) name for page titles in pdf outputs
  
  # use just the filename portion of the path
  runs = list(outer_test_pth, outer_ref_pth, inner_test_pth, inner_ref_pth)
  
  if (length(compact(runs)) == 0) {
    stop('No EMEP file path provided')
  }
  if (length(compact(runs)) == 4) {
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
    test_title = compact(list(outer_test_pth, inner_test_pth)) %>% 
      map_chr(~str_replace(.x, '_[^_]+$', '')) #strip everything after the last underscore from filename
    ref_title = compact(list(outer_ref_pth, inner_ref_pth)) %>% 
      map_chr(~str_replace(.x, '_[^_]+$', ''))
    
  }
  if (length(ref_title) == 0) {
    pg_title = str_c(str_wrap(str_c(run_labels[1], ': ', path_file(test_title)), 50, whitespace_only = F), '\n')
  } else if(length(test_title) == 0) {
    pg_title = str_c(str_wrap(str_c(run_labels[1], ': ', path_file(ref_title)), 50, whitespace_only = F), '\n')
  } else {
    pg_title = str_c(str_wrap(str_c(run_labels[1], ': ', path_file(test_title)), 50, whitespace_only = F),
                     '\n\n',
                     str_wrap(str_c(run_labels[2],': ', path_file(ref_title)), 50, whitespace_only = F),
                     '\n') 
  }
  pg_title
}

format_file_size_table = function(file_size_df) {
  gt_tbl = gt(file_size_df) %>% 
    tab_style(style = list(cell_text(color = 'red')),
              locations = cells_body(columns = rel_diff,
                                     rows = abs(rel_diff) > 5)) %>% 
    sub_missing(columns = everything()) %>% 
    cols_label(fname = 'File',
               test = 'Test Run',
               ref = 'Reference Run',
               abs_diff = 'Size difference (B)',
               rel_diff = 'Size difference (%)') %>% 
    cols_align(align = 'right',
               columns = -fname) %>% 
    tab_options(data_row.padding = px(3),
                
                table.align='left')
  gt_tbl
}

format_inv_table = function(inv_dframe) {
  em_table = inv_dframe %>%
    select(Land, pollutant, rel_diff) %>%
    mutate(pollutant = gt_var_labeller[pollutant]) %>% 
    gt() %>% 
    cols_label(Land = 'Country/Region',
               pollutant = 'Species',
               rel_diff = 'Difference (%)') %>% 
    cols_align(align = 'right',
               columns = rel_diff) %>% 
    fmt_number(columns = rel_diff, decimals = 1) %>% 
    tab_options(data_row.padding = px(3),
                table.align='left') %>% 
    text_transform(location = cells_body(columns = pollutant),
                   fn = function(x) {
                     str_replace_all(x, '\\[', "<sub>") %>% 
                       str_replace_all('\\]', "</sub>")
                   })
}

format_MBS_table = function(MBS_dframe) {
  MBS_table = MBS_dframe %>% 
    select(species, rel_diff) %>%
    gt() %>% 
    cols_label(species = 'Species',
               rel_diff = 'Difference (%)') %>% 
    cols_align(align = 'right',
               columns = rel_diff) %>% 
    fmt_number(columns = rel_diff, decimals = 1) %>% 
    tab_options(data_row.padding = px(3),
                table.align='left')
}

check_nrow = function(dframe) {
  #if empty dframe return NULL
  if (is.null(dframe)) {
    return(NULL)
  } else if (nrow(dframe) == 0) {
    return(NULL)
  }
  dframe
}

#cumsum calculation helper
calculate_cumsum = function(x) {
  if (all(is.na(x))) {
    x = NA_real_
  } else {
    x = cumsum(replace_na(x, 0))
  }
}

archive_log = function(log_pth) {
  #moves superseeded log files into 'Log_archive' directory
  #automatically numbers old files to prevent overwriting (up to 99 log files)
  archive_dir = path_dir(log_pth) %>% 
    path('Log_archive') %>% 
    dir_create()
  
  old_log_files = dir_ls(archive_dir)
  if(file_exists(log_pth)) {
    if (length(old_log_files) == 0) {
      file_move(log_pth, path(archive_dir, paste0('00', path_file(log_pth))))
    } else {
      n = old_log_files %>%
        path_file() %>%
        str_extract('\\d*') %>%
        as.integer() %>%
        max(na.rm = T) + 1
      n_string = n %>%
        str_pad(2, side = 'left', pad = '0')
      file_move(log_pth, path(archive_dir, paste0(n_string,path_file(log_pth))))
    }
  }
}

decimal_count <- function(value) sapply(value, decimal_counter)
#from skgrange/threadr

# The worker
decimal_counter <- function(x) {
  
  # Check
  stopifnot(class(x) == "numeric")
  
  # If NA, return NA
  if (is.na(x)) {
    
    x <- NA
    
  } else {
    
    # If contains a period
    if (grepl("\\.", x)) {
      
      x <- stringr::str_replace(x, "0+$", "")
      x <- stringr::str_replace(x, "^.+[.]", "")
      x <- stringr::str_length(x)
      
    } else {
      
      # Otherwise return zero
      x <- 0
      
    }
    
  }
  
  # Return
  x
  
}

#helpers to floor and ceil numbers to a given decimal place
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
