library(tidyverse)
library(fs)
library(sf)
library(furrr)
library(checkmate)
library(worldmet)
library(openair)
library(rvest)
library(reticulate)
wrf = import('wrf')
ncpy = import('netCDF4')
xr = import('xarray')
ccrs = import('cartopy.crs')
metpy = import('metpy')
np = import('numpy')

source('emep_qaqc_funcs.R')
source('wrf_qaqc_user_input.R')

future::plan(multicore)

# MAIN --------------------------------------------------------------------

#extract modelled year based on input wrf files
wrf_files = WRF_DIR %>% 
  dir_ls() %>% 
  str_subset(str_c('wrfout_',WRF_DOMAIN))

#get modelled year from the first file
yr = ncpy$Dataset(wrf_files[1]) %>% 
  wrf$extract_times(timeidx = wrf$ALL_TIMES) %>% 
  year() %>% 
  unique() %>% 
  as.integer()

if (!file_exists(path(data_pth_out, paste0('All_sites_in_domain_', WRF_DOMAIN, '.rds')))) {
  #collate met sites with available data for modelled year into a tibble
  met_sites = get_isd_sites(yr)
  
  #extract model domain x,y dimension sizes
  sn_dim = ncpy$Dataset(wrf_files[1])$dimensions$south_north$size
  we_dim = ncpy$Dataset(wrf_files[1])$dimensions$west_east$size
  
  
  #extract site indeces and filter out those outside the model domain
  met_indeces = wrf$ll_to_xy(ncpy$Dataset(wrf_files[1]), latitude = met_sites$latitude, longitude = met_sites$longitude, meta = F)
  met_indeces = as_tibble(met_indeces, .name_repair = ~met_sites$code) %>% 
    mutate(index = c('i', 'j')) %>% 
    pivot_longer(-index, names_to = 'code', values_to = 'val') %>%
    pivot_wider(id_cols = code, names_from = index, values_from = val) %>% 
    filter(between(i, 0, we_dim - 1),between(j, 0, sn_dim - 1)) # python is 0-start index
  
  met_sites_in = met_sites %>% 
    filter(code %in% met_indeces$code)
  
  #save a tibble with meta data of all met sites within the model domain with available data
  write_rds(met_sites_in, path(data_pth_out, paste0('All_sites_in_domain_', WRF_DOMAIN, '.rds')))
} else {
  met_sites_in = read_rds(path(data_pth_out, paste0('All_sites_in_domain_', WRF_DOMAIN, '.rds')))
}

met_sites_sample = met_sites_in
for (s in seq_along(SITES_SAMPLING_TYPE)) {
  met_sites_sample  = met_sites_sample %>%
    sample_sites(type = SITES_SAMPLING_TYPE[[s]],
                 selector = SITES_SAMPLING_SELECTOR[[s]],
                 n = N_SITES[[s]])
  write_rds(met_sites_sample, path(data_pth_out, paste0('Sites_sample_in_domain_', WRF_DOMAIN, '.rds')))
}

#download observation data
future_pwalk(list(code = met_sites_sample$code, pth = path(dir_create(path(data_pth_out, 'Raw_obs')), str_c(met_sites_sample$code, '_raw_obs'), ext = 'rds')),
             my_importNOAA, year = yr)

# processing downloaded noaa data -----------------------------------------

raw_obs = dir_ls(path(data_pth_out, 'Raw_obs')) %>% 
  str_subset('raw_obs')

noaa_summary = raw_obs %>% 
  future_map_dfr(~read_rds(.x) %>%
                   clean_noaa() %>%
                   assess_noaa())

#filter the available stations as desired
suitable = noaa_summary %>%
  filter(obs_minute == 0, !is.na(precip_code), ws_dc >= 75, air_temp_dc >= 75) %>% distinct(code, report_type, obs_minute, .keep_all = T) 

#and write the suitable sites in a file
write_rds(suitable, path(data_pth_out, paste0('Suitable_sites_in_domain_', WRF_DOMAIN, '_summary'), ext = 'rds'))

#filter the desired observation data from the raw data and save them in the Data directory for collation with modelled values
#this will depend on individual needs
noaa_list = map_chr(suitable$code, str_subset, string = raw_obs) %>% 
  future_map(read_rds) %>% 
  future_map(clean_noaa) %>% 
  future_map(~semi_join(.x, suitable, by = c('code', 'report_type', 'obs_minute'))) %>% 
  future_map(format_noaa) %>% 
  future_map(~pivot_wider(.x, id_cols = c(date, code, scenario), names_from = var, values_from = value)) %>% 
  future_walk2(.y = path(data_pth_out, paste0(suitable$code, '_obs'), ext = 'rds'), write_rds)

sites_geo = met_sites_sample %>% 
  filter(code %in% suitable$code) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
  mutate(longitude = st_coordinates(.)[ , 1],
         latitude = st_coordinates(.)[ , 2]) %>% 
  write_rds(path(data_pth_out, '00Sites_used.rds'))


