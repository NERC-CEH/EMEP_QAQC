### Extracts wrf variables at sites (lon lat coordinates) at the lowest vertical level from a wrf run
### All data provided by Janice

library(tidyverse)
library(fs)
library(sf)
library(furrr)
library(checkmate)
library(worldmet)
library(openair)
library(rvest)
#library(Hmisc)
library(reticulate)

use_virtualenv('/gpfs01/air_models/home/tomlis65/.virtualenvs/qaqc')

wrf = import('wrf')
ncpy = import('netCDF4')
xr = import('xarray')
ccrs = import('cartopy.crs')
metpy = import('metpy')
np = import('numpy')

source('/gpfs01/air_models/home/tomlis65/EMEP_QAQC/source_code/EMEP_QAQC/emep_qaqc_funcs.R')
future::plan(multicore)

# INPUT -------------------------------------------------------------------

WRF_DIR = '/home/emep4uk/WRF/STORED_WRF_RUNS/UKSCAPE/WRF4.4.2/2024'
WRF_DOMAIN = 'd03'
WRF_VARS = c('UST', 'CLDFRA', 'ua','va', 'QCLOUD', 'QVAPOR', 'tc')

wrf_files = WRF_DIR %>% 
  dir_ls() %>% 
  str_subset(str_c('wrfout_',WRF_DOMAIN))

#get modelled year from the first file
yr = ncpy$Dataset(wrf_files[1]) %>% 
  wrf$extract_times(timeidx = wrf$ALL_TIMES) %>% 
  year() %>% 
  unique() %>% 
  as.integer()

sites_pth = '/gpfs01/air_models/home/tomlis65/Janice/Complex_Terrain_sites.csv'
#sites_pth = '../Janice/Complex_Terrain_sites.csv' 


sites = read_csv(sites_pth) %>% 
  discard(~all(is.na(.)))

# sites_geo = sites %>% 
#   st_as_sf(coords = c('lon', 'lat'), crs = 4326)

#extract site indexes
met_index_xr = wrf$ll_to_xy(ncpy$Dataset(wrf_files[1]), latitude = sites$lat, longitude = sites$lon, meta = T)
met_index_xr2 = wrf$ll_to_xy(ncpy$Dataset(wrf_files[1]), latitude = sites$lat, longitude = sites$lon, meta = F)


wrf_file = ncpy$Dataset(wrf_files[1])

indexes = as_tibble(met_index_xr2, .name_repair = ~sites$code) %>% 
  mutate(index = c('i', 'j')) %>% 
  pivot_longer(-index, names_to = 'code', values_to = 'val') %>%
  pivot_wider(id_cols = code, names_from = index, values_from = val)

#set up loop and iterate safely
sites = sites %>%
  left_join(indexes) %>% 
  mutate(cell_id = str_glue('{i}_{j}'))

sites_iter = sites %>%  
  distinct(cell_id, .keep_all = T) %>% 
  select(code, i, j, cell_id)

iter_df = expand_grid(met_var = WRF_VARS, wrf_file = wrf_files[1:2])



wrf_data_tlist = future_pmap(list(wrf_file_pth = iter_df$wrf_file, wrf_var = iter_df$met_var),
                             safely(extract_wrf_var_point), code = sites$code, xr_index = met_index_xr, index_level = 0, 
                             .options = furrr_options(seed = T)) %>% 
  transpose()

is_ok <- wrf_data_tlist$error %>%
  map_lgl(is_null)
wrf_data_list = wrf_data_tlist$result[is_ok] %>% 
  bind_rows() %>%
  split(.$code)
write_rds(wrf_data_list, path('/gpfs01/air_models/home/tomlis65/Janice', 'wrf_sites_data_level0.rds'))

