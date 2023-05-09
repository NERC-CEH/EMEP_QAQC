library(tidyverse)
library(fs)
library(stars)
library(ggspatial)
#library(mapview)
#library(leaflet)
#library(leaflet.providers)
#library(leafem)
library(checkmate)
library(openair)
#library(rnaturalearthdata)
source('emep_vars_parameters.R')
source('emep_qaqc_funcs.R')
VAR_PARAMS_LIST = get_params_list() # list of parameters for each EMEP var 
OBS_VAR_PARAMS_LIST = get_obs_var_params_list() # list of plotting params for EMEP vars used in obs-mod comparison


# USER_INPUT --------------------------------------------------------------

UEMEP_FPATH = '/home/tomlis65/uEMEP/Runs/UK_annual_RPIG50/UK_annual_RPIG50_merged.nc'
#UEMEP_FPATH = '../uEMEP/output/UK_annual_RPIG250_merged.nc'

POLLUTANTS = c('no2', 'o3')
#POLLUTANT = 'no2'

AREAS = list(UK = list(xlim = c(164750, 823888.4), ylim = c(5533794.9, 6747809.0)),
             CB = list(xlim = c(371784, 512831), ylim = c(6165233, 6242239)),
             London = list(xlim = c(670500, 729000), ylim = c(5682000, 5733000)))

PIC_DIMS = list(UK = list(height = 8, width = .7 * 8),
                London = list(height = 3, width = 3 * 1.15 + 0.25),
                CB = list(height = 4, width = 4 *1.84 +0.25))
MAP_BREAKS = list(no2 = c(0, 5, 10, 15, 20, 30, 40, 50, 80, 100, 150, 200, 250),
                  o3 = c(seq(0, 70, 5), 85),
                  pm25 = c(seq(0,30,2), 35, 40, 45, 51),
                  pm10 = c(seq(0,50,5), 50, 60, 70,80))
AREA = 'UK'
                  

# notes -------------------------------------------------------------------
#utm30 N EPSG: 32630 


                  
# MAIN --------------------------------------------------------------------
for (j in seq_along(POLLUTANTS)) {
  uemep_var = paste0(POLLUTANTS[j], '_concentration')
  
  assert_choice(POLLUTANTS[j], c('no2', 'o3', 'pm25', 'pm10'))
  
  uemep = read_stars(UEMEP_FPATH, sub = uemep_var) %>%
    st_as_stars()
  
  #uk = st_read('N:/PhD/Data/GIS data/Cartopy/ne_10m_admin_0_countries.shp') %>% 
  #  filter(NAME == 'United Kingdom')
  uk = st_read('Area_masks/Countries.gpkg') %>% 
    filter(NAME == 'United Kingdom')
  
  
  map_breaks = MAP_BREAKS[[POLLUTANTS[j]]]
  #uemep_var_precision = as.integer(str_sub(VAR_PARAMS_LIST[[uemep_var]][['map_diffprecision']], -2, -2))
  uemep_var_max = uemep %>% 
    map_dbl(max, na.rm = T)
  
  uk = uk %>% #read_sf('UK_geo_data/UK_countries.gpkg') %>% 
    st_transform(st_crs(uemep))
  
  uemep = uemep[uk]
  
  
  #palette
  ncl_palette_dir = 'NCL_colors'
  ncl_rainbow = read_color(path(ncl_palette_dir, 'WhiteBlueGreenYellowRed.rgb')) %>%
    get_color(n = length(map_breaks) -1)
  
  # cb = colorBin(palette = ncl_rainbow, domain = c(map_breaks[1], map_breaks[length(map_breaks)]), bins = map_breaks,
  #               na.color = NA)
  
  uemep_lab = POLLUTANTS[j] %>% 
    str_c(., '\n(', VAR_PARAMS_LIST[[uemep_var]][['units']], ')')
  
  for (i in seq_along(names(AREAS))) {
    p = ggplot() +
      geom_stars(data = cut(uemep, breaks = map_breaks)) +
      geom_sf(data = uk, color = 'black', fill = NA,  size = 0.05 ) +
      #geom_sf(data = roads,
      #aes(size = fclass),
      #       color = '#60676f', alpha = .1, show.legend = F, size = 0.05) +
      scale_fill_manual(values = ncl_rainbow, drop = F,
                        labels = scales::label_number(accuracy = 1),
                        guide = guide_coloursteps(title.position = 'top',
                                                  show.limits = T,
                                                  frame.colour = 'black',
                                                  frame.linewidth = 0.1,
                                                  barheight = unit(3., 'inches')
                        )) +
      labs(fill = quickText(uemep_lab)) +
      coord_sf(xlim = AREAS[[names(AREAS)[i]]][['xlim']], ylim = AREAS[[names(AREAS)[i]]][['ylim']]) +
      scale_y_continuous(expand = c(0,0)) + 
      scale_x_continuous(expand = c(0,0)) + 
      annotation_scale(height = unit(0.2, 'cm'), text_cex = 1, location = 'br') + 
      theme_void() +
      theme(legend.position = 'left',
            legend.direction = 'vertical',
            #legend.key.height = unit(2, 'mm'),
            legend.title = element_text(size = 15),
            legend.text = element_text(size = 13),
            panel.background = element_rect(fill = 'white', color = NA),
            panel.border = element_rect(color = 'black', size = 0.3, fill = NA),
            plot.title = element_text(size = 10, hjust = 0.5, face = 'bold'))
    
    if (names(AREAS)[[i]] %in% c('CB', 'London')) {
      p = p + guides(fill = 'none')
    }
    ggsave(paste0('/home/tomlis65/uEMEP/Plots/', names(AREAS)[[i]], '_uEMEP50_', str_replace(uemep_var, '_[^_]+$', ''), '_2018.png'),
           plot = p, height = PIC_DIMS[[names(AREAS)[[i]]]][['height']], width = PIC_DIMS[[names(AREAS)[[i]]]][['width']], dpi = 320)
  }  
  
}

