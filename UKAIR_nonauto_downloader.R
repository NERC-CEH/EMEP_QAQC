library(tidyverse)
library(lubridate)
library(fs)
library(sf)
library(xml2)
library(glue)
library(furrr)


xpoll_lookup = c(Ca = 'Particulate calcium',
                 Cl = 'Particulate chloride',
                 HCl = 'Gaseous hydocloric acid',
                 HNO3 = 'Gaseous nitric acid',
                 HONO = 'Gaseous nitrous acid',
                 Mg = 'Particulate magnesium',
                 NH3_alpha = 'gaseous ammonia (active)',
                 NH3_delta = 'gaseous ammonia (passive)',
                 NH3_diffusion_tube = 'gaseous ammonia (diffusion tube)',
                 NH4 = 'particulate ammonium',
                 NO2 = 'Nitrogen dioxide',
                 NO3 = 'Nitric oxide',
                 Na = 'Particulate sodium',
                 SO2 = 'Gaseous sulphur dioxide',
                 SO4 = 'Particulate sulphate',
                 ph_precip = 'pH in precipitation',
                 acid_precip = 'Acidity in precipitation',
                 NH4_precip = 'Ammonium as N in precipitation',
                 As_precip = 'Arsenic in precipitation',
                 Cd_precip = 'Cadmium in precipitation',
                 Ca_precip = 'Calcium in precipitation',
                 Cl_precip = 'Chloride in precipitation',
                 Pb_precip = 'Lead in precipitation',
                 Mg_precip = 'Magnesium in precipitation',
                 Hg_precip = 'Mercury in precipitation',
                 Ni_precip = 'Nickel in precipitation',
                 NO3_precip = 'Nitrate as N in precipitation',
                 nm_SO4_precip = 'Non-marine sulphate as S in precipitation',
                 PO4_precip = 'Phosphate as P in precipitation',
                 K_precip = 'Potassium in precipitation',
                 Na_precip = 'Sodium in precipitation',
                 SO4_precip = 'Sulphate as S in precipitation'
)

get_nonauto_meta = function(year, site = 'all', pollutant = 'all', include_data = T, agg = F) {
  #collates metadata of non-automatic monitoring sites from UKEAP website, with the options of
  #including the actual measurements in the output (nested tibble),
  #these can be either raw or aggregated (annually) if agg = T 
  
  get_xpoll_data = function(site_atom_link, pollutant_ref_link)  {
    #returns a tibble with measurement data if exist and include_data == T, 
    #returns NULL if no measurements,
    #returns an empty tibble if include_data == F
    x0 = read_xml(site_atom_link)
    ns0 = xml_ns(x0)
    data_link = x0 %>% 
      xml_find_all(glue('{names(ns0)[1]}:entry')) %>%
      xml_find_all(glue('{names(ns0)[1]}:id')) %>%
      xml_text()
    
    x1 = read_xml(data_link)
    ns = xml_ns(x1)
    
    x_polls = x1 %>% 
      xml_find_all('//om:observedProperty', ns)
    
    #find the OM_Observation node with the required pollutant 
    x_poll = tryCatch(
      error = function(cnd) NULL,
      
      x_polls[xml_attr(x_polls, 'href') == pollutant_ref_link] %>% 
        xml_parent()
    )
    
    if ((!is.null(x_poll)) & (length(x_poll) != 0)) {
      if (include_data == T) {
        components = xml_find_all(x_poll, str_c(xml_path(x_poll), '//swe:field/@name')) %>% 
          xml_text()
        
        text_encoding = xml_find_all(x_poll, str_c(xml_path(x_poll), '//swe:TextEncoding')) %>% 
          xml_attrs() %>% 
          flatten_chr()
        
        x_data = xml_find_all(x_poll, str_c(xml_path(x_poll), '//swe:values')) %>%
          xml_text() %>% 
          str_remove_all('\\n|[[:space:]]') %>% 
          str_split(text_encoding['blockSeparator']) %>%
          flatten_chr() %>%
          str_subset('.+') %>% #remove potential empty strings (will occur for AggregatedObs)
          as_tibble() %>% 
          separate(col = 1, into = components, sep = text_encoding['tokenSeparator']) %>% 
          mutate(start_time = ymd_hms(StartTime),
                 end_time = ymd_hms(EndTime),
                 .before = StartTime) %>% 
          select(-StartTime, -EndTime) %>% 
          mutate(across(c(-start_time,-end_time), ~parse_double(.x)),
                 Value = na_if(Value, -99)) %>% 
          rename_with(.fn = janitor::make_clean_names) %>% 
          rename(conc = value)
        
        
        if ('Value' %in% components) {
          x_unit = xml_find_all(x_poll, str_c(xml_path(x_poll), "//swe:field[@name='Value']")) %>% 
            xml_find_all(str_c(xml_path(.),'//swe:uom/@xlink:href')) %>% 
            xml_text() %>% 
            path_split() %>% 
            map_chr(last)
          x_data = x_data %>% 
            mutate(unit = x_unit)
        }
        return(x_data)
      } else {
        return(tibble())
      }
    } else {
      return(NULL)
    }
  }
  
  
  if (agg == F) {
    atom_link = glue('https://uk-air.defra.gov.uk/data/atom-dls/non-auto/{year}/atom.en.xml')
  } else {
    atom_link = glue('https://uk-air.defra.gov.uk/data/atom-dls/aggregated/{year}/atom.en.xml')
  }
  
  x0 = read_xml(atom_link)
  ns = xml_ns(x0) #namespaces
  
  x_e = x0 %>%
    xml_find_all(glue('{names(ns)[1]}:entry'))
  
  x_name0 = x_e %>% 
    map(~xml_find_all(.x, glue('{names(ns)[1]}:title'))) %>% 
    map(xml_text) %>% 
    map(~str_remove_all(.x, str_glue('(GB (Aggregated|Fixed) Observations for )|( in {year})|\\)'))) %>% 
    map(~str_split(.x, ' \\(')) %>% 
    flatten()
  
  #extract coordinates for each entry, only take first two (order is latitude, longitude)
  x_coords = x_e %>%
    map(~xml_find_all(.x, 'georss:polygon', ns)) %>%
    map(xml_text) %>% 
    map(~map(.x, ~str_split(.x, '[[:space:]]'))) %>%
    map(flatten)
  
  x_lat = x_coords %>%
    map(~map_chr(.x, 1)) %>%
    map(as.double)
  
  x_lon = x_coords %>%
    map(~map_chr(.x, 2)) %>%
    map(as.double)
  
  #extract compound name
  x_pname = x_e %>%
    map(~xml_find_all(.x, glue('{names(ns)[1]}:link[@rel="related"]/@title'))) %>% 
    map(xml_text) %>% 
    map(~str_replace(.x, 'Pollutant in feed - ', ''))
  
  #extract compound code
  x_pref = x_e %>% 
    map(~xml_find_all(.x, glue('{names(ns)[1]}:link[@rel="related"]/@href'))) %>% 
    map(xml_text)
  
  #extract atom link to site
  x_link = x_e %>%
    map(~xml_find_all(.x, glue('{names(ns)[1]}:id'))) %>% 
    map(xml_text)
  
  #combine into tibble
  x_meta = tibble(site = map(x_name0, 1),
                  code = map(x_name0, 2),
                  site_link = x_link,
                  longitude = x_lon,
                  latitude = x_lat,
                  poll_name = x_pname,
                  poll_ref = x_pref) %>% 
    unnest(cols = everything()) %>% 
    mutate(poll = set_names(names(xpoll_lookup), xpoll_lookup)[poll_name]) %>% 
    filter(!is.na(longitude) & !is.na(latitude)) #output only stations with coords
  
  #some atom links contain both agg and fixed observations so filter out the unwanted ones
  if (agg == F) {
    x_meta = x_meta %>% 
      filter(str_detect(site_link, 'FixedObs'))
  } else {
    x_meta = x_meta %>% 
      filter(str_detect(site_link, 'AggregatedObs'))
  }
  
  if (site != 'all') {
    x_meta = x_meta %>% 
      filter(site %in% !!!site)
  }
  
  if (pollutant != 'all') {
    x_meta = x_meta %>% 
      filter((poll_name %in% pollutant) | (poll %in% pollutant))
  }
  
  xpoll_data = future_pmap(list(site_atom_link = x_meta$site_link,
                                pollutant_ref_link = x_meta$poll_ref),
                           get_xpoll_data)
  
  xpoll_data_lgl = xpoll_data %>% 
    map_lgl(~(!is.null(.x)))
  xpoll_data = xpoll_data[xpoll_data_lgl]
  x_meta = x_meta[xpoll_data_lgl, ]
  
  if (include_data == T) {
    x_meta = x_meta %>% 
      mutate(poll_data = xpoll_data)
  }
  
  x_meta
  
}