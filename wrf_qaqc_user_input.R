library(fs)

# tasks -------------------------------------------------------------------
COLLATE_MOBS = T
EVALUATE_MOBS = T
PLOT_MOBS_MAPS = T
PLOT_MOBS = T

# Input dirs and fnames -------------------------------------------------------------------
### provide ABSOLUTE paths 
WRF_DIR = '/gpfs01/home/emep4uk/WRF/STORED_WRF_RUNS/UKSCAPE/WRF4.2.2/2018'
###select which wrf domain you want qaqced
WRF_DOMAIN = 'd03'
WRF_RUN_DESCRIPTOR = 'WRF4.2.2 UKSCAPE 2018'

OBS_DIR = 'default' #default is the Data subdirectory in the QAQC_DIR but can be any previous qaqc run data - make sure both the domain and year are the same!!!
REF_MOBS_DIR = NULL

PALETTE_DIR = '/home/emep4uk/Tools/NCL/6.6.2/lib/ncarg/colormaps' #where ncl color palettes are stored
                                                                  #see https://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml

# Output directory ------------------------------------------------------------
### qaqc output directory is by default 'WRF_QAQC' directory created in the current working directory

### replace the dir_create(path(getwd(), 'WRF_QAQC')) argument with a different path if you wish
### the QAQC_DIR !!!MUST!!! contain the WRF run details and year (as a subdirectory if multiple years are qaqced)
### e.g. QAQC_DIR = path('/home/tomlis65/QAQC_output/WRF/WRF4_2_2/UKSCAPE/2018')

QAQC_DIR = dir_create(path(getwd(), 'WRF_QAQC'))


# Output fnames -----------------------------------------------------------
### provide filenames for qaqc output

REPORT_FNAME = 'WRF_QAQC_Report.html'                    # name of the published report (must be .html)
MOBS_STATS_FNAME = 'Mobs_modstats_annual.csv'                # summary statistics table of modelled - observed 

# Site sampling parameters ------------------------------------------------
# parameter values for 'sample_site' function - each parameter below must be a list of same length
# and corresponding elements must be valid for the sampling type - please see function docstring
SITES_SAMPLING_TYPE = list('subset', 'random') # one of c(NULL, 'sf', 'random', 'stratified', 'subset', 'manual', 'exclude')
SITES_SAMPLING_SELECTOR = list('airport', NULL)
N_SITES = list(NULL, 150)

# Other parameters --------------------------------------------------------------
MODSTATS_STATS = c('n', 'FAC2', 'MB', 'NMB', 'RMSE', 'r')            #choose stats for model evaluation
MOBS_THRESHOLD = 75            # mobs stats calculated for sites with data capture >= MOBS_THRESHOLD 
MOBS_TZONE = 'UTC' # timezone  MOBS time series plots are shown in. For options run OlsonNames()
MOBS_GROUPING_VAR = NULL
MOBS_GROUPING_VAR_COLOURS = NULL

MOBS_STATION_REPORT_TSERIES = c(1,2,3) # daily means of these will be plotted in the report

MOBS_STATION_REPORT_MAP = T #show selected MOBS station for the report on a map
MOBS_MAP_BASEMAP = 'world_topo'
MOBS_MAP_STAT = 'MB'
MOBS_MAP_VAR = c('T2', 'rh2', 'ws')

# tested vars -if any other vars added, RAINNC + RAINC needed for precip
# their plotting params must be set in OBS_VAR_PARAMS_LIST - see 'emep_vars_parameters.R'
WRF_VARS = c('sea_level_pressure' = 'slp',
             'air_temp_2m' = 'T2',
             'dew_temp_2m' = 'td2',
             'rel_humidity_2m' = 'rh2',
             'wind_speed_10m' = 'ws',
             'wind_dir_10m' = 'wd',
             'acc_grid_rain' = 'RAINNC',
             'acc_cu_rain' = 'RAINC')

PLOT_ALL_WRF_VARS = T    #should all vars in WRF_VARS be plotted? If FALSE, plot only those (FALSE currently not working)
#vars that are measured at the automatic monitoring site. RAINNC and RAINC will are accumulated vars and will be summed up for accumulated precip

PPP = 4                        #plots per page for obs-mod - do NOT change (unless a good reason!)

RANDOM_SEED = 6 # for reproducible random selection of met sites, default is set to 6
                # do NOT change (unless a good reason!)

# output paths ------------------------------------------------------------

### please do not change any paths unless you have a very good reason (there isn't one!)
report_pth_out = dir_create(path(QAQC_DIR, 'Reports'))
maps_pth_out = dir_create(path(QAQC_DIR, 'Maps'))
plots_pth_out = dir_create(path(QAQC_DIR, 'Plots'))
tables_pth_out = dir_create(path(QAQC_DIR, 'Tables'))
data_pth_out = dir_create(path(QAQC_DIR, 'Data'))



