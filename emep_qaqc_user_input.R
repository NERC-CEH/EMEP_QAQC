library(fs)
library(logger)
# projection defs ---------------------------------------------------------
### projection definitions used in EMEP

EMEP_CRS_LL = '+proj=eqc +ellps=WGS84 +a=6378137.0 +lon_0=0.0 +to_meter=111319.4907932736 +vto_meter=1 +no_defs +type=crs'
EMEP_CRS_STEREO1 = NULL
EMEP_CRS_STEREO2 = '+proj=stere +ellps=sphere +lat_0=90.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +units=km +k_0=0.9330127018922193 +no_defs +type=crs'

# USER INPUT --------------------------------------------------------------
# Coordinate systems --------------------------------------------------------------
TEST_CRS = EMEP_CRS_STEREO2
REF_CRS = EMEP_CRS_STEREO2

EMEP_BUDGET_CRS = c(EMEP_CRS_STEREO2, EMEP_CRS_STEREO2)


# tasks -------------------------------------------------------------------
COMPARE_FILE_SIZE = T
COMPARE_EMISSIONS = T
COMPARE_BUDGET_MSC = T
PLOT_COMPARISON_MAPS = T
COLLATE_MOBS = T
EVALUATE_MOBS = T
PLOT_MOBS = T
PLOT_MOBS_MAPS = T

# Input dirs and fnames -------------------------------------------------------------------
### provide ABSOLUTE paths - for OUTER (e.g. EU) and INNER (e.g. UK) domains
###                        - TEST is the RUN that needs QAQC, REF is the reference run for comparison
###                           - not all dirs are required, in which case set them to NULL
###                        - EMEP_BUDGET for calculating budget (can be a list of dirs for comparison)
###                           - only first element in EMEP_BUDGET_FNAME is used for deposition and conc plots
###                           - BUDGET_MASK is a shp (or.gpkg) file for area masking (e.g. UK land)
###                        - TEST_OBS is the run used for comparisons with observations

###set directories to NULL if not needed
TEST_OUTER_DIR = NULL
TEST_INNER_DIR = NULL
REF_OUTER_DIR = NULL
REF_INNER_DIR = NULL

FULLRUN_OUTER_TRIMMED = F                          #set to T if fullrun output has been trimmed to match reference run domain
FULLRUN_INNER_TRIMMED = F

EMEP_BUDGET_DIR = list(TEST_INNER_DIR, REF_INNER_DIR) #budget fnames by default fullrun outputs for test inner and ref inner runs resp.
                                                   #the test fname must be the first element of the vector

BUDGET_MASK_FNAME = NULL # gpks or shp files in Area_masks directory or own supplied
EMISSION_INVENTORY_PTH = NULL #choose one of the EMEP emissions files found in Emission_Inventory_files directory
REF_MOBS_DIR = NULL

PALETTE_DIR = '/home/emep4uk/Tools/NCL/6.6.2/lib/ncarg/colormaps' #where ncl color palettes are stored
                                                                  # see https://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml

# Output directory ------------------------------------------------------------
### qaqc output directory is by default the directory of the inner EMEP domain test run

### the output can be saved in a different directory in which case the QAQC_DIR !!!MUST!!! contain the project
### name (and maybe a year as a subdirectory if multiple years are qaqced)
### e.g. QAQC_DIR = dir_create(path('/home/tomlis65/QAQC_output/ONS/URBAN/2007'))

QAQC_DIR = TEST_INNER_DIR

# Output fnames -----------------------------------------------------------
### provide filenames for qaqc output

REPORT_FNAME = 'QAQC_Report.html'                    # name of the published report (must be .html)
LOGGER_FNAME = 'default'                             #name of the logger file; default is REPORT_FNAME.log
BUDGET_TABLE_FNAME = 'Budget_table.csv'              # deposition and surf conc summary (DSC)
BUDGET_PLOT_FNAME = 'Budget_diff.png'                 # budget_table plotted if exactly 2 runs are being compared
DSC_MAPS_FNAME = 'DSC_maps.pdf'                      # DSC maps pdf
COMP_MAPS_FNAME = 'COMP_maps.pdf'                    # test and reference run comparison maps
MOBS_STATS_FNAME = 'Mobs_modstats.csv'                # summary statistics table of modelled - observed 
MBS_TABLE_FNAME = 'MBS_diff.csv'                      # emission comparison between test and ref runs in MassBudgetSummary.txt
#  - will be preceded by domain name (e.g. EU_...)
MBS_PLOT_FNAME = 'MBS_diff.png'                       # MBS table plotted (do not change the file format)
#  - will be preceded by domain name
INV_MOD_EMISS_TABLE_FNAME = 'inv_mod_emiss_table.csv'# emission comparison between EMEP/NAEI inventory and model input in RunLog.out
#  - will be preceded by domain name
INV_MOD_EMISS_PLOT_FNAME = 'inv_mod_emiss.pdf'        # inv_mod_emiss_table plotted
#  - will be preceded by domain name

# Other parameters --------------------------------------------------------------

RUN_LABELS = c('Test', 'Reference')                                # labels for test and ref comparison maps
PRETTY_LABS = F                                                    # properly formatted pollutant labels (for external publishing) 
#  - if FALSE, raw EMEP var_names are used 

EMISS_DIFF_THRESHOLD = 5       # flag emission differences over this percentage
BUDGET_DIFF_THRESHOLD = 5      # flag budget differences over this percentage

BUDGET_VARS = 'all'            # variables for budget calculations and DSC plotting 
COMP_MAP_VARS = 'all'          # variables for comparison maps
# if 'all' then every var for which the calculation and plotting 
# parameters are set (see 'emep_vars_parameters0.R') are used

COMP_MAP_VARS_REPORT = c('SURF_ug_NO2',
                         'SURF_ug_NH3',
                         'SURF_ppb_O3',
                         'SURF_ug_PM25_rh50') #these will be plotted in the report 

AUTO_NETWORK = 'aurn'          # the automatic monitoring network used for observation data
# can currently be any or all of 'aurn', 'saqn', 'waqn', 'aqe', 'nian' 
# ADDITIONALLY a path to an rds file containing info on other monitoring sites can be included
# - such a file needs to contain the following per site: 'code' , 'site', 'site_type_grp',
#                                                        'latitude' 'longitude'
# - the file MUST also contain a column called 'network' with full paths to the actual monitoring data csv files
# - the csv data files must have columns: 'code', 'date' (which is hour starting) and poll names as in OBSERVED_POLLS
#   below

MOBS_THRESHOLD = 75 # mobs stats calculated for sites with data capture >= MOBS_THRESHOLD

MOBS_GROUPING_VAR = 'site_type_grp' # splits mobs by given var for modstats and scatter plots, set to NULL if no split wanted
MOBS_GROUPING_VAR_COLOURS = c(Urban = '#7570b3', Rural = '#1b9e77', Industrial = '#d95f02', Road = '#e7298a', Unknown = '#666666') #set grouping var colour

MOBS_TZONE = 'UTC' # timezone  MOBS time series plots are shown in. For options run OlsonNames()
MOBS_STATION_REPORT_TSERIES = c('CHBO', 'ACTH', 'AGRN', 'KC1', 'BRS8', 'GLKP', 'MY1') # daily means of these will be plotted in the report

MOBS_STATION_REPORT_MAP = T #show selected MOBS station for the report on a map
MOBS_MAP_BASEMAP = 'world_topo' #options are c('world_topo', 'satellite', 'terrain')
MOBS_MAP_STAT = 'MB'
MOBS_MAP_VAR = c('no2', 'o3', 'pm2.5')

OBSERVED_POLLS = c('no', 'no2', 'o3', 'ox', 'so2', 'pm10', 'pm2.5') # tested pollutants -if any other compounds added
# their plotting params must be set in 
# OBS_VAR_PARAMS_LIST - see 'emep_vars_parameters.R'

OX_UNITS = 'ppb' #if Ox is calculated in which units to output - either 'ppb' or 'ug/m3'

#non-auto species
NONAUTO_SPECIES = c('Ca_p', 'Cl_p', 'HCl_g', 'HNO3_g', 'HONO_g', 'Mg_p', 'Na_p',
                   'NH3_alpha', 'NH3_delta', 'NH3_diffusion_tube', 'NH4_p', 'NO2_p',
                   'NO3_p', 'SO2_g', 'SO4_p', 'NH4_precip', 'NO3_precip', 'nmSO4_precip', 'rainfall'
                   )

OBSERVED_POLLS_EMEP_LINK = c(no = 'SURF_ug_NO',                     # links obs pollutants with their emep vars
                             no2 = 'SURF_ug_NO2',
                             o3 = 'SURF_ppb_O3',
                             so2 = 'SURF_ug_SO2',
                             pm10 = 'SURF_ug_PM10_rh50',
                             pm2.5 = 'SURF_ug_PM25_rh50')

PLOT_ALL_OBSERVED_POLLS = T    #should all pollutants in OBSERVED_POLLS be plotted? If FALSE, plot only those 
#pollutants that are measured at the automatic monitoring site

MODSTATS_STATS = c('n', 'FAC2', 'MB', 'NMB', 'RMSE', 'r')            #choose stats for model evaluation


PPP = 4                        #plots per page for obs-mod - do NOT change (unless a good reason!)

# output paths ------------------------------------------------------------

### please do not change any output paths unless you have a very good reason (there isn't one!)
if (is.null(TEST_INNER_DIR) | QAQC_DIR != TEST_INNER_DIR) { 
  qaqc_pth_out = dir_create(path(QAQC_DIR))
} else {
  qaqc_pth_out = dir_create(path(QAQC_DIR, 'QAQC'))
}

report_pth_out = dir_create(path(qaqc_pth_out, 'Reports'))
maps_pth_out = dir_create(path(qaqc_pth_out, 'Maps'))
plots_pth_out = dir_create(path(qaqc_pth_out, 'Plots'))
tables_pth_out = dir_create(path(qaqc_pth_out, 'Tables'))
data_pth_out = dir_create(path(qaqc_pth_out, 'Data'))

#logger
logger_pth = ifelse(LOGGER_FNAME == 'default',
                      path(report_pth_out, path_ext_remove(REPORT_FNAME), ext = 'log'),
                      path(report_oth_out, LOGGER_FNAME))
