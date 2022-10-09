library(fs)

# projection defs ---------------------------------------------------------
### projection definitions used in EMEP

EMEP_CRS_LL = '+proj=eqc +ellps=WGS84 +a=6378137.0 +lon_0=0.0 +to_meter=111319.4907932736 +vto_meter=1 +no_defs +type=crs'
EMEP_CRS_STEREO1 = NULL
EMEP_CRS_STEREO2 = '+proj=stere +ellps=sphere +lat_0=90.0 +lon_0=0.0 +x_0=0.0 +y_0=0.0 +units=km +k_0=0.9330127018922193 +no_defs +type=crs'

# USER INPUT --------------------------------------------------------------

# tasks -------------------------------------------------------------------

PLOT_COMPARISON_MAPS = T
CALCULATE_BUDGET = T
PLOT_DSC = T
COLLATE_MOBS = T
SAVE_MOBS_RDS = T
PLOT_MOBS_PDF = T
COMPARE_EMISSIONS = T

# input paths and fnames -------------------------------------------------------------------
### provide ABSOLUTE paths - for OUTER (e.g. EU) and INNER (e.g. UK) domains
###                        - TEST is the RUN that needs QAQC, REF is the reference run for comparison
###                           - not all fnames are required, in which case set them to NA
###                        - EMEP_BUDGET for calculating budget (can be a vector of paths for comparison)
###                           - only first element in EMEP_BUDGET_FNAME is used for deposition and conc plots
###                           - BUDGET_MASK is a shp (or.gpkg) file for area masking (e.g. UK land)
###                        - TEST_OBS is the run used for comparisons with observations

###set fnames to NA if not needed
TEST_OUTER_DIR = '/home/jansch/EMEP/EMEP_user_4.45/output/REGREEN/BASE/2018_nofire/EU'
TEST_INNER_DIR = '/home/jansch/EMEP/EMEP_user_4.45/output/REGREEN/BASE/2018_nofire/REGREEN'
REF_OUTER_DIR = '/home/mvi/EMEP_user_4.36/output/REGREEN/BASE/2018/EU'
REF_INNER_DIR = '/home/mvi/EMEP_user_4.36/output/REGREEN/BASE/2018/REGREEN'

#TEST_OUTER_FNAME = '/home/tomlis65/EMEP_user_4.36/output/UKSCAPE/BASE/2018/EU/EMEP4UK_emep-ctm-rv4.36_wrf4.2.2_UKSCAPE_BASE_trend2018_emiss2018_EU_2018_fullrun.nc'
#TEST_INNER_FNAME = '/home/tomlis65/EMEP_user_4.36/output/UKSCAPE/BASE/2018/UK_3Km_LF2/EMEP4UK_emep-ctm-rv4.36_wrf4.2.2_UKSCAPE_BASE_trend2018_emiss2018_UK_3Km_2018_fullrun.nc'
#REF_OUTER_FNAME = '/home/mvi/EMEP_user_4.36/output/ROWE/BASE/2019/EU/EMEP4UK_emep-ctm-rv4.36_wrf4.2.2_ROWE_BASE_trend2019_emiss2019_EU_2019_fullrun.nc'
#REF_INNER_FNAME = '/home/mvi/EMEP_user_4.36/output/ROWE/BASE/2019/UK_3Km/EMEP4UK_emep-ctm-rv4.36_wrf4.2.2_ROWE_BASE_trend2019_emiss2019_UK_3Km_2019_fullrun.nc'

EMEP_BUDGET_FNAME = c(str_subset(dir_ls(TEST_INNER_DIR), 'fullrun'), #budget fnames by default fullrun outputs for test inner and ref inner runs resp.
                      str_subset(dir_ls(REF_INNER_DIR), 'fullrun'))  #the test fname must be the first element of the vector

BUDGET_MASK_FNAME = 'Area_masks/UK_landmask.gpkg'
WEBDABEMEP_PTH = 'WebdabEMEP_files/webdabEMEPNationalEmissions2000-2019.txt' #EMEP emissions

TEST_OBS_FNAME = TEST_INNER_FNAME #EMEP file used for comparison with hourly observations

PALETTE_DIR = 'NCL_colors' #where ncl color palettes are stored

# projections --------------------------------------------------------------
### declare projections - the projection MUST be the same for all EMEP input files
###                       except for EMEP_BUDGET_FNAMEs - they need to be declared separately

EMEP_CRS = EMEP_CRS_STEREO2

EMEP_BUDGET_CRS = c(EMEP_CRS_STEREO2, EMEP_CRS_STEREO2)

# output paths ------------------------------------------------------------
### qaqc output directory is by default the directory of the inner EMEP domain test run

### the output can be saved in a different directory in which case the QAQC_DIR !!!MUST!!! contain the project
### name (and maybe a year as a subdirectory if multiple years are qaqced)
### e.g. QAQC_DIR = dir_create(path('/home/tomlis65/QAQC_output/ONS/URBAN/2007'))

### please do not change any other paths!

QAQC_DIR = TEST_INNER_DIR

if (QAQC_DIR != TEST_INNER_DIR) { 
  qaqc_pth_out = dir_create(path(QAQC_DIR))
} else {
  qaqc_pth_out = dir_create(path(QAQC_DIR, 'QAQC'))
}

report_pth_out = dir_create(path(qaqc_pth_out, 'Reports'))
maps_pth_out = dir_create(path(qaqc_pth_out, 'Maps'))
plots_pth_out = dir_create(path(qaqc_pth_out, 'Plots'))
tables_pth_out = dir_create(path(qaqc_pth_out, 'Tables'))
data_pth_out = dir_create(path(qaqc_pth_out, 'Data'))

# output fnames -----------------------------------------------------------
### provide filenames for qaqc output

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
# other parameters --------------------------------------------------------------

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

MOBS_STATION_REPORT_TSERIES = c('CHBO', 'ACTH', 'AGRN', 'KC1', 'BRS8', 'GLKP', 'MY1') # daily means of these will be plotted in the report
MOBS_STATION_REPORT_MAP = T #show selected MOBS station for the report on a map

OBSERVED_POLLS = c('no', 'no2', 'o3', 'ox', 'so2', 'pm10', 'pm2.5') # tested pollutants -if any other compounds added
# their plotting params must be set in 
# OBS_VAR_PARAMS_LIST - see 'emep_vars_parameters0.R'

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