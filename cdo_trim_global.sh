#!/bin/ksh
# Janice Scheffler
# 2023-01-06

# >>> Purpose of this script: Trim global fullrun files to match the current reference for global runs. 

# Currently the reference for global runs is Yao's run:
# /home/yaoge33/EMEP_user_4.34/output/GLOBAL/EMEP4UK_emep-ctm-rv4.34_wrf4.2.2_GLOBAL_BASE_EDGAR_EQSAM_SoilNoxBug_PAN_d/2015/GLOBAL/EMEP4UK_emep-ctm-rv4.34_wrf4.2.2_GLOBAL_BASE_EDGAR_EQSAM_SoilNoxBug_PAN_d_trend2015_emiss2015_GLOBAL_2015_fullrun.nc

# In order to map the difference between ref and test in the QAQC, the lat and lon values need to match. Yao's run has 2 latitudes less than our current global runs (missing one lat at the North and one at the South pole). 
# We therefore need to cut off 2 latitudes from our runs to match with Yao's grid. 
# The script creates a trimmed fullrun file in the orginial output directory that will be picked up by the QAQC.

# !!! Check that the reference and rest runs have different grid sizes: cdo griddes file
# !!! Do not use if reference and test grids match

if [ "$1" == '' ]; then echo "missing input"; echo "usage: ./cdo_trim_global.sh /full/path/to/fullrun.nc"; exit ; fi 

# >>> User settings
# input full path and filename of *fullrun.nc
infile=$1

# > adapt grid of fullrun
# use "cdo selindexbox,idx1,idx2,idy1,idy2 infile outfile"
echo ">> cdo -z zip selindexbox,1,360,2,179 ${infile} ${infile/.nc/_trimmed.nc}"
cdo -z zip selindexbox,1,360,2,179 ${infile} ${infile/.nc/_trimmed.nc}

echo ""
echo "Inpath was: $(dirname ${infile})"
echo ""
echo "Finished."
