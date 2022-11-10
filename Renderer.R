library(rmarkdown)
library(tidyverse)
library(fs)
source('emep_qaqc_user_input.R')

rmarkdown::render('EMEP_QAQC_Report.Rmd', output_file = path(report_pth_out, REPORT_FNAME))