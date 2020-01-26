
#--------------------------------------------------------------------------------
# Load parameters and functions
#--------------------------------------------------------------------------------
source("code/setup/parameters.R")
source("code/r_functions/functions.R")

#--------------------------------------------------------------------------------
# Packages
#--------------------------------------------------------------------------------
library(knitr)
library(tidyverse)
library(srvyr)
#library(ergm.ego)
library(egonet)
library(plotly)
library(htmltools)
library(DT)
#library(Rjkb)
#library(Hmisc)
#library(descr)
#--------------------------------------------------------------------------------
# Knitr
#--------------------------------------------------------------------------------
opts_chunk$set(echo=FALSE, message=FALSE, warning=FALSE)
options(knitr.table.format = "html") 

#--------------------------------------------------------------------------------
# DataTable 
#--------------------------------------------------------------------------------
# Control default number of rows displayed
# Make sure there's a horizontal scroll bar for wide tables
options(DT.options = list(lengthMenu = tables_display_nrows,
			  scrollX = TRUE))

# https://github.com/rstudio/DT/issues/67
# You need this code to conduct the magic dependences attaching...
# Without it, the loops with DTs in them did NOT print
# However, it seems to generate a funky empty table when it's in this file. 
# So, instead we include it in each chapter rmd file separately
# DT::datatable(matrix())



