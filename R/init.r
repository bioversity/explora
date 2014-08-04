#------------------------------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                                           #
#AUTHOR: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
#VERSION 2.0 - AUGUST-04-2014 
#
# Initialize Explora Environment
#------------------------------------------------------------------------------------------------------------------ 

## Loading packages (installed by install.R)
library(vegan) 
library(ade4)
library(gWidgets) 
library(gWidgetsRGtk2) 
library(plyr)
library(cluster)
library(grid)
library(gridExtra)

## Clean work spaces
rm(list=ls())

## select tools for GUI
options("guiToolkit"="RGtk2")

## Change locale for message in english
Sys.setlocale(category = "LC_ALL", locale = "English")
Sys.setenv(LANG = "en")

# Where am I?
nframe <- sys.nframe()
#print(paste("nframe:",nframe))
if(nframe>0) {
	script_dir <- dirname(c(sys.frame(nframe)$ofile,""))
} else {
	script_dir <- getwd()
}

#print(c("Script Directory: ",script_dir))

image_dir <- paste(script_dir,"../inst/image")