#------------------------------------------------------------------------------------------------------------------
# Installation Script for Explora only needs to be run once to install packages in the library.
# Extracted out from original version of Explora coded by Johannes Ospina
#------------------------------------------------------------------------------------------------------------------ 
## Installing packages
install.packages("gWidgets") ## This package is used to generate graphical interface
install.packages("gWidgetsRGtk2") ## This package is used to generate graphical interface
install.packages("plyr") ## Function "empty"
install.packages("vegan")  ## Calculate Shannon index
install.packages("cluster")  ## Cluster analysis
install.packages("ade4")  ## principal component
# install.packages("grid")  ## generate tables in plot - superseded by gridbase which integrates base+grid?
install.packages("gridBase")  ## generate tables in plot
install.packages("gridExtra")  ## generate tables in plot