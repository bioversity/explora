#------------------------------------------------------------------------------------------------------------------
# APPLICATION BIOVERSITY                                                                                           #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
# VERSION 2.0 - AUGUST-04-2014 
#
# Explora Data Loaders
#------------------------------------------------------------------------------------------------------------------ 

load = function(file){file = read.csv(file, header = T, sep = ",")}## This function used to load csv files


load_dataset <- function(){## Load dataset
	data_set = load(gfile(""))
	
	ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"'Results' folder already exists?")
	write.csv(data_set, file = paste(getwd(),"/Results/data_set.csv", sep = ""),
			row.names = FALSE)
	return(data_set)
}