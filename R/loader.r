#------------------------------------------------------------------------------------------------------------------
# APPLICATION BIOVERSITY                                                                                           #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
# VERSION 2.0 - AUGUST-04-2014 
#
# loader.r - Explora Data Loaders
#------------------------------------------------------------------------------------------------------------------ 

#' @include configuration.r

#' @importFrom gWidgets gfile

## This function used to load csv files
load_dataset <- function(){
  
  data_file_name <- gfile("")
  data_set_name <- sub("\\.csv$","", basename(data_file_name), ignore.case = TRUE)

  print(paste("Loading dataset: ",data_set_name))
  
	data_set <- read.csv(data_file_name, header = T, sep = ",") 
	
  project_dir = file.path(getwd(),data_set_name,sep="")
  ifelse( 
      file.exists(project_dir)=="FALSE", 
      dir.create(project_dir,recursive=TRUE), 
      paste("Project folder '",project_dir,"' already exists?") 
  )
  
	write.csv(data_set, file = file.path(project_dir,paste(data_set_name,".csv"), sep = ""), row.names = FALSE)

  attr(data_set,"identifier") <- data_set_name
  
	return( data_set )
}
