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
  
  data_file_name <- gfile(text="",filter = list("csv data files" = list(patterns = c("*.csv"))))
  
  if(is.na(data_file_name)){ return(NA) }
  
  data_set_name <- sub("\\.csv$","", basename(data_file_name), ignore.case = TRUE)

  print(paste("Loading dataset: ",data_set_name))
  
	data_set <- read.csv(data_file_name, header = T, sep = ",") 
	
  projectFolder = file.path(getwd(),paste(data_set_name,".explora",sep=""))
  print(paste("Creating project directory: ", projectFolder))
  
  ifelse( 
      file.exists(projectFolder)=="FALSE", 
      dir.create(projectFolder,recursive=TRUE), 
      paste("Project folder '",projectFolder,"' already exists?") 
  )
  
	write.csv(data_set, file = file.path( projectFolder, paste(data_set_name,".csv"), sep = ""), row.names = FALSE)

  attr(data_set,"identifier")    <- data_set_name
  attr(data_set,"projectFolder") <- projectFolder
  
	return( data_set )
}

getProjects <- function() {
  
  dirs <- list.dirs(getwd(),full.names=FALSE, recursive=FALSE)
  dirs <- dirs[length(dirs)>0 & grepl("\\.explora$",dirs)]
  
  return(dirs)
}

#
# The result.path function tests the 
# availability of a file
#
result.path <- function( filename, filext ) {
  
  projectFolder <- currentProjectFolder(analysis)
  
  if( nchar(projectFolder)>0 & 
      file.exists(projectFolder) & 
      nchar(filename)>0 
  ) {
    
    path = file.path( projectFolder, paste( filename, ".", filext, sep="") )
    
    if( file.exists(path) ) {
      return(path) 
    }
  }
  
  return(FALSE)
}

#
# This function serves as a common mechanism 
# for saving project analysis results as a CSV file
#
# Results and filename are assumed non-empty
# to be set to valid values here(?). A simple
# sanity check made to test this assumption
#
saveResults <- function( results, filename, row.names = TRUE ) {
  
  if( is.data.frame(results) ) {
    
    path <- result.path( filename, "csv" )
    
    if( path ) {
      
      DialogBox( paste("Result data posted to: ", path) )
      
      write.csv( results, file = path, row.names = row.names)
      
      return(TRUE)
    }
  } 
  
  DialogBox(paste("Error: could not save analysis results for '", filename,"'?", sep="" ))
  
  return(FALSE)
}

#
# opens up a PNG device to the specified file
#
plotImage <- function( filename, width = 2000, height = 1000, res = NA ) {
  
  path <- result.path( filename, "png" )
  
  if( path ) {
    
    DialogBox( paste("Result image posted to: ", path) )
    
    png(path, width = width, height = height, res = res)
    
    return(TRUE)
  }
  
  DialogBox(paste("Warning: could not open plot image file '", filename,"'?", sep="" ))
  
  return(FALSE)
}


#
# Reciprocal of saveResults, this readResults function 
# retrieves a previously saved result file;
# Singular file assumed here(?)
#
readResults <- function( filename ) {
  
    path <- result.path( filename, "csv" )
    
    if( path ) {
      
        data <- read.csv(path)
        return(data)
    }
}
