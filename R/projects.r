#------------------------------------------------------------------------------------------------
# APPLICATION BIOVERSITY                                                                        #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM #
# VERSION 2.0 - AUGUST-04-2014                                                                  #
#                                                                                               #
# projects.r - Explora Dataset Project Management                                               #
#------------------------------------------------------------------------------------------------

#' @importFrom methods setClass
#' @importFrom methods setGeneric
#' @importFrom methods setMethod
#' @importFrom methods setReplaceMethod
#' @importFrom methods new

#' @importFrom gWidgets gfile
#' @importClassesFrom gWidgets gCombobox
#' @importClassesFrom gWidgets gLayout
#' @importClassesFrom gWidgets guiComponent

# ExploraAnalysis - S4 Class for global project data management
setClass( "ExploraAnalysis", 
          representation(
            
            datasetSelector              = "gCombobox",    # formerly nom_data (abbreviated Spanglish for "nombre de datos"?)
            datasetCatalog               = "character",    # new way of tracking dataset names
            currentDataSet               = "data.frame",   # active dataset being analysed

            numberOfAccessions           = "guiComponent", # formerly num.access
            numberOfContinuousVariables  = "guiComponent", # formerly ncon
            numberOfCategoricalVariables = "guiComponent", # formerly ncat
            percentageOfSolutions        = "guiComponent", # formerly npercen
            numberOfSolutions            = "guiComponent", # formerly Nsim
            numberOfFinalSolutions       = "guiComponent", # formerly nfinal
            optimizationResult           = "list"          # output.opt from algorithm.r f.optimization()
          )
)

# 
# Accessor Methods
#

setGeneric("datasetSelector",              function(x) standardGeneric("datasetSelector"))
setGeneric("datasetCatalog",               function(x) standardGeneric("datasetCatalog"))
setGeneric("currentDataSet",               function(x) standardGeneric("currentDataSet"))
setGeneric("currentProjectName",           function(x) standardGeneric("currentProjectName"))
setGeneric("currentProjectFolder",         function(x) standardGeneric("currentProjectFolder"))

setGeneric("numberOfAccessions",           function(x) standardGeneric("numberOfAccessions"))
setGeneric("numberOfContinuousVariables",  function(x) standardGeneric("numberOfContinuousVariables"))
setGeneric("numberOfCategoricalVariables", function(x) standardGeneric("numberOfCategoricalVariables"))
setGeneric("percentageOfSolutions",        function(x) standardGeneric("percentageOfSolutions"))
setGeneric("numberOfSolutions",            function(x) standardGeneric("numberOfSolutions"))
setGeneric("numberOfFinalSolutions",       function(x) standardGeneric("numberOfFinalSolutions"))
setGeneric("optimizationResult",           function(x) standardGeneric("optimizationResult"))

setMethod("datasetSelector",              "ExploraAnalysis",function(x) x@datasetSelector )
setMethod("datasetCatalog",               "ExploraAnalysis",function(x) x@datasetCatalog )
setMethod("currentDataSet",               "ExploraAnalysis",function(x) x@currentDataSet )

setMethod(  "currentProjectName",  
            "ExploraAnalysis", 
            function(x) { 
              if(!is.null(attr(x@currentDataSet,"projectFolder"))) {
                return( attr(x@currentDataSet,"identifier") )
              } else {
                return(NA)
              }
            } 
)

setMethod(  "currentProjectFolder",  
            "ExploraAnalysis", 
            function(x) { 
              if(!is.null(attr(x@currentDataSet,"projectFolder"))) {
                return( attr(x@currentDataSet,"projectFolder") )
              } else {
                return(NA)
              }
            } 
)

setMethod("numberOfAccessions",           "ExploraAnalysis", function(x) x@numberOfAccessions )
setMethod("numberOfContinuousVariables",  "ExploraAnalysis", function(x) x@numberOfContinuousVariables )
setMethod("numberOfCategoricalVariables", "ExploraAnalysis", function(x) x@numberOfCategoricalVariables )
setMethod("percentageOfSolutions",        "ExploraAnalysis", function(x) x@percentageOfSolutions )
setMethod("numberOfSolutions",            "ExploraAnalysis", function(x) x@numberOfSolutions )
setMethod("numberOfFinalSolutions",       "ExploraAnalysis", function(x) x@numberOfFinalSolutions )
setMethod("optimizationResult",           "ExploraAnalysis", function(x) x@optimizationResult )

#
# Replacement Methods
#

setGeneric("datasetSelector<-",              function(x,value) standardGeneric("datasetSelector<-"))
setGeneric("datasetCatalog<-",               function(x,value) standardGeneric("datasetCatalog<-"))
setGeneric("addDataset<-",                   function(x,value) standardGeneric("addDataset<-"))
setGeneric("currentDataSet<-",               function(x,value) standardGeneric("currentDataSet<-"))

setGeneric("numberOfAccessions<-",           function(x,value) standardGeneric("numberOfAccessions<-"))
setGeneric("numberOfContinuousVariables<-",  function(x,value) standardGeneric("numberOfContinuousVariables<-"))
setGeneric("numberOfCategoricalVariables<-", function(x,value) standardGeneric("numberOfCategoricalVariables<-"))
setGeneric("percentageOfSolutions<-",        function(x,value) standardGeneric("percentageOfSolutions<-"))
setGeneric("numberOfSolutions<-",            function(x,value) standardGeneric("numberOfSolutions<-"))
setGeneric("numberOfFinalSolutions<-",       function(x,value) standardGeneric("numberOfFinalSolutions<-"))
setGeneric("optimizationResult<-",           function(x,value) standardGeneric("optimizationResult<-"))

setReplaceMethod(
  "window",
  "ExploraAnalysis", 
  function(x,value) { 
    x@window <- value
    return(x)
  }
)
setReplaceMethod(
  "datasetSelector",
  "ExploraAnalysis", 
  function(x,value) { 
    x@datasetSelector <- value
    return(x)
  }
)
setReplaceMethod(
  "datasetCatalog",
  "ExploraAnalysis",
  function(x,value) { 
    x@datasetCatalog <- sub("\\.explora","",as.character(value)) 
    return(x)
  }
)

EMPTY_CATALOG <- function() { return ("No Projects Loaded yet!") }

setReplaceMethod(
  "addDataset",
  "ExploraAnalysis",
  function(x,value) { 
    if(x@datasetCatalog[[1]] == EMPTY_CATALOG() ) {
      # overwrite catalog if it was "empty"
      x@datasetCatalog   <- sub("\\.explora","", as.character(value))
    } else {
      # otherwise, append
      x@datasetCatalog <- c( x@datasetCatalog, sub("\\.explora","", as.character(value)))
    }
    x@datasetSelector[] <- x@datasetCatalog
    return(x)
  }
)

# This method the currentDataSet to the specified data.frame
setReplaceMethod(
  "currentDataSet",
  signature(x="ExploraAnalysis",value="data.frame"),
  function(x,value) { 
    x@currentDataSet <- value
    return(x)
  }
)

setReplaceMethod(
  "numberOfAccessions",
  "ExploraAnalysis",
  function(x,value) { 
    x@numberOfAccessions <- value
    return(x)
  }
)

setReplaceMethod(
  "numberOfContinuousVariables",
  "ExploraAnalysis",
  function(x,value) { 
    x@numberOfContinuousVariables <- value
    return(x)
  }
)

setReplaceMethod(
  "numberOfCategoricalVariables",
  "ExploraAnalysis",
  function(x,value) { 
    x@numberOfCategoricalVariables <- value
    return(x)
  }
)
setReplaceMethod(
  "percentageOfSolutions",
  "ExploraAnalysis",
  function(x,value) { 
    x@percentageOfSolutions <- value
    return(x)
  }
)

setReplaceMethod(
  "numberOfSolutions",
  "ExploraAnalysis",
  function(x,value) { 
    x@numberOfSolutions <- value
    return(x)
  }
)

setReplaceMethod(
  "numberOfFinalSolutions",
  "ExploraAnalysis",
  function(x,value) { 
    x@numberOfFinalSolutions <- value
    return(x)
  }
)

setReplaceMethod(
  "optimizationResult",
  "ExploraAnalysis",
  function(x,value) { 
    x@optimizationResult <- value
    return(x)
  }
)


## This function used to load csv files
createProject <- function(){
  
  data_file_name <- gfile(text="",filter = list("csv data files" = list(patterns = c("*.csv"))))
  
  if( is.na(data_file_name) ){ return(NA) }
  
  dataset_name <- sub("\\.csv$","", basename(data_file_name), ignore.case = TRUE)

  print(paste("Loading dataset: ",dataset_name))
  
  # loads external data into internal data.frame
	dataset <- read.csv(data_file_name, header = TRUE, sep = ",") 
  
  # massage the dataset a bit to make it a bit more usable...
  
  # In later iterations of the application, more massaging may be attempted
  # for example, analysis and remapping of trait column headers (to point to Crop Ontology?)
  # For now, simply relabel the first column name from "ID" to "accession"
  names(dataset)[1] <- "accession"
	
  projectFolder = file.path( getwd(),paste(dataset_name,".explora",sep="") )
  print(paste("Creating project directory: ", projectFolder))
  
  ifelse( 
      file.exists(projectFolder)=="FALSE", 
      dir.create(projectFolder,recursive=TRUE), 
      paste("Project folder '",projectFolder,"' already exists?") 
  )
  
	write.csv(dataset, file = file.path( projectFolder, paste(dataset_name,".csv", sep = "")), row.names = FALSE)

  attr(dataset,"identifier")    <- dataset_name
  attr(dataset,"projectFolder") <- projectFolder
  
	return( dataset )
}

getProjects <- function() {
  
  dirs <- list.dirs(getwd(),full.names=FALSE, recursive=FALSE)
  dirs <- dirs[length(dirs)>0 & grepl("\\.explora$",dirs)]
  if(length(dirs) == 0) {
    print(EMPTY_CATALOG())
    dirs = c(EMPTY_CATALOG())
  }
  return(dirs)
}

#
# The result.path function builds a valid project file path, if it can
#
result.path <- function( filename, filext ) {
  
  projectFolder <- currentProjectFolder(analysis)
  
  if( nchar(projectFolder)>0 & 
      file.exists(projectFolder) & 
      nchar(filename)>0 
  ) {
    
    path = file.path( projectFolder, paste( filename, ".", filext, sep="") )
    return(path) 
  }
  
  return(NA)
}

#
# The result.path function checks the full 
# existence of a csv data file in the projectFolder
#
result.path.exists <- function( filename, filext ) {
  
  path <- result.path( filename, filext )
  
  if( !is.na(path) ) {
    
    if(file.exists(path)) { 
      
      return(path) 
      
    }
    
  } 
  return(NA)
  
}

#
# Defining the general purpose dialog box here 
# in projects, rather than in dialogs.r
# so it can be used in data loading functions below
#
DialogBox <- function(message, handler=NULL) {## This function make a dialog box
  
  w<- gwindow("Alert",width=100,height=100)
  g <- ggroup( container = w)
  gimage("info", dirname="stock", container = g)
  
  ig <- ggroup(horizontal = FALSE,  container = g)
  glabel(message,  container = ig, expand = TRUE)
  
  bg <- ggroup( container = ig)
  addSpring(bg)
  gbutton("Ok", handler = function(h,...) dispose(w),  container = bg)
  
  return()
}  


#
# This function serves as a common mechanism 
# for saving project analysis results as a CSV file
#
# Results and filename are assumed non-empty
# to be set to valid values here(?). A simple
# sanity check made to test this assumption
#
saveProjectFile <- function( results, filename, row.names = TRUE, alert = TRUE) {
  
  if( is.table(results) | is.data.frame(results) | is.matrix(results) ) {
    
    path <- result.path( filename, "csv" )
    
    if( !is.na(path) ) {
      
      projectName <- currentProjectName(analysis)
      
      if(alert) { 
        DialogBox( paste("'", filename,"' data file published\nin project folder '", projectName,"'") )
      }
      
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
  
  if( !is.na(path) ) {
    
    projectName <- currentProjectName(analysis)
    
    DialogBox( paste("'", filename,"'\n data image published to project folder '", projectName,"'") )
    
    png(path, width = width, height = height, res = res)
    
    return(TRUE)
  }
  
  DialogBox(paste("Warning: could not open plot image file '", filename,"'?", sep="" ))
  
  return(FALSE)
}


#
# Reciprocal of saveProjectFile, this readProjectFile function 
# retrieves a previously saved csv project data file;
# Singular file assumed here(?), should exists when this function is called
#
readProjectFile <- function( filename ) {
  
    path <- result.path.exists( filename, "csv" )
    
    if( !is.na(path) ) {
      
        data <- read.csv(path)
        return(data)
        
    } else {
      
      return(NA)
      
    }
}

# Dataset is assumed to exist as a project but may not yet be loaded.
# First iteration simply reads the dataset from the file system afresh each time.
# We could later try to cache the data insteadbut perhaps not worth the hassle, 
# unless this becomes an Explora performance issue)
loadDataset <- function( datasetId ) {
  # first, manually build the expected project path
  projectFolder = file.path( getwd(),paste( datasetId,".explora", sep="") )
  
  # sanity check - does the project really exist at this location?
  if( file.exists(projectFolder) ) {
  
    # second, manually build the dataset file path
    path = file.path( projectFolder, paste( datasetId, ".csv", sep="") )
    
    # third, check if the file exists
    if( file.exists(path) ) {
      
      # if so, then read it in!
      dataset <- read.csv(path)
      
      # don't forget to annotate it a bit
      attr(dataset,"identifier")    <- datasetId
      attr(dataset,"projectFolder") <- projectFolder
      
      return(dataset)
    }
  }
  # otherwise, fail to return the dataset
  return(NA)
}


# Definition of this replacement method is deferred to this point
# so that it can use the above defined readProjectFile() function

# This method uses a dataset identifier to reset the currentDataSet 
# to a specified dataset data.frame 
setReplaceMethod(
  "currentDataSet",
  signature(x="ExploraAnalysis", value="character"),
  function(x,value) { 
    x@currentDataSet <- loadDataset( value )
    return(x)
  }
)

  

