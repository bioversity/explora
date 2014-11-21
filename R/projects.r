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
#' @importClassesFrom gWidgets gWindow
#' @importClassesFrom gWidgets guiComponent

# ExploraAnalysis - S4 Class for global project data management
setClass( "ExploraAnalysis", 
          representation(
            
            mainWindow                   = "gWindow",    # formerly win (main graphical window)
            
            datasetSelector              = "gCombobox",    # formerly nom_data (abbreviated Spanglish for "nombre de datos"?)
            datasetCatalog               = "character",    # new way of tracking dataset names
            dataAnalysisTag              = "guiComponent", # character label with which to tag a agiven analysis run
            currentDataSet               = "data.frame",   # active dataset being analysed

            targetNumberOfAccessions     = "guiComponent", # formerly num.access
            numberOfContinuousVariables  = "guiComponent", # formerly ncon
            numberOfCategoricalVariables = "guiComponent", # formerly ncat
            coefficientOfCorrelation     = "guiComponent", # formerly ncor
            percentageOfSolutions        = "guiComponent", # formerly npercen
            numberOfSolutions            = "guiComponent", # formerly Nsim
            numberOfFinalSolutions       = "guiComponent", # formerly nfinal
            sampleDistribution           = "list"          # output.opt from algorithm.r generateSampleDistribution()
          )
)

# 
# Accessor Methods
#

# Generics

setGeneric("mainWindow",                   function(x) standardGeneric("mainWindow"))

setGeneric("datasetSelector",              function(x) standardGeneric("datasetSelector"))
setGeneric("datasetCatalog",               function(x) standardGeneric("datasetCatalog"))
setGeneric("dataAnalysisTag",              function(x) standardGeneric("dataAnalysisTag"))
setGeneric("currentDataSet",               function(x) standardGeneric("currentDataSet"))
setGeneric("currentProjectName",           function(x) standardGeneric("currentProjectName"))
setGeneric("currentProjectFolder",         function(x) standardGeneric("currentProjectFolder"))

setGeneric("targetNumberOfAccessions",     function(x) standardGeneric("targetNumberOfAccessions"))
setGeneric("numberOfContinuousVariables",  function(x) standardGeneric("numberOfContinuousVariables"))
setGeneric("numberOfCategoricalVariables", function(x) standardGeneric("numberOfCategoricalVariables"))
setGeneric("coefficientOfCorrelation",     function(x) standardGeneric("coefficientOfCorrelation"))

setGeneric("percentageOfSolutions",        function(x) standardGeneric("percentageOfSolutions"))
setGeneric("numberOfSolutions",            function(x) standardGeneric("numberOfSolutions"))
setGeneric("numberOfFinalSolutions",       function(x) standardGeneric("numberOfFinalSolutions"))
setGeneric("sampleDistribution",           function(x) standardGeneric("sampleDistribution"))


# Getters

setMethod("mainWindow",                   "ExploraAnalysis",function(x) x@mainWindow )

setMethod("datasetSelector",              "ExploraAnalysis",function(x) x@datasetSelector )
setMethod("datasetCatalog",               "ExploraAnalysis",function(x) x@datasetCatalog )
setMethod("dataAnalysisTag",              "ExploraAnalysis",function(x) x@dataAnalysisTag )
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

setMethod("targetNumberOfAccessions",     "ExploraAnalysis", function(x) x@targetNumberOfAccessions )
setMethod("numberOfContinuousVariables",  "ExploraAnalysis", function(x) x@numberOfContinuousVariables )
setMethod("numberOfCategoricalVariables", "ExploraAnalysis", function(x) x@numberOfCategoricalVariables )
setMethod("coefficientOfCorrelation",     "ExploraAnalysis", function(x) x@coefficientOfCorrelation )

setMethod("percentageOfSolutions",        "ExploraAnalysis", function(x) x@percentageOfSolutions )
setMethod("numberOfSolutions",            "ExploraAnalysis", function(x) x@numberOfSolutions )
setMethod("numberOfFinalSolutions",       "ExploraAnalysis", function(x) x@numberOfFinalSolutions )
setMethod("sampleDistribution",           "ExploraAnalysis", function(x) x@sampleDistribution )

# 
# Replacement Methods
#

setGeneric("mainWindow<-",                   function(x,value) standardGeneric("mainWindow<-"))

setGeneric("datasetSelector<-",              function(x,value) standardGeneric("datasetSelector<-"))
setGeneric("datasetCatalog<-",               function(x,value) standardGeneric("datasetCatalog<-"))
setGeneric("dataAnalysisTag<-",              function(x,value) standardGeneric("dataAnalysisTag<-"))
setGeneric("addDataset<-",                   function(x,value) standardGeneric("addDataset<-"))
setGeneric("currentDataSet<-",               function(x,value) standardGeneric("currentDataSet<-"))

setGeneric("targetNumberOfAccessions<-",     function(x,value) standardGeneric("targetNumberOfAccessions<-"))
setGeneric("numberOfContinuousVariables<-",  function(x,value) standardGeneric("numberOfContinuousVariables<-"))
setGeneric("numberOfCategoricalVariables<-", function(x,value) standardGeneric("numberOfCategoricalVariables<-"))
setGeneric("coefficientOfCorrelation<-",     function(x,value) standardGeneric("coefficientOfCorrelation<-"))

setGeneric("percentageOfSolutions<-",        function(x,value) standardGeneric("percentageOfSolutions<-"))
setGeneric("numberOfSolutions<-",            function(x,value) standardGeneric("numberOfSolutions<-"))
setGeneric("numberOfFinalSolutions<-",       function(x,value) standardGeneric("numberOfFinalSolutions<-"))
setGeneric("sampleDistribution<-",           function(x,value) standardGeneric("sampleDistribution<-"))


setReplaceMethod(
  "mainWindow",
  "ExploraAnalysis", 
  function(x,value) { 
    x@mainWindow <- value
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

setReplaceMethod(
  "dataAnalysisTag",
  "ExploraAnalysis",
  function(x,value) { 
    x@dataAnalysisTag <- value
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
  "targetNumberOfAccessions",
  "ExploraAnalysis",
  function(x,value) { 
    x@targetNumberOfAccessions <- value
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
  "coefficientOfCorrelation",
  "ExploraAnalysis",
  function(x,value) { 
    x@coefficientOfCorrelation <- value
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
  "sampleDistribution",
  "ExploraAnalysis",
  function(x,value) { 
    x@sampleDistribution <- value
    return(x)
  }
)

## This function used to load csv files and create a project
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
  
  # Before resaving, (silently?) remove any columns and rows for which ALL data is NA (missing) 
  # (believe it or not, some datasets have this strange problem!)
  dataset  <- as.matrix(dataset)
  
  # First, check for columns with completely missing data
  ColNA    <- apply(apply(dataset,2,is.na),2,all)
  if(!all(!ColNA)) {
    # warn the user that some data columns are completely missing data
    DialogBox("Some trait data column(s) full of missing data (NA)? Skipped...")
  }
  dataset  <- dataset[,!ColNA]
  
  # Then, check for rows with completely missing data 
  # (ignoring values in the accession id column)
  RowNA    <- apply(apply(dataset[,-1],2,is.na),1,all)
  if(!all(!RowNA)) {
    # warn the user that some data rows are completely missing data
    DialogBox("Some trait data rows full of missing data (NA)? Skipped...")
  }
  dataset <- dataset[!RowNA,]
  
  # restore the dataset as a data.frame before saving
  dataset  <- data.frame(dataset)
  
  projectFolder = file.path( getwd(),paste(dataset_name,".explora",sep="") )
  
  if( !file.exists(projectFolder) ) {
      print(paste("Creating project folder: ", projectFolder))
      dir.create(projectFolder,recursive=TRUE)
  } else {
      print(paste("Project folder '",projectFolder,"' already exists... no need to recreate?"))
  }
  
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
# The result.folder function returns the current analysis result folder
# which is constructed in the project folder based on the current dataAnalysisTag
# TODO: should this value be cached in the ExploraAnalysis class as well rather than computed each time?
#
result.folder <- function(context) {
  
  projectFolder <- currentProjectFolder(context)
  
  # retrieve the current value of the data analysis tag widget
  dataTag  <- as.character( svalue( dataAnalysisTag(context) ) )
  
  if( !is.na( dataTag ) ) {
    
    resultFolder = file.path( projectFolder, paste( "Analysis_", dataTag, sep="" ) )
    
    if( !file.exists(resultFolder) ) {
      print( paste( "Creating analysis results folder: ", resultFolder ))
      dir.create( resultFolder,recursive=TRUE )
    }

  } else {
    # no dataAnalysisTag declared, 
    # so just save data in the root project folder?
    resultFolder <- projectFolder
  }

  return(resultFolder)
}

#
# The result.path function builds a valid project result file path, if it can
#
result.path <- function( context, filename, filext ) {
  
  resultFolder <- result.folder(context)
  
  if( nchar(resultFolder)>0 & 
      file.exists(resultFolder) & 
      nchar(filename)>0 
  ) {
    
    path = file.path( resultFolder, paste( filename, ".", filext, sep="") )
    return(path) 
  }
  
  return(NA)
}

#
# The result.path function checks the full 
# existence of a csv data file in the projectFolder
#
result.path.exists <- function( context, filename, filext ) {
  
  path <- result.path( context, filename, filext )
  
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
# Setting the 'echo' switch to 'TRUE' forces a GUI display of the table of data.
#
saveProjectFile <- function( context, results, filename, row.names = TRUE, alert = FALSE, echo = FALSE ) {
  
  if( is.table(results) | is.data.frame(results) | is.matrix(results) ) {
    
    path <- result.path( context, filename, "csv" )
    
    if( !is.na(path) ) {
      
      if(alert) { 
        DialogBox( paste("Data published to file\n'", filename,"'") )
      }
      
      write.csv( results, file = path, row.names = row.names)
      
      if( echo ) {
        DataTableViewer( context, results, filename ) 
      }
      
      return(TRUE)
    }
  } 
  
  DialogBox(paste("Error: could not save analysis results for '", filename,"'?", sep="" ))
  
  return(FALSE)
}

deleteProjectFile <- function( context, filename ) {
  
    path <- result.path( context, filename, "csv" )
    
    if( !is.na(path) && file.exists(path) ) {  file.remove( c(path) ) }

}

#
# opens up a PNG device to the specified file
#
plotImage <- function( context, filename, width = 2000, height = 1000, res = NA, alert = FALSE ) {
  
  path <- result.path( context, filename, "png" )
  
  if( !is.na(path) ) {
    
    projectName <- currentProjectName(context)
    
    if( alert ) {
      DialogBox( paste("'", filename,"'\n data image published to project folder '", projectName,"'") )
    }
    
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
readProjectFile <- function( context, filename ) {
  
    path <- result.path.exists( context, filename, "csv" )
    
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

  

