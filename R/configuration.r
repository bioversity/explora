#------------------------------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                                           #
#AUTHOR: RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
#VERSION 2.0 - AUGUST-04-2014 
#
# configuration.r - Explora Configuration is stored in the ExploraAnalysis class
#------------------------------------------------------------------------------------------------------------------ 
  
# Class for global configuration variables

#' @importFrom methods setClass
#' @importFrom methods setGeneric
#' @importFrom methods setMethod
#' @importFrom methods setReplaceMethod
#' @importFrom methods new

#' @importClassesFrom gWidgets gCombobox
#' @importClassesFrom gWidgets guiComponent

setClass( "ExploraAnalysis", 
          representation(
              dataset_selection  = "gCombobox",    # formerly nom_data
              numContVar         = "guiComponent", # formerly ncon
              percentSoln        = "guiComponent", # formerly npercen
              numberSoln         = "guiComponent", # formerly Nsim
              optimizationResult = "list",         # output.opt from algorithm$f.optimization
              datasetCatalog     = "vector",       # new way of tracking dataset names
              currentDataSet     = "data.frame"    # data_set loaded (in gui.R)
          )
        )

setGeneric("dataset_selection",  function(x) standardGeneric("dataset_selection"))
setGeneric("numContVar",         function(x) standardGeneric("numContVar"))
setGeneric("percentSoln",        function(x) standardGeneric("percentSoln"))
setGeneric("numberSoln",         function(x) standardGeneric("numberSoln"))
setGeneric("optimizationResult", function(x) standardGeneric("optimizationResult"))
setGeneric("datasetCatalog",     function(x) standardGeneric("datasetCatalog"))
setGeneric("currentDataSet",     function(x) standardGeneric("currentDataSet"))

setMethod("dataset_selection",  "ExploraAnalysis",function(x) x@dataset_selection )
setMethod("numContVar",         "ExploraAnalysis",function(x) x@numContVar )
setMethod("percentSoln",        "ExploraAnalysis",function(x) x@percentSoln )
setMethod("numberSoln",         "ExploraAnalysis",function(x) x@numberSoln )
setMethod("optimizationResult", "ExploraAnalysis",function(x) x@optimizationResult )
setMethod("datasetCatalog",     "ExploraAnalysis",function(x) x@datasetCatalog )
setMethod("currentDataSet",     "ExploraAnalysis",function(x) x@currentDataSet )

setGeneric("dataset_selection<-",  function(x,value) standardGeneric("dataset_selection<-"))
setGeneric("numContVar<-",         function(x,value) standardGeneric("numContVar<-"))
setGeneric("percentSoln<-",        function(x,value) standardGeneric("percentSoln<-"))
setGeneric("numberSoln<-",         function(x,value) standardGeneric("numberSoln<-"))
setGeneric("optimizationResult<-", function(x,value) standardGeneric("optimizationResult<-"))
setGeneric("datasetCatalog<-",     function(x,value) standardGeneric("datasetCatalog<-"))
setGeneric("currentDataSet<-",     function(x,value) standardGeneric("currentDataSet<-"))

setReplaceMethod(
  "dataset_selection",
  "ExploraAnalysis", 
  function(x,value) { 
    x@dataset_selection <- value
    return(x)
  }
)

setReplaceMethod(
  "numContVar",
  "ExploraAnalysis",
  function(x,value) { 
    x@numContVar <- value
    return(x)
  }
)

setReplaceMethod(
  "percentSoln",
  "ExploraAnalysis",
  function(x,value) { 
    x@percentSoln <- value
    return(x)
  }
)

setReplaceMethod(
  "numberSoln",
  "ExploraAnalysis",
  function(x,value) { 
    x@numberSoln <- value
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

setReplaceMethod(
  "datasetCatalog",
  "ExploraAnalysis",
  function(x,value) { 
    x@datasetCatalog <- c( x@datasetCatalog, value) 
    return(x)
  }
)

setReplaceMethod(
  "currentDataSet",
  "ExploraAnalysis",
  function(x,value) { 
    x@currentDataSet <- value
    return(x)
  }
)


