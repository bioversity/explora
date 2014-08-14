#---------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                          #                                 #
#AUTHOR: RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                               #
#VERSION 2.0 - AUGUST-04-2014                                                    #
#                                                                                #
# configuration.r - Explora Configuration is stored in the ExploraAnalysis class #
#--------------------------------------------------------------------------------- 
  
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
              datasetSelector  = "gCombobox",    # formerly nom_data
              numContVar         = "guiComponent", # formerly ncon
              percentSoln        = "guiComponent", # formerly npercen
              numberSoln         = "guiComponent", # formerly Nsim
              optimizationResult = "list",         # output.opt from algorithm$f.optimization
              datasetCatalog     = "character",    # new way of tracking dataset names
              currentDataSet     = "data.frame"    # data_set loaded (in gui.R)
          )
        )

setGeneric("datasetSelector",  function(x) standardGeneric("datasetSelector"))
setGeneric("numContVar",         function(x) standardGeneric("numContVar"))
setGeneric("percentSoln",        function(x) standardGeneric("percentSoln"))
setGeneric("numberSoln",         function(x) standardGeneric("numberSoln"))
setGeneric("optimizationResult", function(x) standardGeneric("optimizationResult"))
setGeneric("datasetCatalog",     function(x) standardGeneric("datasetCatalog"))
setGeneric("currentDataSet",     function(x) standardGeneric("currentDataSet"))

setMethod("datasetSelector",  "ExploraAnalysis",function(x) x@datasetSelector )
setMethod("numContVar",         "ExploraAnalysis",function(x) x@numContVar )
setMethod("percentSoln",        "ExploraAnalysis",function(x) x@percentSoln )
setMethod("numberSoln",         "ExploraAnalysis",function(x) x@numberSoln )
setMethod("optimizationResult", "ExploraAnalysis",function(x) x@optimizationResult )
setMethod("datasetCatalog",     "ExploraAnalysis",function(x) x@datasetCatalog )
setMethod("currentDataSet",     "ExploraAnalysis",function(x) x@currentDataSet )

setGeneric("datasetSelector<-",  function(x,value) standardGeneric("datasetSelector<-"))
setGeneric("numContVar<-",         function(x,value) standardGeneric("numContVar<-"))
setGeneric("percentSoln<-",        function(x,value) standardGeneric("percentSoln<-"))
setGeneric("numberSoln<-",         function(x,value) standardGeneric("numberSoln<-"))
setGeneric("optimizationResult<-", function(x,value) standardGeneric("optimizationResult<-"))
setGeneric("datasetCatalog<-",     function(x,value) standardGeneric("datasetCatalog<-"))
setGeneric("addDataset<-",         function(x,value) standardGeneric("addDataset<-"))
setGeneric("currentDataSet<-",     function(x,value) standardGeneric("currentDataSet<-"))

setReplaceMethod(
  "datasetSelector",
  "ExploraAnalysis", 
  function(x,value) { 
    x@datasetSelector <- value
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
    x@datasetCatalog <- sub("\\.explora","",as.character(value)) 
    return(x)
  }
)

setReplaceMethod(
  "addDataset",
  "ExploraAnalysis",
  function(x,value) { 
    x@datasetCatalog    <- sub("\\.explora","",c( x@datasetCatalog, as.character(value))) 
    x@datasetSelector[] <- x@datasetCatalog
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


