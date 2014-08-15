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
            
              datasetSelector              = "gCombobox",    # formerly nom_data
              datasetCatalog               = "character",    # new way of tracking dataset names
              currentDataSet               = "data.frame",   # active data_set being analysed
              
              numberOfAccessions           = "guiComponent", # formerly num.access
              numberOfContinuousVariables  = "guiComponent", # formerly ncon
              numberOfCategoricalVariables = "guiComponent", # formerly ncat
              percentageOfSolutions        = "guiComponent", # formerly npercen
              numberOfSolutions            = "guiComponent", # formerly Nsim
              numberOfFinalSolutions       = "guiComponent", # formerly nfinal
              optimizationResult           = "list"          # output.opt from algorithm$f.optimization
          )
        )

#
# Accessor Methods
#

setGeneric("datasetSelector",              function(x) standardGeneric("datasetSelector"))
setGeneric("datasetCatalog",               function(x) standardGeneric("datasetCatalog"))
setGeneric("currentDataSet",               function(x) standardGeneric("currentDataSet"))
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

setMethod(  "currentProjectFolder",  
            "ExploraAnalysis", 
            function(x) { 
              if(!is.null(attr(x@currentDataSet,"projectFolder"))) {
                return(attr(x@currentDataSet,"projectFolder"))
              } else {
                return(NA)
              }
            } 
)

setMethod("numberOfAccessions",           "ExploraAnalysis",function(x) x@numberOfAccessions )
setMethod("numberOfContinuousVariables",  "ExploraAnalysis",function(x) x@numberOfContinuousVariables )
setMethod("numberOfCategoricalVariables", "ExploraAnalysis",function(x) x@numberOfCategoricalVariables )
setMethod("percentageOfSolutions",        "ExploraAnalysis",function(x) x@percentageOfSolutions )
setMethod("numberOfSolutions",            "ExploraAnalysis",function(x) x@numberOfSolutions )
setMethod("numberOfFinalSolutions",       "ExploraAnalysis",function(x) x@numberOfFinalSolutions )
setMethod("optimizationResult",           "ExploraAnalysis",function(x) x@optimizationResult )

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


