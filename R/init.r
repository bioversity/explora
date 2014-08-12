#------------------------------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                                           #
#AUTHOR: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
#VERSION 2.0 - AUGUST-04-2014 
#
# Initialize Explora Environment
#------------------------------------------------------------------------------------------------------------------ 

## Clean work spaces
# what am I cleaning here? Is it necessary?
rm(list=ls())

#' @import gWidgetsRGtk2
library ("gWidgetsRGtk2")

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
#print(c("Image Directory: ",image_dir))

# Global class for application variables

#' @importClassesFrom gWidgets gCombobox
#' @importClassesFrom gWidgets guiComponent

setClass( "ExploraAnalysis", 
          representation(
            dataset_selection = "gCombobox",    # formerly nom_data
            numContVar        = "guiComponent", # formerly ncon
            percentSoln       = "guiComponent", # formerly npercen
            numberSoln        = "guiComponent", # formerly Nsim
            optimizationResult      = "list"          # output.opt from algorithm$f.optimizationResult
          )
        )

setGeneric("dataset_selection", function(x) standardGeneric("dataset_selection"))
setGeneric("numContVar",        function(x) standardGeneric("numContVar"))
setGeneric("percentSoln",       function(x) standardGeneric("percentSoln"))
setGeneric("numberSoln",        function(x) standardGeneric("numberSoln"))
setGeneric("optimizationResult",      function(x) standardGeneric("optimizationResult"))

setMethod("dataset_selection","ExploraAnalysis",function(x) x@dataset_selection )
setMethod("numContVar",       "ExploraAnalysis",function(x) x@numContVar )
setMethod("percentSoln",      "ExploraAnalysis",function(x) x@percentSoln )
setMethod("numberSoln",       "ExploraAnalysis",function(x) x@numberSoln )
setMethod("optimizationResult",     "ExploraAnalysis",function(x) x@optimizationResult )

setGeneric("dataset_selection<-", function(x,value) standardGeneric("dataset_selection<-"))
setGeneric("numContVar<-",        function(x,value) standardGeneric("numContVar<-"))
setGeneric("percentSoln<-",       function(x,value) standardGeneric("percentSoln<-"))
setGeneric("numberSoln<-",        function(x,value) standardGeneric("numberSoln<-"))
setGeneric("optimizationResult<-",      function(x,value) standardGeneric("optimizationResult<-"))

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
analysis = new("ExploraAnalysis")
