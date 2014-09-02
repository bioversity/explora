#-----------------------------------------------------------------------------------------------#
# APPLICATION BIOVERSITY                                                                        #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM #
# VERSION 2.0 - AUGUST-04-2014                                                                  #
#                                                                                               #
# gui.r - Explora Graphical User Interface (based on gWidgets)                                  #
#-----------------------------------------------------------------------------------------------#
#' @include projects.r dialogs.r algorithms.r

#' @importFrom gWidgets gwindow
#' @importFrom gWidgets gnotebook
#' @importFrom gWidgets gframe
#' @importFrom gWidgets glayout
#' @importFrom gWidgets glabel
#' @importFrom gWidgets gbutton
#' @importFrom gWidgets gdroplist
#' @importFrom gWidgets gedit
#' @importFrom gWidgets gspinbutton
#' @importFrom gWidgets ggroup
#' @importFrom gWidgets gimage
#' @importFrom gWidgets visible
#' @importMethodsFrom gWidgets addHandlerChanged
#' 
#' @import gWidgetstcltk

#' @name workbench
#' @title Explora germplasm selection tool
#' @description Bioversity/CCAFS Seeds4Needs Explora germplasm selection tool
#' @details  The main purpose of Explora is to select a set of promising accessions 
#' from genebank collections that meet the interests of the user. Explora allows 
#' 1) selection of germplasm from big characterization and/or evaluation data sets; 
#' 2) consideration of more than one trait and trade-offs between different traits of interests; and 
#' 3) maximization of diversity for specific traits of interests.
#' @aliases workbench
#' @author Maarten van Zonneveld,  Johannes Ospina and Richard Bruskiewich
#' @export workbench
#' @examples
#' workbench()
workbench <- function() {
  
  ## Clean work spaces
  # what am I cleaning here? Is it necessary?
  rm(list=ls())
  
  ## select tools for GUI
  options("guiToolkit"="tcltk")
  
  ## Change locale for message in English
  Sys.setlocale(category = "LC_ALL", locale = "English")
  Sys.setenv(LANG = "en")
  
  # Global analysis parameters are kept in an S4 class instance of class "ExploraAnalysis"
  # and propagated into a special environment
  session  <- new.env()
  session$analysis <- new("ExploraAnalysis")
  
  datasetCatalog(session$analysis) <- getProjects() 
  
  # not elegant, but it is tricky to give these functions 
  # their session context in an encapsulated (functional) way
  environment(saveProjectFile)          <- session
  environment(deleteProjectFile)        <- session
  environment(readProjectFile)          <- session
  environment(result.path.exists)       <- session
  environment(result.path)              <- session
  
  environment(getTargetDataSet)         <- session
  
  environment(DialogSelectThresholds)   <- session
  environment(DialogSelectOptimization) <- session
  
  environment(number.access)            <- session
  environment(number.solutions)         <- session
  environment(number.final)             <- session
  environment(number.percent)           <- session
  
  environment(descriptors.continuous)   <- session
  environment(descriptors.nominal)      <- session
  environment(correlation)              <- session
  environment(f.optimization)           <- session
  environment(optimization)             <- session
  
  environment(MAXVAR.type.opt)          <- session
  environment(PCA.type.opt)             <- session
  environment(WSM.type.opt)             <- session
  environment(DTree.type.opt)           <- session 
  
	## Principal window
	win <- gwindow("Explora Germplasm Selection Tool", visible = F , width = 500, height = 300)
  
  datasetCatalog(session$analysis) <- getProjects() 
  
	nb  <- gnotebook( container = win, expand = T, tab.pos = 3)
  
  ## Welcome Window
  welcome <- ggroup(container = nb, horizontal = FALSE, label="Welcome")
  
  image_dir <- paste(path.package("explora"),"/images/",sep="")
  gimage("Explora_Logo.gif", dirname = image_dir, container = welcome) 
  
  ##########################
	## Load project datasets #
  ##########################
  lyt1        <- glayout(homogeneous = FALSE,  container = nb, spacing = 10, label = "Projects", expand = TRUE) 
	lyt1[1,1:3] <- g1 <- gframe("Projects",  container = lyt1, horizontal = TRUE)
	lytg1       <- glayout(homogeneous = FALSE, container = g1, spacing = 10, expand = TRUE) 

  lytg1[1:2,1:5] <- glabel( text = " ", container = lytg1)
  
	lytg1[3,1]     <-  h <- gbutton(
                        "Set Project Folder...",  
                        container = lytg1, 
                        handler = function(h,...) { 
                                    newdir <- gfile(text = "Select directory", type = "selectdir") 
                                    if(! is.na(newdir) )  { 
                                      setwd(newdir)
                                      datasetCatalog(session$analysis)    <- getProjects()
                                      datasetSelector(session$analysis)[] <- datasetCatalog(session$analysis)
                                    } 
                                  } 
                      )
                
	lytg1[3,2] <- glabel( text = " ", container = lytg1 )
	lytg1[3,3] <- gbutton(
                  "Load Datasets as Projects...",  
                  container = lytg1, 
                  expand = FALSE, 
                  handler = function(h,...){ 
                                dataset <- createProject()
                                dsl <- length(dataset)
                                if( dsl > 0 && !(dsl == 1 && is.na(dataset)) ) {
                                  addDataset(session$analysis)        <- attr(dataset,"identifier")
                                  currentDataSet(session$analysis)    <- dataset
                                  datasetSelector(session$analysis)[] <- datasetCatalog(session$analysis)
                                }
                            }
                )
  
  lytg1[4,1:5] <- glabel( text = " ", container = lytg1)
  
  lytg1[9,1]   <- glabel("Select Active Project for Analysis:", container = lytg1)
  lytg1[9,2]   <- glabel( text = "", container = lytg1)
  lytg1[9,3]   <- datasetSelector(session$analysis) <- gdroplist( 
                    datasetCatalog(session$analysis), 
                    selected = 0,  
                    container = lytg1, 
                    expand = TRUE, 
                    handler = function(h,...){
                      datasetId <- svalue( h$obj )
                      print(paste("dataSelection.gdroplist.handler('", datasetId,"')", sep=""))
                      if( length(datasetId) > 0 ) {
                        if( nchar(datasetId) > 0 ) {
                          if( !( datasetId == EMPTY_CATALOG() ) ) {
                              currentDataSet(session$analysis) <- datasetId
                          }
                        }
                      }
                    }
                  )
  
	
  ##########################
  ##Analysis Parameters    #
  ##########################
  lyt2         <- glayout( homogeneous = FALSE,  container = nb, spacing = 5, label = "Analysis Parameters", expand = TRUE)
	lyt2[1,1:6]  <- g2 <- gframe("Analysis Parameters and Data Filtering",container = lyt2, expand = TRUE, horizontal=FALSE) 
	lytg2        <- glayout( homogeneous = FALSE,  container = g2, spacing = 10, expand = TRUE) 
  
	lytg2[1,1:2] <- glabel("Number of Continuous Variables (CV): ",  container = lytg2)
	lytg2[1,3]   <- numberOfContinuousVariables(session$analysis)  <- gedit( "", container = lytg2, width = 10, initial.msg="" ) 
  lytg2[1,4]   <- gbutton(
                    "Run CV Analysis",
                    container = lytg2, 
                    handler=function(h,...){ print( descriptors.continuous() )
                  }
                )
  
	lytg2[2,1:2] <- glabel("Number of Nominal Variables (NV): ", container = lytg2)
  lytg2[2,3]   <- numberOfCategoricalVariables(session$analysis) <- gedit( "", container = lytg2, width = 10, initial.msg="" )
  lytg2[2,4]   <- gbutton(
                    "Run NV Analysis",
                    container = lytg2, 
                    handler=function(h,...){ print( descriptors.nominal() )
                  }
                )
	
  lytg2[3,1:2] <- glabel("Specify Coefficient of Correlation (CC):", container=lytg2)
  lytg2[3,3]   <- ncor <- gspinbutton( from = 0, to = 1, by = 0.1, value = 0, container = lytg2)
  lytg2[3,4]   <- gbutton(
                  "Run CC Analysis",
                  container = lytg2,
                  handler=function(h,...){ print( correlation() ) }
                )
  
  lytg2[4,1:2]  <- glabel( text = "Number of Accessions in Final Dataset:", container = lytg2)
  lytg2[4,3]    <- numberOfAccessions(session$analysis) <- gedit("10",  container = lytg2, width = 10, initial.msg =" ")
  lytg2[4,4]    <- gbutton(
                      "Set",
                      container = lytg2, 
                      expand=FALSE,
    			            handler = function(h,...){ print(number.access()) }
                    )
  
  lytg2[5,1:2] <- glabel("Specify Target Number of Solutions: ",  container = lytg2)
  lytg2[5,3]   <- numberOfSolutions(session$analysis) <- gedit("10000", width=7,  container = lytg2)
  lytg2[5,4]   <- gbutton( 
                    "Set",  
                    container = lytg2, 
                    expand=FALSE, 
                    handler = function(h,...){ 
                      print( number.solutions() )
                    }
                  )
  
  lytg2[6,1:2] <- glabel("Enter Target Percentage of Solutions (%):",  container = lytg2)
  lytg2[6,3]   <- percentageOfSolutions(session$analysis) <- gedit("1", width=3, container = lytg2)
  lytg2[6,4]   <- gbutton(
                      "Set",
                      container = lytg2,
                      expand = FALSE,
                      handler = function(h,...){ print( number.percent() )} 
                 )
  
  lytg2[7,1:2] <- glabel("Enter the number of final solutions\nfor the Maximum Variation or the\nNumber of Principal Components:",  container = lytg2)
  lytg2[7,3]   <- numberOfFinalSolutions(session$analysis) <- gedit("10", width=7, container = lytg2)
  lytg2[7,4]   <- gbutton(
                    "Set",
                    container = lytg2,
                    expand = FALSE,
                    handler = function(h,...){ print( number.final() ) }
                  )
  
  #############################
  ##Filter Input Trait Values #
  #############################
  inputTraitsFiltered <- FALSE
  optimizationTargetsSpecified <- FALSE
  
  traitFilterPageHandler <- function( win, notebook ) {
    
      return(
        
        function(h,...) {  # returns a gWidget handler closure for "Filter Traits..."
      
              ncon <- as.numeric( svalue( numberOfContinuousVariables( session$analysis )) )
              
              if(is.na(ncon) || !is.numeric(ncon) || !(ncon>0)) {
                DialogBox(
                  paste( 
                        "You need to tell Explora how many\n",
                        "Continuous Variables (CV) you have\n",
                        "before Explora can filter them!"
                  )
                )
          
              } else {
                
                  # I only create new trait filter page if not yet visible
                  if( !inputTraitsFiltered ) {

                      # I also only create new trait filter page if optimization page is NOT already displayed
                      if( !optimizationTargetsSpecified ) {
                    
                          DialogSelectThresholds(  win, notebook )
                          inputTraitsFiltered <<- TRUE
                          
                      } else {
                        DialogBox("Cannot filter out input data once you've started optimization analyses!")
                      }
                      svalue(notebook) <- 4
                  }
              }
          }
      )
  }

  lytg2[8,1]   <- gbutton(
    "Filter Traits Inputs ... (Default: Use all data)",
    container = lytg2,
    handler   =  traitFilterPageHandler( win, nb )
  )
  
  ###########################################
  ##Specify Optimization Analysis Variables #
  ###########################################
  optimizationTargetsPageHandler <- function( win, notebook, analysisPageHandler ) {
    
    return ( 
      
      function(h,...) { # returns a gWidget handler closure for "Specify Optimization Target Variables..."
                         
           ncon <- as.numeric( svalue( numberOfContinuousVariables( session$analysis )) )
           
           if(is.na(ncon) || !is.numeric(ncon) || !(ncon>0)) {
             DialogBox(                      
               paste( 
                 "You need to tell Explora how many\n",
                   "Continuous Variables (CV) you have\n",
                   "before Explora can set optimization targets!"
               )
             )
             
           } else {
             
               if( !optimizationTargetsSpecified ) {
                 
                   # modified version of original function
                   DialogSelectOptimization( win, notebook, analysisPageHandler )
                   
                   optimizationTargetsSpecified <<- TRUE
                   
                   if( inputTraitsFiltered ) {
                     svalue(notebook) <- 5
                   } else {
                     svalue(notebook) <- 4
                   }
               }
          }
      }
    )
  }
  
  ###########################################
  ##Specify Optimization Analysis Variables #
  ###########################################
  optimizationAnalysisPageHandler <- function( win, notebook ) {
    
      return ( 
        
          function() {  # returns a gWidget handler closure for "Specify Target Variables for Optimization ..."
                             
              if( optimizationTargetsSpecified ) {
                                 
                  DialogOptimizationAnalysis( win, notebook )
                  
                  if( inputTraitsFiltered ) {
                    svalue(notebook) <- 6
                  } else {
                    svalue(notebook) <-5
                  }
              }
          }
      )
  }
 
  lytg2[8,3]   <- gbutton(
    "Specify Optimization Variables ...",
    container = lytg2,
    handler =  optimizationTargetsPageHandler( win, nb , optimizationAnalysisPageHandler( win, nb ) )
  )
  
  svalue(nb) <- 1
  
	visible(win) <- TRUE

}
