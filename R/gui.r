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
  
  ## Change locale for message in english
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
  environment(readProjectFile)          <- session
  environment(result.path.exists)       <- session
  environment(result.path)              <- session
  
  environment(getDataThresholds)        <- session
  
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
	nb  <- gnotebook( container = win, expand = T, tab.pos = 3)
  
  ## Welcome Window
  welcome <- ggroup(container = nb, horizontal = FALSE, label="Welcome")
  
  image_dir <- paste(path.package("explora"),"/images/",sep="")
  gimage("Explora_Logo.gif", dirname = image_dir, container = welcome) 
  
	## Load project datasets
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
                  expand = F, 
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
  
	
	##Descriptor analysis
	lyt2         <- glayout( homogeneous = FALSE,  container = nb, spacing = 5, label = "Descriptor Analysis", expand = TRUE)
	lyt2[1,1:6]  <- g2 <- gframe("Trait Descriptor Analysis & Filtering",container = lyt2, expand = TRUE, horizontal=FALSE) 
	lytg2        <- glayout( homogeneous = FALSE,  container = g2, spacing = 10, expand = TRUE) 
  
  lytg2[1,1:5] <- glabel( text = "",  container = lytg2)
  
	lytg2[2,1:2] <- glabel("Number of Continuous Variables (CV): ",  container = lytg2)
	lytg2[2,3:4] <- numberOfContinuousVariables(session$analysis)  <- gedit( "", container = lytg2, width = 10, initial.msg="" ) 
  lytg2[2,5]   <- gbutton(
                    "Run CV Analysis",
                    container = lytg2, 
                    handler=function(h,...){ print( descriptors.continuous() )
                  }
                )
  
  lytg2[3,1:5] <- glabel( text = "", container=lytg2)
  
	lytg2[4,1:2] <- glabel("Number of Nominal Variables (NV): ", container = lytg2)
  lytg2[4,3:4] <- numberOfCategoricalVariables(session$analysis) <- gedit( "", container = lytg2, width = 10, initial.msg="" )
  lytg2[4,5]   <- gbutton(
                    "Run NV Analysis",
                    container = lytg2, 
                    handler=function(h,...){ print( descriptors.nominal() )
                  }
                )
	
	lytg2[5,1:5] <- glabel( text = "", container=lytg2 )
	
  lytg2[6,1:2] <- glabel("Specify Coefficient of Correlation (CC):", container=lytg2)
  lytg2[6,3]   <- ncor <- gspinbutton( from = 0, to = 1, by = 0.1, value = 0, container = lytg2)
  lytg2[6,4]   <- glabel( text = "", container=lytg2)
  lytg2[6,5]   <- gbutton(
                  "Run CC Analysis",
                  container = lytg2,
                  handler=function(h,...){ print( correlation() ) }
                )
  
	lytg2[7,1:5]  <- glabel( text = "", container = lytg2 )
	
  lytg2[8,1:2]  <- glabel( text = "Number of Accessions in Final Dataset:", container = lytg2)
  lytg2[8,3]    <- numberOfAccessions(session$analysis) <- gedit("10",  container = lytg2, width = 10, initial.msg =" ")
  lytg2[8,4]    <- glabel( text = "", container=lytg2)
  lytg2[8,5]    <- gbutton(
                      "Set",
                      container = lytg2, 
                      expand=FALSE,
    			            handler = function(h,...){ print(number.access()) }
                    )
  
	lytg2[9,1:5]  <- glabel( text = "", container=lytg2)
	
  lytg2[10,1:2] <- glabel( text = "Filter input trait variables with thresholds:", container = lytg2)
  lytg2[10,3:5] <- gbutton(
                      "Select...",
                      container = lytg2,
                      handler = function(h,...){ DialogSelectThresholds( currentDataSet(session$analysis) ) }
                    )
  
  
	## Optimization Analysis
	lyt3         <- glayout(homogeneous = F, container = nb , spacing=10,label="Optimization",expand=T)
	lyt3[1,1:10] <- g3 <- gframe("Optimization Analysis", container = lyt3, expand = TRUE, horizontal = FALSE)
	lytg3        <- glayout(homogeneous = FALSE,  container = g3, spacing = 10, expand = TRUE) 
	
  lytg3[1,1:3] <- glabel( text = "",  container = lytg2)
  
  lytg3[2,1] <- glabel("Specify Target Number of Solutions: ",  container = lytg3)
  lytg3[2,2] <- numberOfSolutions(session$analysis) <- gedit("10000", width=7,  container = lytg3)
  lytg3[2,3] <- gbutton( 
                      "Set",  
                      container = lytg3, 
                      expand=FALSE, 
                      handler = function(h,...){ 
                        print( number.solutions() )
                      }
                    )
	
	lytg3[3,1:3]  <- glabel( text = "", container = lytg3)
	
	lytg3[4,1] <- glabel("Specify Optimization Analysis Variables:",  container = lytg3)
  lytg3[4,3] <- gbutton(
                      "Select...",
                      container = lytg3,
                      expand = FALSE,
                      handler = function(h,...){ DialogSelectOptimization() }
                   )
	
	lytg3[5,1:3] <- glabel( text = "", container = lytg3)
	
	lytg3[6,1] <- glabel("Enter Target Percentage of Solutions (%):",  container = lytg3)
  lytg3[6,2]   <- percentageOfSolutions(session$analysis) <- gedit("1", width=3, container = lytg3)
	lytg3[6,3]   <- gbutton(
                    "Set",
                    container = lytg3,
                    expand = FALSE,
  		            	handler = function(h,...){ print( number.percent() )} 
                  )
	
	lytg3[7,1:3] <- glabel( text = "", container = lytg3)
	
	lytg3[8,1] <- glabel("Enter the number of final solutions\nfor the Maximum Variation or the\nNumber of Principal Components:",  container = lytg3)
	lytg3[8,2]   <-numberOfFinalSolutions(session$analysis) <- gedit("10", width=7, container = lytg3)
	lytg3[8,3]   <- gbutton(
                    "Set",
                    container = lytg3,
                    expand = FALSE,
  			            handler = function(h,...){ print( number.final() ) }
                  )
	
	lytg3[9,1:3] <- glabel( text = "", container = lytg3)
	
	lytg3[10,1]  <- glabel("Select Preferred Optimization Algorithm: ",  container = lytg3, horizontal = FALSE)
	
	items.option <- c(
                      " ", 
                      "Maximum variation", 
                      "Principal components",
  			              "Weighted sum model",
                      "Decision tree"
                    )
	
	lytg3[11,1] <- option.preferred <- gdroplist(items.option,  container = lytg3)
	lytg3[11,2] <- btn <- gbutton("Run",  container = lytg3)
	
	addHandlerChanged(btn, handler <- function(h,...){
				if(svalue(option.preferred) == "Maximum variation")   { MAXVAR.type.opt()}
				if(svalue(option.preferred) == "Principal components"){ PCA.type.opt()}
				if(svalue(option.preferred) == "Weighted sum model")  { WSM.type.opt()}
				if(svalue(option.preferred) == "Decision tree")       { DTree.type.opt()}
			})

  svalue(nb) <- 1
  
	visible(win) <- TRUE

}
