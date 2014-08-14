#-----------------------------------------------------------------------------------------------#
# APPLICATION BIOVERSITY                                                                        #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM #
# VERSION 2.0 - AUGUST-04-2014                                                                  #
#                                                                                               #
# gui.r - Explora Graphical User Interface (based on RGtk2)                                     #
#-----------------------------------------------------------------------------------------------#
#' @include configuration.r dialogs.r loader.r algorithms.r

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
#' @import gWidgetsRGtk2

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
  options("guiToolkit"="RGtk2")
  
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
  environment(DialogSelectThresholds)  <- session
  environment(number.solution)         <- session
  environment(number.final)            <- session
  environment(number.percent)          <- session
  environment(descriptives.continuous) <- session
  environment(descriptives.nominal)    <- session
  environment(correlation)             <- session
  environment(f.optimization)          <- session
  environment(MAXVAR.type.opt)         <- session
  environment(PCA.type.opt)            <- session
  environment(WSM.type.opt)            <- session
  environment(DTree.type.opt)          <- session 
  
	## Principal window
	win <- gwindow("Explora Germplasm Selection Tool", visible = F , width = 500, height = 300) 
	nb  <- gnotebook( container = win, expand = T, tab.pos = 2)
	
	##Load dataset
	lyt1 = glayout(homogeneous = F,  container = nb, spacing = 1, label = "Load dataset", expand = T) 
	lyt1[1,1:3] = (g = gframe("Load dataset",  container = lyt1, horizontal = T))
	lytgb1 = glayout(homogeneous = F, container = g, spacing = 1, expand = T) 
	lytgb1[1,1] = (glabel = (""))
	lytgb1[2,1] = (h = gbutton(
                        "Set Output Directory",  
                        container = lytgb1, 
                        handler = function(h,...) { 
                                    newdir <- gfile(text = "Select directory", type = "selectdir") 
                                    if(! is.na(newdir) )  { 
                                      setwd(newdir)
                                      datasetCatalog(session$analysis) <- getProjects() 
                                    } 
                                  } 
                      )
                )
	lytgb1[3,1] = (glabel = (""))
	lytgb1[4,1] = gbutton(
                  "Load Data Set",  
                  container = lytgb1, 
                  expand = F, 
                  handler = function(h,...){ 
                                dataset <- load_dataset()
                                addDataset(session$analysis) <- attr(dataset,"identifier")
                                currentDataSet(session$analysis) <- dataset
                                dataset_selection(session$analysis)[1] <- datasetCatalog(session$analysis)
                            }
                )
	
	##Descriptive analysis
	lyt2 = glayout(homogeneous = F,  container = nb, spacing = 1, label = "Descriptive analysis", expand = T)
	lyt2[1,1:6] = (g1 <- gframe("Descriptive analysis",container = lyt2,expand=T,horizontal=F))
	lytg2 = glayout(homogeneous = F,  container = g1, spacing = 1, expand = T) 
  
  lytgb1[1,1] = (glabel = (""))
  lytg2[2,1] = glabel("Dataset for analysis  ",container = lytg2)
  
  ####################
  # TODO - FIX THIS! selection of the data_set doesn't really work; use of the "attach(eval(parse(text=svalue(h$obj)))" in the handler also seems a bit strange
  ####################
  lytg2[2,2] = ( 
      dataset_selection(session$analysis) <- gdroplist( 
            c(), 
            selected = 0,  
            container = lytg2, 
            expand = T, 
            handler = function(h,...){ attach(eval(parse(text=svalue(h$obj)))) }
      )
  )
  
  lytgb1[3,1] = (glabel = (""))
	lytg2[4,1] = glabel("Number of continuous variables: ",  container = lytg2)
	lytg2[4,2] = ( numContVar(session$analysis) <-gedit("",container = lyt2,width = 10,initial.msg="") ) 
	lytg2[5,1] = glabel("Number of categorical variables: ",container = lytg2)
	lytg2[5,2] = (ncat <-gedit("",container=lyt2,width = 10,initial.msg=""))
	
	lytg2[6,1]=(glabel=(""))
	lytg2[7,1]=(glabel=(""))
	
	lytg2[9,1] = gbutton("Descriptive analysis for continuous variables",container=lytg2, handler=function(h,...){print(descriptives.continuous(eval(parse(text=svalue( dataset_selection(session$analysis) )))))})
	lytg2[10,1] = gbutton("Descriptive analysis for nominal variables",container=lytg2, handler=function(h,...){print(descriptives.nominal(eval(parse(text=svalue( dataset_selection(session$analysis) )))))})
	
	lytg2[11,1]=(glabel=(""))
	lytg2[12,1]=(glabel=(""))
	
	lytg2[13,1] = glabel("Level of correlation: ",container=lytg2)
	lytg2[13,2] = (ncor <- gspinbutton(from=0, to = 1, by = 0.1, value=0, container=lytg2)) 
	lytg2[14,1] = gbutton("Correlation analysis",container=lytg2, handler=function(h,...){print(correlation(eval(parse(text=svalue( dataset_selection(session$analysis) )))))})
	
	lytg2[15,1]=(glabel=(""))
	lytg2[16,1]=(glabel=(""))
	
	lytg2[17,1] = glabel("Number accessions in final dataset: ",  container = lytg2)
	lytg2[17,2] = (num.access <- gedit("10",  container = lyt2, width = 10, initial.msg =" "))
	lytg2[18,1] = gbutton("Select number accessions",  container = lyt2, expand=F,
			handler = function(h,...){print(number.access())})
	
	lytg2[19,1]=(glabel=(""))
	lytg2[20,1]=(glabel=(""))
	
	lytg2[21,1] = glabel("Threshold analysis: ",  container = lytg2)
	lytg2[22,1] = gbutton("Select variables",  container = lytg2, handler = function(h,...){DialogSelectThresholds(eval(parse(text=svalue( dataset_selection(session$analysis) ))))})
	
	##Selection of preferred analysis
	lyt5 = glayout(homogeneous = F, container = nb , spacing=1,label="Selection of preferred analysis",expand=T)
	lyt5[1,1:10] = (g5 = gframe("Type selection of preferred", container = lyt5, expand = T, horizontal = F))
	lytg5 = glayout(homogeneous = F,  container = g5, spacing = 1, expand = T) 
	
	lytg5[1,1] = glabel("Enter the number of solutions: ",  container = lytg5)
	lytg5[1,2] = ( numberSoln(session$analysis) <- gedit("10000",  container = lytg5))
	lytg5[2,1] = gbutton("Select the number of solutions",  container = lytg5, expand=F,
			handler = function(h,...){print(number.solution())})
	
	lytg5[3,1] = (glabel=(""))
	lytg5[4,1] = (glabel=(""))
	
	lytg5[5,1] = glabel("Optimization analysis: ",  container = lytg5)
	lytg5[6,1] = gbutton("Select variables",  container = lytg5, expand=F,
			handler = function(h,...){DialogSelectOptimization(eval(parse(text=svalue( dataset_selection(session$analysis) ))))})
	
	lytg5[7,1] = (glabel=(""))
	lytg5[8,1] = (glabel=(""))
	
	lytg5[9,1] = glabel("Enter the percentage of solutions (%):",  container = lytg5)
	lytg5[9,2] = ( percentSoln(session$analysis) <- gedit("1",  container = lytg5))
	lytg5[10,1] = gbutton("Select the percentage of solutions",  container = lytg5, expand=F,
			handler = function(h,...){print(number.percent())})
	
	lytg5[11,1] = (glabel=(""))
	lytg5[12,1] = (glabel=(""))
	
	lytg5[13,1] = glabel("Enter the number of final solutions for \n (Maximum Variation or number of Principal Components) :",  container = lytg5)
	lytg5[13,2] = (nfinal = gedit("10",  container = lytg5))
	lytg5[14,1] = gbutton("Select the number of final solutions",  container = lytg5, expand=F,
			handler = function(h,...){print(number.final())})
	
	lytg5[15,1] = (glabel=(""))
	lytg5[16,1] = (glabel=(""))
	
	lytg5[17,1] = glabel("Select the type of selection of preferred: ",  container = lytg5, horizontal = F)
	
	items.option <- c(" ", "Maximum variation", "Principal components",
			"Weighted sum model", "Decision tree")
	
	lytg5[18,1] = (option.preferred <- gdroplist(items.option,  container = lytg5))
	lytg5[18,2] = (btn <- gbutton("Run",  container = lytg5))
	
	addHandlerChanged(btn, handler = function(h,...){
				if(svalue(option.preferred) == "Maximum variation"){MAXVAR.type.opt( optimizationResult(session$analysis) )}
				if(svalue(option.preferred) == "Principal components"){PCA.type.opt( optimizationResult(session$analysis) )}
				if(svalue(option.preferred) == "Weighted sum model"){WSM.type.opt( optimizationResult(session$analysis) )}
				if(svalue(option.preferred) == "Decision tree"){DTree.type.opt( optimizationResult(session$analysis) )}
			})

	## Principal windows
	welcome = ggroup(container = nb,horizontal = F,label="About the Application")
	
  image_dir <- paste(path.package("explora"),"/images/",sep="")
  gimage("Explora_Logo.png", dirname = image_dir, size = "button",  container = welcome) 
	
	visible(win) <- TRUE

}
