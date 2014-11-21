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
#' @importFrom gWidgets gimage
#' @importFrom gWidgets gtable
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
  session$context <- new("ExploraAnalysis")
  
  datasetCatalog(session$context) <- getProjects() 
  
	## Principal window
  mainWindow(session$context) <- win <- gwindow("Explora Germplasm Selection Tool", visible = FALSE , width = 800, height = 600)
  
  datasetCatalog(session$context) <- getProjects() 
  
	notebook <- gnotebook( container = win, expand = TRUE, tab.pos = 3)
  
  ## Welcome Window
  welcome <- ggroup(label="Welcome", container = notebook, horizontal = FALSE )
  
  # image_dir <- paste(path.package("explora"),"/images/",sep="")
  # gimage("Explora_Logo.gif", dirname = image_dir, container = welcome) 
  gimage(system.file("images/Explora_Logo.gif", package="explora"), container=welcome)
  
  ##########################
	## Load project datasets #
  ##########################
  lyt1 <- glayout(
                  homogeneous = TRUE,  
                  container = notebook, 
                  spacing = 5, 
                  label = "STEP 1. Project Selection",
                  expand = TRUE, fill="both"
         )
  
  lyt1[ 1, 1, expand = TRUE, fill = "x" ] <- projectView <- gframe(text="Project Selection", container = lyt1 )
  projectGroup <- glayout( spacing = 5, container = projectView )
  
  projectGroup[1,1] <- gbutton(
          "Set Project Folder...",  
          container = projectGroup, 
          handler = function(h,...) { 
                      newdir <- gfile(text = "Select directory", type = "selectdir") 
                      if(! is.na(newdir) )  { 
                        setwd(newdir)
                        datasetCatalog(session$context)    <- getProjects()
                        datasetSelector(session$context)[] <- datasetCatalog(session$context)
                      } 
                    } 
        )

  projectGroup[1,2] <- gbutton(
      "Load Projects...",  
      container = projectGroup, 
      expand = FALSE, 
      handler = function(h,...){ 
                    dataset <- createProject()
                    dsl <- length(dataset)
                    if( dsl > 0 && !(dsl == 1 && is.na(dataset)) ) {
                      addDataset(session$context)        <- attr(dataset,"identifier")
                      currentDataSet(session$context)    <- dataset
                      datasetSelector(session$context)[] <- datasetCatalog(session$context)
                    }
                }
    )
  
  projectGroup[2,1] <- glabel("Select for Analysis:", container = projectGroup)
  
  ## these flags control the analysis parameter pages
  
  projectLoaded <- FALSE
  analysisParameterWindow <- FALSE
  traitFilterWindow <- FALSE
  accessionsSelectionWindow <- FALSE
  
  #############################
  ##Filter Input Trait Values #
  #############################
  traitFilterPageHandler <- function( context, notebook ) {
    
    return(
      
      function(h,...) {  # returns a gWidget handler closure for "Filter Traits..."
        
        ncon <- as.numeric( svalue( numberOfContinuousVariables( context )) )
        
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
          if( !traitFilterWindow ) {
            
            # I also only create new trait filter page if optimization page is NOT already displayed
            if( !accessionsSelectionWindow ) {
              
              DialogSelectThresholds( context, notebook )
              traitFilterWindow <<- TRUE
              
            } else {
              DialogBox("Cannot filter out input data once you've started optimization analyses!")
            }
            svalue(notebook) <- 4
          }
        }
      }
    )
  }
  
  ###########################################
  ##Specify Optimization Analysis Variables #
  ###########################################
  accessionSelectionHandler <- function( context, notebook ) {
    
    return ( 
      
      function(h,...) { # returns a gWidget handler closure for "Specify Optimization Target Variables..."
        
        ncon <- as.numeric( svalue( numberOfContinuousVariables( context )) )
        
        if(is.na(ncon) || !is.numeric(ncon) || !(ncon>0)) {
          DialogBox(                      
            paste( 
              "You need to tell Explora how many\n",
              "Continuous Variables (CV) you have\n",
              "before Explora can set optimization targets!"
            )
          )
          
        } else {
          
          if( !accessionsSelectionWindow ) {
            
            # modified version of original function
            DialogSelectOptimization( context, notebook )
            
            accessionsSelectionWindow <<- TRUE
            
            if( traitFilterWindow ) {
              svalue(notebook) <- 5
            } else {
              svalue(notebook) <- 4
            }
          }
        }
      }
    )
  }
  
  ########################
  ## Analysis Parameters #
  ########################
  setAnalysisParameters <- function( context, notebook ) {
    
    lyt2         <- glayout( homogeneous = FALSE,  container = notebook, spacing = 5, label = "STEP 2. Analysis Parameters", expand = TRUE)
    lyt2[1,1:6]  <- g2 <- gframe("Analysis Parameters and Data Filtering",container = lyt2, expand = TRUE, horizontal=FALSE) 
    lytg2        <- glayout( homogeneous = FALSE,  container = g2, spacing = 10, expand = TRUE)
    
    lytg2[1,1]   <- glabel("Data Analysis Tag: ", container = lytg2)
    lytg2[1,3]   <- dataAnalysisTag( context ) <- gedit( 
                      format(Sys.time(), "%H-%M-%S_%d-%m-%Y"), 
                      container = lytg2, 
                      width = 20, 
                      initial.msg=""
                    )
    
    lytg2[2,1:2] <- glabel("Number of Continuous Variables (CV): ",  container = lytg2)
    lytg2[2,3]   <- numberOfContinuousVariables( context ) <- gedit( "", container = lytg2, width = 10, initial.msg="" ) 
    lytg2[2,4]   <- gbutton(
                      "Run CV Analysis",
                      container = lytg2, 
                      handler=function(h,...){ 
                        print( descriptors.continuous( context ) )
                      }
                    )
    
    lytg2[3,1:2] <- glabel("Number of Nominal Variables (NV): ", container = lytg2)
    lytg2[3,3]   <- numberOfCategoricalVariables(context) <- gedit( "", container = lytg2, width = 10, initial.msg="" )
    lytg2[3,4]   <- gbutton(
                      "Run NV Analysis",
                      container = lytg2, 
                      handler=function(h,...){ 
                        print( descriptors.nominal( context ) )
                      }
                    )
    
    lytg2[4,1:2] <- glabel("Specify Coefficient of Correlation (CC):", container=lytg2)
    lytg2[4,3]   <- coefficientOfCorrelation( context ) <- gspinbutton( from = 0, to = 1, by = 0.1, value = 0, container = lytg2)
    lytg2[4,4]   <- gbutton(
                      "Run CC Analysis",
                      container = lytg2,
                      handler=function(h,...){ 
                        print( correlationAnalysis( context ) ) 
                      }
                    )
    
    lytg2[5,1:2]  <- glabel( text = "Number of Accessions in Final Dataset:", container = lytg2)
    lytg2[5,3]    <- targetNumberOfAccessions( context ) <- gedit("10",  container = lytg2, width = 10, initial.msg =" ")
    #lytg2[5,4]    <- gbutton(
    #                    "Set",
    #                    container = lytg2, 
    #                    expand=FALSE,
    #  			            handler = function(h,...){ print( number.access() ) }
    #                  )
    
    lytg2[6,1:2] <- glabel("Specify Target Number of Solutions: ",  container = lytg2)
    lytg2[6,3]   <- numberOfSolutions( context ) <- gedit("10000", width=7,  container = lytg2)
    #lytg2[6,4]   <- gbutton( 
    #                  "Set",  
    #                  container = lytg2, 
    #                  expand=FALSE, 
    #                  handler = function(h,...){ 
    #                    print( number.solutions(context) )
    #                  }
    #                )
    
    lytg2[6,4]   <- gbutton(
                      "Check Parameters",
                      container = lytg2,
                      expand = FALSE,
                      handler = function(h,...){ 
                        check.parameters( context ) 
                      }
                    )
    
    lytg2[7,1:2] <- glabel("Enter Target Percentage of Solutions (%):",  container = lytg2)
    lytg2[7,3]   <- percentageOfSolutions( context ) <- gedit("1", width=3, container = lytg2)
    #lytg2[7,4]   <- gbutton(
    #                    "Set",
    #                    container = lytg2,
    #                    expand = FALSE,
    #                    handler = function(h,...){ print( number.percent(context) )} 
    #               )
    
    lytg2[7,4]   <- gbutton(
      "Impute missing data\n(Experimental)",
      container = lytg2,
      expand = FALSE,
      handler = function(h,...){ 
        impute.missing.data( context ) 
      }
    )
    
    lytg2[8,1:2] <- glabel("Enter the number of final solutions:",  container = lytg2)
    lytg2[8,3]   <- numberOfFinalSolutions( context ) <- gedit("10", width=7, container = lytg2)
    #lytg2[8,4]   <- gbutton(
    #                  "Set",
    #                  container = lytg2,
    #                  expand = FALSE,
    #                  handler = function(h,...){ print( number.final(context) )}
    #                )
    
    lytg2[9,1]   <- gbutton(
      "STEP 3. Traits Filtering",
      container = lytg2,
      handler   =  traitFilterPageHandler( context, notebook )
    )
    
    lytg2[9,3]   <- gbutton(
      "STEP 4. Accession Selection",
      container = lytg2,
      handler =  accessionSelectionHandler( context, notebook  )
    )
    
    analysisParameterWindow <<- TRUE
    
  }
  
  resetAnalysis <- function( context, notebook ) {

    # Reset the computational environment here
    sampleDistribution( context ) <- list()
    
    if(traitFilterWindow) {
      
      traitFilterWindow <<- FALSE
      
      if(accessionsSelectionWindow) {
        
        accessionsSelectionWindow <<- FALSE
        svalue(notebook) <- 5
        dispose(notebook)
        
      }
      
      svalue(notebook) <- 4
      dispose(notebook)
      
    } else {
      
      if(accessionsSelectionWindow) {
        
        accessionsSelectionWindow <<- FALSE
        svalue(notebook) <- 4
        dispose(notebook)
        
      }
      
    }
    
    if(analysisParameterWindow) {
      svalue(notebook) <- 3
      dispose(notebook)
      analysisParameterWindow <<- FALSE
    }
    
  }
  
  dataPreview <- function( context ) {

    dataWindow  <- gwindow( 
                      paste("Explora DataSet: ",currentProjectName( context )),
                      parent = mainWindow(context) ,
                      visible = FALSE, 
                      width = 800, 
                      height = 600 
                  )
    
    dataGroup <- ggroup(horizontal=FALSE, container = dataWindow )
    
    dataTable <- gtable( 
                      currentDataSet( context ), 
                      container = dataGroup 
                    )
    
    visible(dataWindow)<-TRUE  
    
  }
  
  projectGroup[2,2] <- datasetSelector(session$context) <- gdroplist( 
                          datasetCatalog( session$context ), 
                          selected = 0,  
                          container = projectGroup, 
                          expand = TRUE, 
                          handler = function(h,...){
                            datasetId <- svalue( h$obj )
                            print(paste("dataSelection.gdroplist.handler('", datasetId,"')", sep=""))
                            if( length(datasetId) > 0 ) {
                              if( nchar(datasetId) > 0 ) {
                                if( !( datasetId == EMPTY_CATALOG() ) ) {
                                    currentDataSet( session$context ) <- datasetId
                                    resetAnalysis(  session$context, notebook )
                                }
                              }
                            }
                          }
                        )

  projectGroup[3,1] <- gbutton(
    "Preview Project Dataset",
    container = projectGroup,
    handler   =  function(h,...) {
      dataPreview( session$context )
    } 
  )
  
  projectGroup[4,1] <- glabel("", container = projectGroup)
  
  projectGroup[5,1] <- gbutton(
    "STEP 2. Set Analysis Parameters",
    container = projectGroup,
    handler   =  function(h,...) {
      setAnalysisParameters( session$context, notebook)
      svalue(notebook) <- 3
    } 
  )
  
  svalue(notebook) <- 1
  
  visible(win) <- TRUE
  
}
