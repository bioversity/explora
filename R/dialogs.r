#----------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                       #
#AUTHOR: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM #
#VERSION 2.0 - AUGUST-04-2014                                                                 #
#                                                                                             #
# dialogs.r - Smaller GUI Dialog Boxes used by Explora                                        #
#---------------------------------------------------------------------------------------------- 
#' @include projects.r

#' @importFrom gWidgets gwindow
#' @importFrom gWidgets gnotebook
#' @importFrom gWidgets glayout
#' @importFrom gWidgets glabel
#' @importFrom gWidgets gradio
#' @importFrom gWidgets ginput
#' @importFrom gWidgets gedit
#' @importFrom gWidgets gdroplist
#' @importFrom gWidgets gbutton
#' @importFrom gWidgets ggroup
#' @importFrom gWidgets gbasicdialog
#' @importFrom gWidgets gimage

#' @importMethodsFrom gWidgets svalue
#' @importMethodsFrom gWidgets svalue<-
#' @importMethodsFrom gWidgets addSpring
#' @importMethodsFrom gWidgets dispose
#' @importMethodsFrom gWidgets visible
#' @importMethodsFrom gWidgets visible<-

#' @import gWidgetstcltk

DialogBoxDTree <- function(items,question="") {## This function make a dialog box
	
	out <- ""
	w <-  gbasicdialog("", handler = function(h,...){out <<- svalue(txt)})
	
	glabel(paste(question,"Do you want to continue?",sep=""),  container = w)
	glabel(" ",  container = w)
	txt <- gradio(items,  container = w)
	visible(w, set = TRUE)
	return(out)
	
}  

DialogSelectItems <- function( message, items ){
	
  out.category <- ""
  
	dlg <-  gbasicdialog( message )
	
	txt.category <- gdroplist(
                    items,  
                    selected = 0,
                    container = dlg,
                    expand = TRUE, 
                    handler = function(h,...){ 
                      out.category <<- svalue(h$obj) 
                    }
                  )
  
	visible(dlg, set = TRUE)
	
  return(out.category) 
	
}

DialogSelectCategory <- function( categories ) { ## Function to select preference category
  DialogSelectItems( "Select the category of preference", categories )
}

DialogSelectSolution <- function( solutions ){## Function to select solutions
  DialogSelectItems( "Select the solutions", solutions )
}
	
## Function to select variables for thresholds
DialogSelectThresholds <- function( win, notebook ){
  
  ncon  <- as.numeric( svalue( numberOfContinuousVariables( analysis )) )
  object.thresholds <- object.complete <- theDataSet <- currentDataSet(analysis)
	object.thresholds <- object.thresholds[,-1]
	object.thresholds <- object.thresholds[,1:ncon]

	min.values <- matrix(round(as.table(sapply( object.thresholds, min, na.rm=TRUE )),3), ncol = 1)
  names(min.values) <- names(object.thresholds)
  
	max.values <- matrix(round(as.table(sapply( object.thresholds, max, na.rm=TRUE )),3), ncol = 1)
  names(max.values) <- names(object.thresholds)
  
	names.thresholds <- paste(names(object.thresholds),":(","Min = ", min.values," ; ","Max = ",
			max.values, ")", sep = "")
	
	traitFilterPage <- glayout( homogeneous = FALSE,  container = notebook, spacing = 5, label = "Trait Filtering", expand = TRUE)
  
	# traitFilterNotebook <- gnotebook( container = traitFilterPage, expand = TRUE, tab.pos = 3)
	
  ## TODO: to fix: this of variable thresholds seem to hard coded 
  # to a specific size. What happens if there are more variables in the dataset?
  
	tpl <- glayout(homogeneous = FALSE, container = traitFilterPage, expand=TRUE, spacing=5, label = "Trait Input Thresholds", expand = TRUE)
  
	tpl[1,1:10] <- glabel( text=" ", container = tpl )
	
	tpl[2,1:4]  <- glabel("Variables to select",  container = tpl)
	tpl[3,1:4]  <- var1  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[4,1:4]  <- var2  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[5,1:4]  <- var3  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[6,1:4]  <- var4  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[7,1:4]  <- var5  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[8,1:4]  <- var6  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[9,1:4]  <- var7  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[10,1:4] <- var8  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[11,1:4] <- var9  <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	tpl[12,1:4] <- var10 <- gdroplist(c("NA",  names.thresholds),  container = tpl)
	
	tpl[2,5:7] <- glabel("Minimum values", container = tpl)
	tpl[3,7]   <- min.var1  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[4,7]   <- min.var2  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[5,7]   <- min.var3  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[6,7]   <- min.var4  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[7,7]   <- min.var5  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[8,7]   <- min.var6  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[9,7]   <- min.var7  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[10,7]  <- min.var8  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[11,7]  <- min.var9  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	tpl[12,7]  <- min.var10 <- gedit(" ",  container = tpl, width = 10, initial.msg = "Min")
	
	tpl[2,8:10] <- glabel("Maximum values", container = tpl)
	tpl[3,10]   <- max.var1  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[4,10]   <- max.var2  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[5,10]   <- max.var3  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[6,10]   <- max.var4  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[7,10]   <- max.var5  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[8,10]   <- max.var6  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[9,10]   <- max.var7  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[10,10]  <- max.var8  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[11,10]  <- max.var9  <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
	tpl[12,10]  <- max.var10 <- gedit(" ",  container = tpl, width = 10, initial.msg = "Max")
  
	tpl[13,1:10] <- glabel( text=" ", container = tpl )
	
  # Exit button here! 
  # Local saveTraitThresholds() function and variables visible by closure?
  
  saveThresholds <- function(h,...) {

	    var1 <- svalue(var1); var2 <- svalue(var2); var3 <- svalue(var3); var4 <- svalue(var4); var5 <- svalue(var5)
	    var6 <- svalue(var6); var7 <- svalue(var7); var8 <- svalue(var8); var9 <- svalue(var9); var10 <- svalue(var10)
      
	    min.var1 <- as.numeric(svalue(min.var1)); min.var2  <- as.numeric(svalue(min.var2))
	    min.var3 <- as.numeric(svalue(min.var3)); min.var4  <- as.numeric(svalue(min.var4))
	    min.var5 <- as.numeric(svalue(min.var5)); min.var6  <- as.numeric(svalue(min.var6))
	    min.var7 <- as.numeric(svalue(min.var7)); min.var8  <- as.numeric(svalue(min.var8))
	    min.var9 <- as.numeric(svalue(min.var9)); min.var10 <- as.numeric(svalue(min.var10))
      
	    max.var1 <- as.numeric(svalue(max.var1)); max.var2  <- as.numeric(svalue(max.var2))
	    max.var3 <- as.numeric(svalue(max.var3)); max.var4  <- as.numeric(svalue(max.var4))
	    max.var5 <- as.numeric(svalue(max.var5)); max.var6  <- as.numeric(svalue(max.var6))
	    max.var7 <- as.numeric(svalue(max.var7)); max.var8  <- as.numeric(svalue(max.var8))
	    max.var9 <- as.numeric(svalue(max.var9)); max.var10 <- as.numeric(svalue(max.var10))
	    
      vars <- c(var1, var2, var3, var4, var5, var6, var7, var8, var9, var10)
      if(all(vars=="NA")) {
        svalue(notebook) <- 3
        return(list())
      }
      
      if(var1!="NA"){
        var1 <- unlist(strsplit(var1, ":"))[1]
        if(is.na(min.var1)) {
          min.var1 <- min.values[var1]
        }
        if(is.na(max.var1)) {
          max.var1 <- max.values[var1]
        }
      }
      if(var2!="NA"){
        var2 <-unlist(strsplit(var2, ":"))[1]
        if(is.na(min.var2)) {
          min.var2 <- min.values[var2]
        }
        if(is.na(max.var2)) {
          max.var2 <- max.values[var2]
        }
      }
      if(var3!="NA"){
        var3 <- unlist(strsplit(var3, ":"))[1]
        if(is.na(min.var3)) {
          min.var3 <- min.values[var3]
        }
        if(is.na(max.var3)) {
          max.var3 <- max.values[var3]
        }
        
      }
      if(var4!="NA"){
        var4 <-unlist(strsplit(var4, ":"))[1]
        if(is.na(min.var4)) {
          min.var4 <- min.values[var4]
        }
        if(is.na(max.var4)) {
          max.var4 <- max.values[var4]
        }
        
      }
      if(var5!="NA"){
        var5 <- unlist(strsplit(var5, ":"))[1]
        if(is.na(min.var5)) {
          min.var5 <- min.values[var5]
        }
        if(is.na(max.var5)) {
          max.var5 <- max.values[var5]
        }
        
      }
      if(var6!="NA"){
        var6 <-unlist(strsplit(var6, ":"))[1]
        if(is.na(min.var6)) {
          min.var6 <- min.values[var6]
        }
        if(is.na(max.var6)) {
          max.var6 <- max.values[var6]
        }
        
      }
      if(var7!="NA"){
        var7 <- unlist(strsplit(var7, ":"))[1]
        if(is.na(min.var7)) {
          min.var7 <- min.values[var7]
        }
        if(is.na(max.var7)) {
          max.var7 <- max.values[var7]
        }
        
      }
      if(var8!="NA"){
        var8 <-unlist(strsplit(var8, ":"))[1]
        if(is.na(min.var8)) {
          min.var8 <- min.values[var8]
        }
        if(is.na(max.var8)) {
          max.var8 <- max.values[var8]
        }
        
      }
      if(var9!="NA"){
        var9 <- unlist(strsplit(var9, ":"))[1]
        if(is.na(min.var9)) {
          min.var9 <- min.values[var9]
        }
        if(is.na(max.var9)) {
          max.var9 <- max.values[var9]
        }
        
      }
      if(var10!="NA"){
        var10 <-unlist(strsplit(var10, ":"))[1]
        if(is.na(min.var10)) {
          min.var10 <- min.values[var10]
        }
        if(is.na(max.var10)) {
          max.var10 <- max.values[var10]
        }
        
      }
      
      var.thresholds <- c(var1, var2, var3, var4, var5,
                          var6, var7, var8, var9, var10)
      
      var.thresholds <- var.thresholds[var.thresholds!="NA"]
      var.thresholds <- unique(var.thresholds)
      
      min.val <- c(min.var1,min.var2,min.var3,min.var4,min.var5,
                    min.var6,min.var7,min.var8,min.var9,min.var10)
      
      min.val <- min.val[!is.na(min.val)]
      
      max.val <- c(max.var1,max.var2,max.var3,max.var4,max.var5,
                    max.var6,max.var7,max.var8,max.var9,max.var10)
      
      max.val <- max.val[!is.na(max.val)]
      
      matrix.thresholds <- matrix(NA, nrow=length(var.thresholds), ncol=3) 
      matrix.thresholds <- cbind(var.thresholds, min.val, max.val)
      matrix.thresholds <- as.data.frame(matrix.thresholds, class = c("character","numeric","numeric"))
      colnames(matrix.thresholds) <- c("Variable", "Min", "Max")
      matrix.thresholds <- na.omit(matrix.thresholds)  
      
      val.min <- character(dim(matrix.thresholds)[1])
      val.max <- character(dim(matrix.thresholds)[1])
      
      for(i in 1:dim(matrix.thresholds)[1]){
        
        if(as.numeric(as.matrix(matrix.thresholds)[i,2]) < min(theDataSet[colnames(theDataSet) ==  as.character(matrix.thresholds[i,1])])){
          val.min[i] <-  as.character(matrix.thresholds[i,1])
        }
        if(as.numeric(as.matrix(matrix.thresholds)[i,3]) > max(theDataSet[colnames(theDataSet) ==  as.character(matrix.thresholds[i,1])])){
          val.max[i] <-  as.character(matrix.thresholds[i,1])
        }
      }
      
      val.min <- val.min[val.min!=""]
      val.max <- val.max[val.max!=""]
      
      if(length(val.min) == 0 & length(val.max) == 0){
        
        ## Extract subset of data base from thresholds
        data.var.thresholds <- as.data.frame(theDataSet[,is.element(colnames(theDataSet), matrix.thresholds$Variable)])
        rownames(data.var.thresholds) <- object.complete$accession
        accession_subset <- matrix(NA, nrow = dim(theDataSet)[1], ncol = length(var.thresholds))
        colnames(accession_subset) <- as.character(matrix.thresholds[,1])
        
        w<-1
        
        while(w <= dim(matrix.thresholds)[1]){  
          sub <- subset(data.var.thresholds,data.var.thresholds[,w] >= as.numeric(as.character(matrix.thresholds[w,2])) & data.var.thresholds[,w] <= as.numeric(as.character(matrix.thresholds[w,3])))
          data.var.thresholds <- subset(data.var.thresholds,data.var.thresholds[,w] >= as.numeric(as.character(matrix.thresholds[w,2])) & data.var.thresholds[,w] <= as.numeric(as.character(matrix.thresholds[w,3])))
          accession_subset[1:length(rownames(sub)),w] <- rownames(sub)
          w <- w +1
        }
        
        accession_subset <- as.numeric(na.omit(accession_subset[,dim(matrix.thresholds)[1]]))
        data.var.thresholds.final <- object.complete[is.element(object.complete$accession, accession_subset),]
        thresholdFilteredDataSubset <- data.var.thresholds.final
        data.var.thresholds.final <- data.var.thresholds.final[,-1]
        data.var.thresholds.final <- data.var.thresholds.final[,1:7]
        d.thresholds = sapply(data.var.thresholds.final, des.continuous)
        row.names(d.thresholds) <- c("n","Min","Max","Average","Variance","Est.Desv","Median","CV %","NA","NA %")
        d.thresholds = as.table(d.thresholds)
        names(dimnames(d.thresholds)) <- c(" ", paste("Variable thresholds", svalue( datasetSelector(analysis) )))
        
        saveProjectFile( d.thresholds, "ResultsDescriptiveAnalysisThresholds" )   	
        saveProjectFile( thresholdFilteredDataSubset, "ThresholdFilteredDataSubset", row.names = FALSE )   	
        
        print(d.thresholds)
        
        output <- list("Threshold.Values" = matrix.thresholds, "Descript.Thresholds" = d.thresholds)
        
        cat("\n")
        cat("\n")
        cat(paste("Process completed................."))     
        cat("\n")
        
        # Send the user back to the original notebook panel...
        svalue(notebook) <- 3
        
        return(output)
      }
      
      var1; var2; var3; var4; var5; var6; var7; var8; var9; var10
      min.var1; min.var2; min.var3; min.var4; min.var5; min.var6; min.var7; min.var8; min.var9; min.var10
      max.var1; max.var2; max.var3; max.var4; max.var5; max.var6; max.var7; max.var8; max.var9; max.var10
  
  }
  
  tpl[14,1:10] <- gbutton(
    "Save Thresholds",
    container = tpl,
    handler = saveThresholds
	)
	
	visible(win) <- TRUE
  
	
	
}

check.parameters <- function(...){
  
   error <- FALSE
   
   dataset <- currentDataSet(analysis)
   
   ## Selection of number of accessions in final set 
   desiredNumberOfAccessions   <- as.integer( svalue( targetNumberOfAccessions(analysis) ))
   
   availableNumberOfAccessions <- as.integer(dim(dataset)[1])
   
   if(  desiredNumberOfAccessions <= 0 | 
          desiredNumberOfAccessions > availableNumberOfAccessions 
   ){
     error <- TRUE
     DialogBox(paste("Invalid number of accessions.\n",
             "Target number must lie within the range of 1 to",
             as.character(availableNumberOfAccessions),
             sep=" ")
     )
     svalue( targetNumberOfAccessions(analysis) ) <- 10  # default back to 10
  }
  
  nsoln <- as.numeric( svalue( numberOfSolutions(analysis) ))
  
  if( nsoln < 0 | nsoln > 1000000){ 
    error <- TRUE
    DialogBox(paste("Invalid target number of solutions to screen.\n",
              "Target number must lie within the range of 1 and 1000000")
    )
    svalue( numberOfSolutions(analysis)) <- 10000 # default back to 10000
  }
  
  ## Selected number of final accessions
  nfinal <- as.numeric(svalue( numberOfFinalSolutions(session$analysis) ))
  
  if( any( dir( currentProjectFolder(analysis) ) == "ThresholdFilteredDataSubset.csv") == TRUE ){
    availableData <- readProjectFile( "ThresholdFilteredDataSubset" )
  } else {
    availableData <- dataset
  } 
  
  availableNumberOfAccessions <- nrow(availableData)
  if( nfinal <0 | nfinal > availableNumberOfAccessions){
    error <- TRUE
    DialogBox(paste(
        "Invalid number of final solutions.\n",
        "Target number must lie within the range of 1 to",
        as.character(availableNumberOfAccessions),".",
        sep=" ")
    )
    svalue( numberOfFinalSolutions(session$analysis) ) <- availableNumberOfAccessions 
  }
  
  npercent <- as.numeric(svalue( percentageOfSolutions(analysis) ))
  if( npercent < 1 | npercent > 100){
    error <- TRUE
    DialogBox("Error: percentage of solutions out of valid range [1..100]")
    svalue( percentageOfSolutions(analysis) ) <- 1 # default back to 1
  }
  
  if(!error) {
    DialogBox("Parameters are valid :-)")
  }
  
  if(any(is.na(dataset))) {
    DialogBox("Residual missing data detected in the dataset.\nYou may wish to impute the missing data?")
  }
  
}

## Function for selection of number of accessions in final set 
number.access <- function(...){
  
  dataset <- currentDataSet(analysis)
  
  desiredNumberOfAccessions   <- as.integer( svalue( targetNumberOfAccessions(analysis) ))
  
  availableNumberOfAccessions <- as.integer(dim(dataset)[1])
  
  if(  desiredNumberOfAccessions > 0 & 
         desiredNumberOfAccessions <= availableNumberOfAccessions 
  ){
    DialogBox(paste("Target number of accessions in the final set to", desiredNumberOfAccessions, sep=" "))
  } else {
    DialogBox(
      paste("Invalid number of accessions.\n",
            "Target number must lie within the range of 1 to",
            as.character(availableNumberOfAccessions),
            sep=" ")
    )
  }
  
  return( desiredNumberOfAccessions ) 
}

number.solutions <- function(h,...) {
  
  nsoln <- as.numeric( svalue( numberOfSolutions(analysis) ))
  
  if(nsoln > 0 & nsoln <= 1000000){
    DialogBox(paste("The target number of solutions to screen set to", nsoln, sep=" "))
  } else { 
    DialogBox("Invalid target number of solutions to screen.\n",
              "Target number must lie within the range of 1 and 1000000")
  }
  
  return(nsoln) 
}

## Function to set selected number of final accessions
number.final <- function(h,...){
    
  dataset <- currentDataSet(analysis)
  
  nfinal <- as.numeric(svalue( numberOfFinalSolutions(session$analysis) ))
  
  if( any( dir( currentProjectFolder(analysis) ) == "ThresholdFilteredDataSubset.csv") == TRUE ){
    
    availableData <- readProjectFile( "ThresholdFilteredDataSubset" )
    
  } else {
    
    availableData <- dataset
    
  } 
  
  availableNumberOfAccessions <- nrow(availableData)
  if(nfinal <= availableNumberOfAccessions & nfinal > 0){
    DialogBox(paste("Size of the final subset of the accessions set to", nfinal, sep=" "))
  } else {
    DialogBox(
      paste(
        "Invalid size of the final subset.\n",
        "Target number must lie within the range of 1 to",
        as.character(availableNumberOfAccessions),".",
        sep=" ")
    )
    nfinal <- availableNumberOfAccessions 
  }
  
  return(nfinal)
}

number.percent <- function(h,...){
  npercent <- as.numeric(svalue( percentageOfSolutions(analysis) ))
  if( npercent > 0 & npercent <= 100){
    DialogBox(paste("Target percentage of solutions is: ", npercent, "%", sep=" "))
  } else {DialogBox("Error: percentage of solutions out of valid range [0..100]")}
  return(npercent) 
  
}

## Function to select the objective function for nominal variables
select.functions <- function(fitems, f){
  win <- gwindow("Selection function", visible = FALSE, width = 300, height = 100) 
  g   <- ggroup(horizontal=FALSE,  container = win)
  function.optimize <- gdroplist(fitems, expand = TRUE, editable = FALSE,  container = g, handler = f)
  visible(win)<-TRUE
}
