#------------------------------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                                           #
#AUTHOR: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
#VERSION 2.0 - AUGUST-04-2014 
#
# dialogs.r - Smaller GUI Dialog Boxes used by Explora
#------------------------------------------------------------------------------------------------------------------ 
#' @include configuration.r

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

#' @import gWidgetsRGtk2

DialogBox <- function(message, handler=NULL) {## This function make a dialog box
	
	w<- gwindow("Alert",width=100,height=100)
	g <- ggroup( container = w)
	gimage("info", dirname="stock", size="large_toolbar",  container = g)
	
	ig <- ggroup(horizontal = FALSE,  container = g)
	glabel(message,  container = ig, expand = TRUE)
	
	bg <- ggroup( container = ig)
	addSpring(bg)
	gbutton("Ok", handler = function(h,...) dispose(w),  container = bg)
	
	return()
}  


DialogBoxDTree <- function(items) {## This function make a dialog box
	
	out <- ""
	w <-  gbasicdialog("", 
			handler = function(h,...){out <<- svalue(txt)})
	
	glabel("Do you want to continue?",  container = w)
	glabel(" ",  container = w)
	txt <- gradio(items,  container = w)
	visible(w, set = T)
	return(out)
	
}  


DialogSelect <- function(items){## Function to select preference category
	
  out.category <- ""
  
	w <-  gbasicdialog("Select the category of preference", 
			handler = function(h,...){out.category <<- svalue(txt.category)})
	
	txt.category <- gdroplist(items,  container = w)
	visible(w, set = T)
	
  return(out.category) 
	
}

SelectSolution <- function(solutions){## Function to select preference category
	
  out.solution <- ""
  
	w <-  gbasicdialog("Select the solutions", 
			handler = function(h,...){out.solution <<- svalue(txt.solution)})
	
	txt.solution <- gdroplist(solutions,  container = w)
	visible(w, set = T)
  
	return(out.solution)
}

DialogSelectThresholds <- function(object){## Function to select variables for thresholds
	
	object.thresholds <- object
	ncon <- as.numeric( svalue(numContVar(analysis) )) 
	object.thresholds <- object.thresholds[,-1]
	object.thresholds <- object.thresholds[,1:ncon]
	object.complete <- object

	min.values <- matrix(round(as.table(sapply(object.thresholds, min)),3), ncol = 1)
	max.values <- matrix(round(as.table(sapply(object.thresholds, max)),3), ncol = 1)
	
	names.thresholds <- paste(names(object.thresholds),":(","Min = ", min.values," ; ","Max = ",
			max.values, ")", sep = "")
	
	win <- gbasicdialog("Selection variable for threholds", visible = F , width = 700, height = 450,
			handler = function(h,...){var1 <<- svalue(var1);var2 <<- svalue(var2);var3 <<- svalue(var3);var4 <<- svalue(var4);var5 <<- svalue(var5);
				var6 <<- svalue(var6);var7 <<- svalue(var7);var8 <<- svalue(var8);var9 <<- svalue(var9);var10 <<- svalue(var10);
				min.var1 <<- as.numeric(svalue(min.var1)); min.var2 <<- as.numeric(svalue(min.var2));min.var3 <<- as.numeric(svalue(min.var3));min.var4 <<- as.numeric(svalue(min.var4));min.var5 <<- as.numeric(svalue(min.var5));
				min.var6 <<- as.numeric(svalue(min.var6)); min.var7 <<- as.numeric(svalue(min.var7));min.var8 <<- as.numeric(svalue(min.var8));min.var9 <<- as.numeric(svalue(min.var9));min.var10 <<- as.numeric(svalue(min.var10));
				max.var1 <<- as.numeric(svalue(max.var1)); max.var2 <<- as.numeric(svalue(max.var2));max.var3 <<- as.numeric(svalue(max.var3));max.var4 <<- as.numeric(svalue(max.var4));max.var5 <<- as.numeric(svalue(max.var5));
				max.var6 <<- as.numeric(svalue(max.var6)); max.var7 <<- as.numeric(svalue(max.var7));max.var8 <<- as.numeric(svalue(max.var8));max.var9 <<- as.numeric(svalue(max.var9));max.var10 <<- as.numeric(svalue(max.var10))}) 
	
	nb <- gnotebook( container = win, expand = T, tab.pos = 3)
	
	lyt3 = glayout(homogeneous = F,  container = nb, spacing=1, label = "Threshold Analyses", expand = T)
	
	lyt3[3,1]=(glabel=(""))
	
	lyt3[4,1] = glabel("Variables to select",  container = lyt3)
	lyt3[5,1:4] = (var1 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[6,1:4] = (var2 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[7,1:4] = (var3 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[8,1:4] = (var4 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[9,1:4] = (var5 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[10,1:4] = (var6 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[11,1:4] = (var7 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[12,1:4] = (var8 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[13,1:4] = (var9 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	lyt3[14,1:4] = (var10 = gdroplist(c("NA",  names.thresholds),  container = lyt3))
	
	lyt3[1,2]=(glabel=(""))
	
	lyt3[4,7] = glabel("Minimum values",  container = lyt3)
	lyt3[5,7] = (min.var1 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[6,7] = (min.var2 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[7,7] = (min.var3 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[8,7] = (min.var4 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[9,7] = (min.var5 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[10,7] = (min.var6 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[11,7] = (min.var7 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[12,7] = (min.var8 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[13,7] = (min.var9 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	lyt3[14,7] = (min.var10 = gedit("",  container = lyt3, width = 3, initial.msg = "Min"))
	
	lyt3[1,8]=(glabel=(""))
	
	lyt3[4,10] = glabel("Maximum values",  container = lyt3)
	lyt3[5,10] = (max.var1 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[6,10] = (max.var2 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[7,10] = (max.var3 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[8,10] = (max.var4 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[9,10] = (max.var5 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[10,10] = (max.var6 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[11,10] = (max.var7 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[12,10] = (max.var8 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[13,10] = (max.var9 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))
	lyt3[14,10] = (max.var10 = gedit("",  container = lyt3, width = 3, initial.msg = "Max"))

	visible(win)<-TRUE
	
	if(var1!="NA"){var1<-unlist(strsplit(var1, ":"))[1]};if(var2!="NA"){var2<-unlist(strsplit(var2, ":"))[1]}
	if(var3!="NA"){var3<-unlist(strsplit(var3, ":"))[1]};if(var4!="NA"){var4<-unlist(strsplit(var4, ":"))[1]}
	if(var5!="NA"){var5<-unlist(strsplit(var5, ":"))[1]};if(var6!="NA"){var6<-unlist(strsplit(var6, ":"))[1]}
	if(var7!="NA"){var7<-unlist(strsplit(var7, ":"))[1]};if(var8!="NA"){var8<-unlist(strsplit(var8, ":"))[1]}
	if(var9!="NA"){var9<-unlist(strsplit(var9, ":"))[1]};if(var10!="NA"){var10<-unlist(strsplit(var10, ":"))[1]}
	
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
	matrix.thresholds = na.omit(matrix.thresholds)  
	
	val.min <- character(dim(matrix.thresholds)[1])
	val.max <- character(dim(matrix.thresholds)[1])
	
	for(i in 1:dim(matrix.thresholds)[1]){
		
		if(as.numeric(as.matrix(matrix.thresholds)[i,2]) < min(object[colnames(object) ==  as.character(matrix.thresholds[i,1])])){
			val.min[i] <-  as.character(matrix.thresholds[i,1])
		}
		if(as.numeric(as.matrix(matrix.thresholds)[i,3]) > max(object[colnames(object) ==  as.character(matrix.thresholds[i,1])])){
			val.max[i] <-  as.character(matrix.thresholds[i,1])
		}
	}
	
	val.min <- val.min[val.min!=""]
	val.max <- val.max[val.max!=""]
	
	if(length(val.min) == 0 & length(val.max) == 0){
		
		## Extrac subset of data base from thresholds
		data.var.thresholds <- as.data.frame(object[,is.element(colnames(object), matrix.thresholds$Variable)])
		rownames(data.var.thresholds) <- object.complete$n_acces
		n_acces_subset <- matrix(NA, nrow = dim(object)[1], ncol = length(var.thresholds))
		colnames(n_acces_subset) <- as.character(matrix.thresholds[,1])
		
		w<-1
		
		while(w <= dim(matrix.thresholds)[1]){  
			sub <- subset(data.var.thresholds,data.var.thresholds[,w] >= as.numeric(as.character(matrix.thresholds[w,2])) & data.var.thresholds[,w] <= as.numeric(as.character(matrix.thresholds[w,3])))
			data.var.thresholds <- subset(data.var.thresholds,data.var.thresholds[,w] >= as.numeric(as.character(matrix.thresholds[w,2])) & data.var.thresholds[,w] <= as.numeric(as.character(matrix.thresholds[w,3])))
			n_acces_subset[1:length(rownames(sub)),w] <- rownames(sub)
			w <- w +1
		}
		
		n_acces_subset <- as.numeric(na.omit(n_acces_subset[,dim(matrix.thresholds)[1]]))
		data.var.thresholds.final <- object.complete[is.element(object.complete$n_acces, n_acces_subset),]
		Data.Thresholds <- data.var.thresholds.final
		data.var.thresholds.final <- data.var.thresholds.final[,-1]
		data.var.thresholds.final <- data.var.thresholds.final[,1:7]
		d.thresholds = sapply(data.var.thresholds.final, des.continuous)
		row.names(d.thresholds) <- c("n","Min","Max","Average","Variance","Est.Desv","Median","CV %","NA","NA %")
		d.thresholds = as.table(d.thresholds)
		names(dimnames(d.thresholds)) <- c(" ", paste("Variables thresholds",svalue( dataset_selection(analysis) )))
		
		DialogBox(paste("The results should be saved in",getwd(),"/Results"))

		ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"'Results' folder already exists?")

		write.csv(d.thresholds, file = paste(getwd(),"/Results/ResultsDescriptiveAnalysisThresholds.csv", sep=""))
		write.csv(Data.Thresholds, file = paste(getwd(),"/Results/Data.Thresholds.csv", sep=""), row.names = FALSE)
		
		print(d.thresholds)
		
		output <- list("Threshold.Values" = matrix.thresholds, "Descript.Thresholds" = d.thresholds)
		
		cat("\n")
		cat("\n")
		cat(paste("Process completed................."))     
		cat("\n")
		return(output)
	}
	
	var1; var2; var3; var4; var5; var6; var7; var8; var9; var10
	min.var1; min.var2; min.var3; min.var4; min.var5; min.var6; min.var7; min.var8; min.var9; min.var10
	max.var1; max.var2; max.var3; max.var4; max.var5; max.var6; max.var7; max.var8; max.var9; max.var10
	
}

number.access <- function(h,...){## Function for selection of number of accessions in final set 
  
  object = eval(parse(text=svalue( dataset_selection(analysis) )))
  num.access <- as.numeric(svalue(num.access))
  
  if(num.access <= dim(object)[1] & num.access > 0){
    DialogBox(paste("Number of accessions in the final set: ", num.access, sep=" "))
  } else {
    DialogBox("Error in the accession number")
  }
  
  return(num.access) 
}

number.solution <- function(h,...){
  nsoln <- as.numeric( svalue(numberSoln(analysis) ))
  if(nsoln > 0 & nsoln <= 1000000){
    DialogBox(paste("The number of solution is: ", nsoln, sep=" "))
  }else{DialogBox("Error in the percentage of solutions")}
  
  return(nsoln) 
}

## Function to selected number of final accessions
number.final <- function(h,...){
  object = eval(parse(text=svalue( dataset_selection(analysis) )))
  nfinal = as.numeric(svalue(nfinal))
  
  if(any(dir("Results") == "Data.Thresholds.csv") == TRUE){
    Data.Thresholds <- read.csv(paste(getwd(), "/Results/Data.Thresholds.csv", sep=""))
    Data <- Data.Thresholds
  }else if(any(dir("Results") == "Data.Thresholds.csv") == FALSE){
    Data <- object
  } 
  
  if(nfinal <= dim(Data)[1] & nfinal > 0){
    DialogBox(paste("Size of the final subset of the accessions: ", nfinal, sep=" "))
  }else{DialogBox("Error in the size of the final subset")}
  
  return(nfinal)
}

number.percent <- function(h,...){
  npercent <- as.numeric(svalue( percentSoln(analysis) ))
  if( npercent > 0 & npercent <= 100){
    DialogBox(paste("The percentage of solutions is: ", npercent, "%", sep=" "))
  } else {DialogBox("Error in the percentage of solutions")}
  return(npercent) 
  
}

setDialogSession <- function(session) {
  environment(DialogSelectThresholds) <- session
  environment(number.solution)        <- session
  environment(number.final)           <- session
  environment(number.percent)         <- session
}

