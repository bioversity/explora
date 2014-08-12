#------------------------------------------------------------------------------------------------------------------
# APPLICATION BIOVERSITY                                                                                           #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
# VERSION 2.0 - AUGUST-04-2014 
#
# util.r - Explora Utility Functions
#------------------------------------------------------------------------------------------------------------------ 

#' @include dialogs.r
#' 
#' @importMethodsFrom gWidgets svalue

number.access <- function(h,...){## Function for selection of number of accessions in final set 
	
	object = eval(parse(text=svalue(dataset_selection(analysis))))
	num.access <- as.numeric(svalue(num.access))
	
	if(num.access <= dim(object)[1] & num.access > 0){
		DialogBox(paste("Number of accessions in the final set: ", num.access, sep=" "))
	} else {
		DialogBox("Error in the accession number")
	}
	
	return(num.access) 
}

number.solution <- function(h,...){
	nsoln <- as.numeric(svalue(numberSoln(analysis)))
	if(nsoln > 0 & nsoln <= 1000000){
		DialogBox(paste("The number of solution is: ", nsoln, sep=" "))
	}else{DialogBox("Error in the percentage of solutions")}

	return(nsoln) 
}

number.final <- function(h,...){## Function to selected number of final accessions
	object = eval(parse(text=svalue(dataset_selection(analysis))))
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
