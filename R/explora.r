#------------------------------------------------------------------------------------------------------------------
# APPLICATION BIOVERSITY                                                                                           #
# AUTHORS: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM                     #
# VERSION 2.0 - AUGUST-04-2014 
#
# Explora Graphical User Interface (based on RGtk2)
#------------------------------------------------------------------------------------------------------------------ 
#' @include init.r dialogs.r loader.r util.r algorithms.r

require(gWidgets) 
require(gWidgetsRGtk2) 

#' @name explora
#' @title explora
#' @description Bioversity/CCAFS Seeds4Needs Explora germplasm selection tool
#' @details  The main purpose of Explora is to select a set of promising accessions 
#' from genebank collections that meet the interests of the user. Explora allows 
#' 1) selection of germplasm from big characterization and/or evaluation data sets; 
#' 2) consideration of more than one trait and trade-offs between different traits of interests; and 
#' 3) maximization of diversity for specific traits of interests.
#' @aliases explora
#' @author Maarten van Zonneveld,  Johannes Ospina and Richard Bruskiewich
#' @export explora
#' @examples 
#' explora()
explora <- function() {
	
	## Principal window
	win <- gwindow("Explora Germplasm Selection Tool", visible = F , width = 500, height = 300) 
	nb <- gnotebook( container = win, expand = T, tab.pos = 2)
	
	##Load dataset
	lyt1 = glayout(homogeneous = F,  container = nb, spacing = 1, label = "Load dataset", expand = T) 
	lyt1[1,1:3] = (g = gframe("Load dataset",  container = lyt1, horizontal = T))
	lytgb1 = glayout(homogeneous = F, container = g, spacing = 1, expand = T) 
	lytgb1[1,1] = (glabel = (""))
	lytgb1[2,1] = (h = gbutton("Change directory",  container = lytgb1, handler = function(h,...)setwd(gfile(text = "Select directory", type = "selectdir"))))
	lytgb1[3,1] = (glabel = (""))
	lytgb1[4,1] = gbutton("Data set",  container = lytgb1, expand = F, handler = function(h,...){data_set<<-load_dataset()})
	
	##Descriptive analysis
	lyt2 = glayout(homogeneous = F,  container = nb, spacing = 1, label = "Descriptive analysis", expand = T)
	lyt2[1,1:6] = (g1 <- gframe("Descriptive analysis",cont=lyt2,expand=T,horizontal=F))
	lytg2 = glayout(homogeneous = F,  container = g1, spacing = 1, expand = T) 
	lytg2[1,1] = glabel("Dataset for analysis  ",cont=lytg2)
	lytg2[1,2] = ( dataset_selection(analysis) = gdroplist(c("data_set"), selected = 0,  container = lytg2, expand = T, handler = function(h,...){attach(eval(parse(text=svalue(h$obj))))}))
	lytgb1[3,1] = (glabel = (""))
	lytg2[4,1] = glabel("Number of continuous variables: ",  container = lytg2)
	lytg2[4,2] = (numContVar(analysis) <-gedit("",cont=lyt2,width = 10,initial.msg="")) 
	lytg2[5,1] = glabel("Number of categorical variables: ",cont=lytg2)
	lytg2[5,2] = (ncat <-gedit("",cont=lyt2,width = 10,initial.msg=""))
	
	
	lytg2[6,1]=(glabel=(""))
	lytg2[7,1]=(glabel=(""))
	
	lytg2[9,1] = gbutton("Descriptive analysis for continuous variables",cont=lytg2, handler=function(h,...){print(descriptives.continuous(eval(parse(text=svalue(dataset_selection(analysis))))))})
	lytg2[10,1] = gbutton("Descriptive analysis for nominal variables",cont=lytg2, handler=function(h,...){print(descriptives.nominal(eval(parse(text=svalue(dataset_selection(analysis))))))})
	
	lytg2[11,1]=(glabel=(""))
	lytg2[12,1]=(glabel=(""))
	
	lytg2[13,1] = glabel("Level of correlation: ",cont=lytg2)
	lytg2[13,2] = (ncor <- gspinbutton(from=0, to = 1, by = 0.1, value=0, cont=lytg2)) 
	lytg2[14,1] = gbutton("Correlation analysis",cont=lytg2, handler=function(h,...){print(correlation(eval(parse(text=svalue(dataset_selection(analysis))))))})
	
	lytg2[15,1]=(glabel=(""))
	lytg2[16,1]=(glabel=(""))
	
	lytg2[17,1] = glabel("Number accessions in final dataset: ",  container = lytg2)
	lytg2[17,2] = (num.access <- gedit("10",  container = lyt2, width = 10, initial.msg =" "))
	lytg2[18,1] = gbutton("Select number accessions",  container = lyt2, expand=F,
			handler = function(h,...){print(number.access())})
	
	lytg2[19,1]=(glabel=(""))
	lytg2[20,1]=(glabel=(""))
	
	lytg2[21,1] = glabel("Threshold analysis: ",  container = lytg2)
	lytg2[22,1] = gbutton("Select variables",  container = lytg2, handler = function(h,...){DialogSelectThresholds(eval(parse(text=svalue(dataset_selection(analysis)))))})
	
	
	##Selection of preferred analysis
	lyt5 = glayout(homogeneous = F, container = nb , spacing=1,label="Selection of preferred analysis",expand=T)
	lyt5[1,1:10] = (g5 = gframe("Type selection of preferred", container = lyt5, expand = T, horizontal = F))
	lytg5 = glayout(homogeneous = F,  container = g5, spacing = 1, expand = T) 
	
	lytg5[1,1] = glabel("Enter the number of solutions: ",  container = lytg5)
	lytg5[1,2] = (numberSoln(analysis) = gedit("10000",  container = lytg5))
	lytg5[2,1] = gbutton("Select the number of solutions",  container = lytg5, expand=F,
			handler = function(h,...){print(number.solution())})
	
	lytg5[3,1] = (glabel=(""))
	lytg5[4,1] = (glabel=(""))
	
	lytg5[5,1] = glabel("Optimization analysis: ",  container = lytg5)
	lytg5[6,1] = gbutton("Select variables",  container = lytg5, expand=F,
			handler = function(h,...){DialogSelectOptimization(eval(parse(text=svalue(dataset_selection(analysis)))))})
	
	lytg5[7,1] = (glabel=(""))
	lytg5[8,1] = (glabel=(""))
	
	lytg5[9,1] = glabel("Enter the percentage of solutions (%):",  container = lytg5)
	lytg5[9,2] = (percentSoln(analysis) = gedit("1",  container = lytg5))
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
				if(svalue(option.preferred) == "Maximum variation"){MAXVAR.type.opt(output.opt)}
				if(svalue(option.preferred) == "Principal components"){PCA.type.opt(output.opt)}
				if(svalue(option.preferred) == "Weighted sum model"){WSM.type.opt(output.opt)}
				if(svalue(option.preferred) == "Decision tree"){DTree.type.opt(output.opt)}
			})

	## Principal windows
	gwelcome = ggroup(cont=nb,horizontal = F,label="About the Application")
	
	gimage("Explora_Logo.png", dirname = image_dir, size = "button",  container = gwelcome) 
	
	visible(win) <- TRUE

}
