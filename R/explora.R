#------------------------------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                                           #
#AUTHOR: JOHANN FOR BIOVERSITY                                                                                               #
#VERSION 1.0 - FEBRARY-09-2014                                                                                    #
#------------------------------------------------------------------------------------------------------------------ 

## Loading packages (installed by install.R)
library(vegan) 
library(ade4)
library(gWidgets) 
library(gWidgetsRGtk2) 
library(plyr)
library(cluster)
library(grid)
library(gridExtra)

## Clean work spaces
rm(list=ls())

## select tools for GUI
options("guiToolkit"="RGtk2")

## Change locale for message in english
Sys.setlocale(category = "LC_ALL", locale = "English")
Sys.setenv(LANG = "en")


#Functions used--------------------------------------------------------------
load = function(file){file = read.csv(file, header = T, sep = ",")}## This function used to load csv files


load_dataset <- function(){## Load dataset
  data_set = load(gfile(""))
  
  ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
  write.csv(data_set, file = paste(getwd(),"/Results/data_set.csv", sep = ""),
            row.names = FALSE)
  return(data_set)
}


DialogBox <- function(message, handler=NULL) {## This function make a dialog box
  
  w<- gwindow("Alert",width=100,height=100)
  g <- ggroup(cont = w)
  gimage("info", dirname="stock", size="large_toolbar", cont = g)
  
  ig <- ggroup(horizontal = FALSE, cont = g)
  glabel(message, cont = ig, expand = TRUE)
  
  bg <- ggroup(cont = ig)
  addSpring(bg)
  gbutton("Ok", handler = function(h,...) dispose(w), cont = bg)
  
  return()
}  



DialogBoxDTree <- function(items) {## This function make a dialog box
  
  
  w <-  gbasicdialog("", 
                     handler = function(h,...){out <<- svalue(txt)})
  
  glabel("Do you want to continue?", con = w)
  glabel(" ", con = w)
  txt <- gradio(items, cont = w)
  visible(w, set = T)
  out
  
}  


DialogSelect <- function(items){## Function to select preferency category
  
  w <-  gbasicdialog("Select the category of preference", 
                     handler = function(h,...){out.category <<- svalue(txt.category)})
  
  txt.category <- gdroplist(items, cont = w)
  visible(w, set = T)
  out.category 
  
}





SelectSolution <- function(solutions){## Function to select preferency category
  
  w <-  gbasicdialog("Select the solutions", 
                     handler = function(h,...){out.solution <<- svalue(txt.solution)})
  
  txt.solution <- gdroplist(solutions, cont = w)
  visible(w, set = T)
  out.solution
}






des.continuos <- function(object){## Descriptive analysis for continuous variables
  n = length(object)
  average = round(mean(object, na.rm=T),3)
  variance = round(var(object, na.rm=T),3)
  Est.Desv = round(sqrt(variance),3)
  median = round(as.numeric(quantile(object,probs=0.5, na.rm=T)),3)
  Coef.Var = round((sqrt(variance)/average)*100,3)
  min = round(min(object,na.rm=T),3)
  max = round(max(object,na.rm=T),3)
  NA.Data =round(sum(is.na(object)),3)
  NA.Percent = round((NA.Data/length(object))*100,3)
  result = cbind(n, min, max, average, variance, Est.Desv, median, Coef.Var, NA.Data, NA.Percent)
}


descriptives.continuos = function(object){## Used to function 'des.continuos' 
  
  ncon <- as.numeric(svalue(ncon))
  object <- object[,-1]
  object <- object[,1:ncon]
  d = sapply(object, des.continuos)
  row.names(d) <- c("n","Min","Max","Average","Variance","Est.Desv","Median","CV %","NA","NA %")
  d = as.table(d)
  names(dimnames(d)) <- c(" ", paste("Variable",svalue(nom_data)))
  
  DialogBox(paste("The results should be saved in",getwd(),"/Results"))
  ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
  write.csv(d, file = paste(getwd(),"/Results/ResultsDescriptiveAnalysisContinuousVariables.csv", sep=""))
  
  return(d)
  
}


des.nominal <- function(object){## Descriptive analysis for nominal variables
  n=length(object)
  category = names(table(object)) 
  frecuence = table(object)
  percentage = round((table(object)/sum(object))*100,2)
  NA.Data = round(sum(is.na(object)),0)
  NA.Percent = round((NA.Data/length(object))*100,2)
  result=cbind(n, category, frecuence, percentage, NA.Data, NA.Percent)
}


descriptives.nominal=function(object){## Used to function 'des.nominal'
  ncon <-as.numeric(svalue(ncon))
  ncat <-as.numeric(svalue(ncat))
  object <- object[,-1]
  object <- object[,(ncon+1):dim(object)[2]]
  d=lapply(object, des.nominal)
  d=ldply(d, data.frame)
  colnames(d) <-c("Variable", "n", "Category", "Freq.Cat","%.Cat", "NA","NA %")
  names(dimnames(d)) <- c(" ", paste("Variable",svalue(nom_data)))
  
  DialogBox(paste("The results should be saved in",getwd(),"/Results"))
  ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
  write.csv(d, file = paste(getwd(),"/Results/ResultsDescriptiveAnalysisNominalVariables.csv", sep=""),
            row.names = FALSE)
  
  
  return(d)
  
}

number.access <- function(h,...){## Function for selection of number of accessions in final set 
  
  object = eval(parse(text=svalue(nom_data)))
  num.access <- as.numeric(svalue(num.access))
  
  if(num.access <= dim(object)[1] & num.access > 0){
    DialogBox(paste("The accessions number in final set: ", num.access, sep=" "))
  }else{DialogBox("Error in the accession number")}
  
  num.access 
}


correlation <- function(object){## Correlation analysis
  ncon <-as.numeric(svalue(ncon))
  
  object <- object[,-1]
  object <- object[,1:ncon]
  correlation <- round(cor(object),2)
  correlation[lower.tri(correlation,diag=TRUE)]=NA 
  correlation <- as.data.frame(as.table(correlation))
  correlation=na.omit(correlation)        
  direction <- rep("",times = dim(correlation)[1])
  
  for(i in 1:dim(correlation)[1]){ ## Direction of correlation
    if(correlation[i,3]>=0 & correlation[i,3]<=1){direction[i]="positive"}
    else if(correlation[i,3]<0 & correlation[i,3]>=-1){direction[i]="negative"}
  }
  
  correlation = cbind(correlation, direction)
  colnames(correlation) <- c("Variable 1", "Variable 2", "Correlation", "Direction") 
  names(dimnames(correlation)) <- c(" ", paste("Variable",svalue(nom_data)))
  cat("\n")
  cat("\n")
  
  ncor <-as.numeric(svalue(ncor))
  ncor <- abs(ncor)
  
  if(ncor==0){
    
    DialogBox(paste("The results should be saved in",getwd(),"/Results"))
    write.csv(correlation, file = paste(getwd(),"/Results/ResultsCorrelationAnalysisGlobal.csv", sep=""), 
              row.names = FALSE)
    
    return(correlation)
    
  }else if(ncor>0){
    correlation.ncor <- subset(correlation, abs(correlation$Correlation)>=ncor)
    DialogBox(paste("The results should be saved in",getwd(),"/Results"))
    write.csv(correlation.ncor, file = paste(getwd(),"/Results/ResultsCorrelationAnalysisLevel",ncor,".csv", sep=""),
              row.names = FALSE)
    
    cat("\n")
    cat("\n")
    cat(paste("#Correlations >= ",ncor,"#\n"))     
    return(correlation.ncor)
  }
  
}




DialogSelectThreholds <- function(object){## Function to select variables for threholds
  
  object.threholds <- object
  ncon <- as.numeric(svalue(ncon)) 
  object.threholds <- object.threholds[,-1]
  object.threholds <- object.threholds[,1:ncon]
  object.complete <- object
  
  
  min.values <- matrix(round(as.table(sapply(object.threholds, min)),3), ncol = 1)
  max.values <- matrix(round(as.table(sapply(object.threholds, max)),3), ncol = 1)
  
  names.threholds <- paste(names(object.threholds),":(","Min = ", min.values," ; ","Max = ",
                           max.values, ")", sep = "")
  
  win <- gbasicdialog("Selection variable for threholds", visible = F , width = 700, height = 450,
                      handler = function(h,...){var1 <<- svalue(var1);var2 <<- svalue(var2);var3 <<- svalue(var3);var4 <<- svalue(var4);var5 <<- svalue(var5);
                                                var6 <<- svalue(var6);var7 <<- svalue(var7);var8 <<- svalue(var8);var9 <<- svalue(var9);var10 <<- svalue(var10);
                                                min.var1 <<- as.numeric(svalue(min.var1)); min.var2 <<- as.numeric(svalue(min.var2));min.var3 <<- as.numeric(svalue(min.var3));min.var4 <<- as.numeric(svalue(min.var4));min.var5 <<- as.numeric(svalue(min.var5));
                                                min.var6 <<- as.numeric(svalue(min.var6)); min.var7 <<- as.numeric(svalue(min.var7));min.var8 <<- as.numeric(svalue(min.var8));min.var9 <<- as.numeric(svalue(min.var9));min.var10 <<- as.numeric(svalue(min.var10));
                                                max.var1 <<- as.numeric(svalue(max.var1)); max.var2 <<- as.numeric(svalue(max.var2));max.var3 <<- as.numeric(svalue(max.var3));max.var4 <<- as.numeric(svalue(max.var4));max.var5 <<- as.numeric(svalue(max.var5));
                                                max.var6 <<- as.numeric(svalue(max.var6)); max.var7 <<- as.numeric(svalue(max.var7));max.var8 <<- as.numeric(svalue(max.var8));max.var9 <<- as.numeric(svalue(max.var9));max.var10 <<- as.numeric(svalue(max.var10))}) 
  
  nb <- gnotebook(cont = win, expand = T, tab.pos = 3)
  
  lyt3 = glayout(homogeneous = F, cont = nb, spacing=1, label = "Threholds analysis", expand = T)
  
  lyt3[3,1]=(glabel=(""))
  
  lyt3[4,1] = glabel("Variables to select", cont = lyt3)
  lyt3[5,1:4] = (var1 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[6,1:4] = (var2 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[7,1:4] = (var3 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[8,1:4] = (var4 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[9,1:4] = (var5 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[10,1:4] = (var6 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[11,1:4] = (var7 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[12,1:4] = (var8 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[13,1:4] = (var9 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  lyt3[14,1:4] = (var10 = gdroplist(c("NA",  names.threholds), cont = lyt3))
  
  lyt3[1,2]=(glabel=(""))
  
  lyt3[4,7] = glabel("Minimum values", cont = lyt3)
  lyt3[5,7] = (min.var1 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[6,7] = (min.var2 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[7,7] = (min.var3 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[8,7] = (min.var4 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[9,7] = (min.var5 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[10,7] = (min.var6 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[11,7] = (min.var7 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[12,7] = (min.var8 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[13,7] = (min.var9 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  lyt3[14,7] = (min.var10 = gedit("", cont = lyt3, width = 3, initial.msg = "Min"))
  
  lyt3[1,8]=(glabel=(""))
  
  lyt3[4,10] = glabel("Maximum values", cont = lyt3)
  lyt3[5,10] = (max.var1 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[6,10] = (max.var2 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[7,10] = (max.var3 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[8,10] = (max.var4 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[9,10] = (max.var5 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[10,10] = (max.var6 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[11,10] = (max.var7 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[12,10] = (max.var8 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[13,10] = (max.var9 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  lyt3[14,10] = (max.var10 = gedit("", cont = lyt3, width = 3, initial.msg = "Max"))
  
  
  
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
    d.thresholds = sapply(data.var.thresholds.final, des.continuos)
    row.names(d.thresholds) <- c("n","Min","Max","Average","Variance","Est.Desv","Median","CV %","NA","NA %")
    d.thresholds = as.table(d.thresholds)
    names(dimnames(d.thresholds)) <- c(" ", paste("Variables thresholds",svalue(nom_data)))
    
    
    DialogBox(paste("The results should be saved in",getwd(),"/Results"))
    ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
    write.csv(d.thresholds, file = paste(getwd(),"/Results/ResultsDescriptiveAnalysisThresholds.csv", sep=""))
    write.csv(Data.Thresholds, file = paste(getwd(),"/Results/Data.Thresholds.csv", sep=""), row.names = FALSE)
    
    print(d.thresholds)
    
    output <- list("Thresholds.Values" = matrix.thresholds, "Descript.Thresholds" = d.thresholds)
    
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




select.functions <- function(fitems, f){## Function to select the objective function for nominal variables
  win <- gwindow("Selection function", visible = F, width = 300, height = 100) 
  g <- ggroup(horizontal=FALSE, cont = win)
  function.optimize <- gdroplist(fitems, expand = T, editable = F, cont = g, handler = f)
  visible(win)<-TRUE
}





DialogSelectOptimization <- function(object){
  
  
  f.items.cont <- c("NA","CV: Maximize coefficient of variation",
                    "MAX.AV: Maximize average", 
                    "MAX.MIN: Maximize minimum", 
                    "MAX.MAX: Maximize maximum",
                    "MIN.AV: Minimize average", 
                    "MIN.MIN: Minimize minimum", 
                    "MIN.MAX: Minimize maximum")
  
  f.items.nom <- c("NA", "SHANNON: Maximize Shannon index", "MAX.PROP: Maximize proportion")
  
  ncon <- as.numeric(svalue(ncon)) 
  object.optimization <- object[,-1]
  object.continuos <- object.optimization[,1:ncon]
  object.nominal <- object.optimization[,(ncon+1):dim(object.optimization)[2]]
  names.continuos <- names(object.continuos)
  names.nominal <- names(object.nominal)
  Nsim <- as.numeric(svalue(Nsim))
  pcategory.v1 <<- 0
  pcategory.v2 <<- 0
  pcategory.v3 <<- 0
  pcategory.v4 <<- 0
  pcategory.v5 <<- 0
  
  win <- gbasicdialog("Selection variable for optmization", visible = F , width = 700, height = 450,
                      handler = function(h,...){varop1c<<-svalue(varop1c);varop2c<<-svalue(varop2c);varop3c<<-svalue(varop3c);varop4c<<-svalue(varop4c);varop5c<<-svalue(varop5c);
                                                fvarop1c<<-svalue(fvarop1c);fvarop2c<<-svalue(fvarop2c);fvarop3c<<-svalue(fvarop3c);fvarop4c<<-svalue(fvarop4c);fvarop5c<<-svalue(fvarop5c);
                                                ri1c<<-svalue(ri1c);ri2c<<-svalue(ri2c);ri3c<<-svalue(ri3c);ri4c<<-svalue(ri4c);ri5c<<-svalue(ri5c);
                                                fvarop1n<<-svalue(fvarop1n);fvarop2n<<-svalue(fvarop2n);fvarop3n<<-svalue(fvarop3n);fvarop4n<<-svalue(fvarop4n);fvarop5n<<-svalue(fvarop5n);
                                                ri1n<<-svalue(ri1n);ri2n<<-svalue(ri2n);ri3n<<-svalue(ri3n);ri4n<<-svalue(ri4n);ri5n<<-svalue(ri5n)}) 
  
  nb <- gnotebook(cont = win, expand = T, tab.pos = 3)
  
  
  lyt4=glayout(homogeneous = F,cont = nb , spacing=1,label="Optimization",expand=T)
  lyt4[1,1:25] = (g4 = gframe("Optimization",cont = lyt4, expand = T, horizontal = F))
  lytg4 = glayout(homogeneous = F, cont = g4, spacing = 1, expand = T) 
  lytg4[1,1]=(glabel=(""))
  lytg4[2,1]=(glabel=(""))
  
  lytg4[3,1] = glabel("Optimization of continuous variables: ", cont =lytg4)
  lytg4[4,1]=(glabel=(""))
  lytg4[5,1] = glabel("Variable: ", cont =lytg4)
  
  lytg4[5,2] = glabel("Objetive function: ", cont =lytg4)
  lytg4[5,3] = glabel("Ranking of importance (1: Low ; 10: hight):", cont =lytg4)
  
  lytg4[6,1] = (varop1c = gdroplist(c("NA", names.continuos), cont = lytg4))
  lytg4[6,2] = (fvarop1c = gdroplist(f.items.cont, cont = lytg4))
  lytg4[6,3] = (ri1c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[7,1] = (varop2c = gdroplist(c("NA", names.continuos), cont = lytg4))
  lytg4[7,2] = (fvarop2c = gdroplist(f.items.cont, cont = lytg4))
  lytg4[7,3] = (ri2c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[8,1] = (varop3c = gdroplist(c("NA", names.continuos), cont = lytg4))
  lytg4[8,2] = (fvarop3c = gdroplist(f.items.cont, cont = lytg4))
  lytg4[8,3] = (ri3c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[9,1] = (varop4c = gdroplist(c("NA", names.continuos), cont = lytg4))
  lytg4[9,2] = (fvarop4c = gdroplist(f.items.cont, cont = lytg4))
  lytg4[9,3] = (ri4c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[10,1] = (varop5c = gdroplist(c("NA", names.continuos), cont = lytg4))
  lytg4[10,2] = (fvarop5c = gdroplist(f.items.cont, cont = lytg4))
  lytg4[10,3] = (ri5c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  
  
  lytg4[11,1]=(glabel=(""))
  lytg4[12,1]=(glabel=(""))
  
  lytg4[13,1] = glabel("Optimization of nominal variables: ", cont =lytg4)
  lytg4[14,1]=(glabel=(""))
  lytg4[15,1] = glabel("Variable: ", cont =lytg4)
  lytg4[15,2] = glabel("Objetive function: ", cont =lytg4)
  lytg4[15,3] = glabel("Ranking of importance (1: Low ; 10: hight):", cont =lytg4)
  lytg4[16,1] = (varop1n = gdroplist(c("NA",names.nominal), cont = lytg4))
  lytg4[16,2] = (fvarop1n = gdroplist(f.items.nom, cont = lytg4, handler = function(h,...){
    if(svalue(fvarop1n) == "MAX.PROP: Maximize proportion" & svalue(varop1n) != "NA"){pcategory.v1 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop1n)])))))}}))
  lytg4[16,3] = (ri1n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[17,1] = (varop2n = gdroplist(c("NA",names.nominal), cont = lytg4))
  lytg4[17,2] = (fvarop2n = gdroplist(f.items.nom, cont = lytg4, handler = function(h,...){
    if(svalue(fvarop2n) == "MAX.PROP: Maximize proportion" & svalue(varop2n) != "NA"){pcategory.v2 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop2n)])))))}}))
  lytg4[17,3] = (ri2n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[18,1] = (varop3n = gdroplist(c("NA",names.nominal), cont = lytg4))
  lytg4[18,2] = (fvarop3n = gdroplist(f.items.nom, cont = lytg4, handler = function(h,...){
    if(svalue(fvarop3n) == "MAX.PROP: Maximize proportion" & svalue(varop3n) != "NA"){pcategory.v3 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop3n)])))))}}))
  lytg4[18,3] = (ri3n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[19,1] = (varop4n = gdroplist(c("NA",names.nominal), cont = lytg4))
  lytg4[19,2] = (fvarop4n = gdroplist(f.items.nom, cont = lytg4, handler = function(h,...){
    if(svalue(fvarop4n) == "MAX.PROP: Maximize proportion" & svalue(varop4n) != "NA"){pcategory.v4 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop4n)])))))}}))
  lytg4[19,3] = (ri4n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  lytg4[20,1] = (varop5n = gdroplist(c("NA",names.nominal), cont = lytg4))
  lytg4[20,2] = (fvarop5n = gdroplist(f.items.nom, cont = lytg4, handler = function(h,...){
    if(svalue(fvarop5n) == "MAX.PROP: Maximize proportion" & svalue(varop1n) != "NA"){pcategory.v5 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop5n)])))))}}))
  lytg4[20,3] = (ri5n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, cont=lytg4)) 
  
  lytg4[21,1]=(glabel=(""))
  lytg4[22,1]=(glabel=(""))
  lytg4[23,1]=(glabel=("Note: Run the optimization procedure before close the window"))
  
  
  lytg4[25,2] = gbutton("Optimization", cont = lytg4,
                        handler = function(h,...){
                          output.opt <<- f.optimization(varop1c, varop2c, varop3c, varop4c, varop5c,
                                                        fvarop1c, fvarop2c, fvarop3c, fvarop4c, fvarop5c,
                                                        varop1n, varop2n, varop3n, varop4n, varop5n,
                                                        fvarop1n, fvarop2n, fvarop3n, fvarop4n, fvarop5n,
                                                        pcategory.v1, pcategory.v2, pcategory.v3,
                                                        pcategory.v4,pcategory.v5)}) 
  
  
  
  
  
  visible(win)<-TRUE
  
  RI <- matrix(c(svalue(ri1c), svalue(ri2c), svalue(ri3c), svalue(ri4c), svalue(ri5c),
                 svalue(ri1n), svalue(ri2n), svalue(ri3n), svalue(ri4n), svalue(ri5n)), ncol = 2)
  
  colnames(RI)<-c("ric", "rin")
  
  varop1c; varop2c; varop3c; varop4c; varop5c
  fvarop1c; fvarop2c; fvarop3c; fvarop4c; fvarop5c
  ri1c; ri2c; ri3c; ri4c; ri5c
  varop1n; varop2n; varop3n; varop4n; varop5n
  fvarop1n; fvarop2n; fvarop3n; fvarop4n; fvarop5n
  ri1n; ri2n; ri3n; ri4n; ri5n
  
  
  write.csv(RI, paste(getwd(),"/Results/RI.csv", sep = ""), row.names = FALSE)
  
  
  
}




number.percent <- function(h,...){
  npercen <- as.numeric(svalue(npercen))
  if(npercen > 0 & npercen <= 100){
    DialogBox(paste("The percentage of solutions is: ", npercen, "%", sep=" "))
  }else{DialogBox("Error in the percentage of solutions")}
  npercen 
  
}

number.solution <- function(h,...){
  Nsim <- as.numeric(svalue(Nsim))
  if(Nsim > 0 & Nsim <= 1000000){
    DialogBox(paste("The number of solution is: ", Nsim, sep=" "))
  }else{DialogBox("Error in the percentage of solutions")}
  Nsim
  
}


number.final <- function(h,...){## Function to selected number of final accessions
  object = eval(parse(text=svalue(nom_data)))
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
  
  nfinal
}


optimization <- function(data, option1, option2, num.access){ ## calculate 'optimization'
  
  name.var.func.con <- NA; val.var.func.con <- NA; name.var.func.nom <- NA; val.var.func.nom <- NA
  
  
  
  
  ##6)  Sampling without replacement of random combinations
  subset0 <- sample(data[,1],num.access, replace = F)
  
  ##7)  Calculation of objective functions for each subset and defined variables: 
  
  if(any(is.na(option1))==FALSE){
    data.con <- cbind(data[,1], data[,is.element(colnames(data),option1[,1])])
    colnames(data.con) <- c("n_acces", colnames(data)[is.element(colnames(data),option1[,1])])
    data0 <- data.con
    
    name.var.func.con <- rep(NA,dim(option1)[1])
    val.var.func.con <- rep(NA,dim(option1)[1]) 
    
    
    
    
    for(i in 1:dim(option1)[1]){ 
      
      if(option1[i,2] == "CV: Maximize coefficient of variation"){
        data1 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "CV",sep="")
        val.var.func.con[i] <- sd(data1)/mean(data1)
      }else if(option1[i,2] == "MAX.AV: Maximize average"){
        data1 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "MAX.AV",sep="")
        val.var.func.con[i] <- mean(data1)
      }else if(option1[i,2] == "MAX.MIN: Maximize minimum"){
        data1 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "MAX.MIN",sep="")
        val.var.func.con[i] <- min(data1)
      }else if(option1[i,2] == "MAX.MAX: Maximize maximum"){
        data1 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "MAX.MAX",sep="")
        val.var.func.con[i] <- max(data1)
      }else if(option1[i,2] == "MIN.AV: Minimize average"){
        data1 <- data0[,as.character(option1[i,1])]
        data2 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "MIN.AV",sep="")
        val.var.func.con[i] <- max(data1)-mean(data2)
      }else if(option1[i,2] == "MIN.MIN: Minimize minimum"){
        data1 <- data0[,as.character(option1[i,1])]
        data2 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "MIN.MIN",sep="")
        val.var.func.con[i] <- max(data1)-mean(data2)
      }else if(option1[i,2] == "MIN.MAX: Minimize maximum"){
        data1 <- data0[,as.character(option1[i,1])]
        data2 <- data0[is.element(data$n_acces,subset0),as.character(option1[i,1])]
        name.var.func.con[i] <- paste(option1[i,1],"-", "MIN.MAX",sep="")
        val.var.func.con[i] <- max(data1)-max(data2)
      }
    }
  } 
  
  if(any(is.na(option2))==FALSE){  
    data.nom <- cbind("n_acces" = data[,1], data[,is.element(colnames(data),option2[,1])])
    colnames(data.nom) <- c("n_acces", colnames(data)[is.element(colnames(data),option2[,1])])
    data0 <- data.nom
    
    name.var.func.nom <- rep(NA, dim(option2)[1])
    val.var.func.nom <- rep(NA, dim(option2)[1])
    
    
    
    for(i in 1:dim(option2)[1]){  
      if(option2[i,2] == "MAX.PROP: Maximize proportion"){
        data1 <- data0[is.element(data$n_acces,subset0),as.character(option2[i,1])]
        name.var.func.nom[i] <- paste(option2[i,1],"-", "MAX.PROP",sep="")
        val.var.func.nom[i] <- as.numeric(prop.table(table(data1 == as.numeric(option2[i,3])))[2])
        ifelse(val.var.func.nom[i] == NA, 0, val.var.func.nom[i])
      }else if(option2[i,2] == "SHANNON: Maximize Shannon index"){
        data1 <- data0[is.element(data$n_acces,subset0),as.character(option2[i,1])]
        name.var.func.nom[i] <- paste(option2[i,1],"-", "SHANN",sep="")
        val.var.func.nom[i] <- diversity(as.numeric(data1), index = "shannon", MARGIN = 1) 
      }
    }
  }  
  
  
  
  output <- list(c(val.var.func.con, val.var.func.nom),subset0,
                 "names" = c(name.var.func.con, name.var.func.nom))
  
  
  return(output)
  
} 


f.optimization <- function(varop1c, varop2c, varop3c, varop4c, varop5c,
                           fvarop1c, fvarop2c, fvarop3c, fvarop4c, fvarop5c,                           
                           varop1n, varop2n, varop3n, varop4n, varop5n,
                           fvarop1n, fvarop2n, fvarop3n, fvarop4n, fvarop5n,
                           pcategory.v1, pcategory.v2, pcategory.v3, pcategory.v4, pcategory.v5){## load the function "optimization" and integrated into the GUI
  
  object = eval(parse(text=svalue(nom_data)))
  ncon <- as.numeric(svalue(ncon)) 
  ncat <- as.numeric(svalue(ncat)) 
  Nsim <- as.numeric(svalue(Nsim)) 
  num.access <- as.numeric(svalue(num.access)) 
  
  varop1c <- svalue(varop1c)
  varop1n <- svalue(varop1n)
  varop2c <- svalue(varop2c)
  varop2n <- svalue(varop2n)
  varop3c <- svalue(varop3c)
  varop3n <- svalue(varop3n)
  varop4c <- svalue(varop4c)
  varop4n <- svalue(varop4n)
  varop5c <- svalue(varop5c)
  varop5n <- svalue(varop5n)
  fvarop1c <- svalue(fvarop1c)
  fvarop1n <- svalue(fvarop1n)
  fvarop2c <- svalue(fvarop2c)
  fvarop2n <- svalue(fvarop2n)
  fvarop3c <- svalue(fvarop3c)
  fvarop3n <- svalue(fvarop3n)
  fvarop4c <- svalue(fvarop4c)
  fvarop4n <- svalue(fvarop4n)
  fvarop5c <- svalue(fvarop5c)
  fvarop5n <- svalue(fvarop5n)
  pcategory.v1 <- svalue(pcategory.v1)
  pcategory.v2 <- svalue(pcategory.v2)
  pcategory.v3 <- svalue(pcategory.v3)
  pcategory.v4 <- svalue(pcategory.v4)
  pcategory.v5 <- svalue(pcategory.v5)
  
  
  if(any(dir("Results") == "Data.Thresholds.csv") == TRUE){
    Data.Thresholds <- read.csv(paste(getwd(), "/Results/Data.Thresholds.csv", sep=""))
    Data <- Data.Thresholds
  }else if(any(dir("Results") == "Data.Thresholds.csv") == FALSE){
    Data <- object
  } 
  
  
  option.objective.con <- matrix(c(varop1c, varop2c, varop3c, varop4c, varop5c,
                                   fvarop1c, fvarop2c, fvarop3c, fvarop4c, fvarop5c),
                                 nrow = 5, ncol = 2)
  colnames(option.objective.con) <- c("Variable", "Objective function")
  
  
  option.objective.nom <- matrix(c(varop1n, varop2n, varop3n, varop4n, varop5n,
                                   fvarop1n, fvarop2n, fvarop3n, fvarop4n, fvarop5n,
                                   pcategory.v1, pcategory.v2, pcategory.v3, pcategory.v4, pcategory.v5),
                                 nrow = 5, ncol = 3)
  colnames(option.objective.nom) <- c("Variable", "Objective function", "Preference category")
  
  
  output.opt0 <- list()
  
  ## Creates the progress bar
  ProgressBar <- winProgressBar(title = "Progress bar", min = 0, max = Nsim, width = 500)
  for(j in 1:Nsim){
    output.opt0[[j]] <- optimization(Data, option.objective.con, option.objective.nom, num.access)
    setWinProgressBar(ProgressBar, j, title=paste(round(j/Nsim*100, 0), "% progress"))
  }
  close(ProgressBar)
  
  return(output.opt0)
  
}



MAXVAR.type.opt <- function(output.opt0){
  
  
  Nsim <- as.numeric(svalue(Nsim))
  npercen <- as.numeric(svalue(npercen))
  nfinal <- as.numeric(svalue(nfinal))
  num.access <- as.numeric(svalue(num.access))
  object <- eval(parse(text=svalue(nom_data)))
  
  ##8)  Standardize values in the sampled subsets:   
  result <- apply(t(sapply(output.opt0, "[[", 1)), MARGIN = 2, FUN = scale)
  colnames(result) <- as.character(output.opt0[[1]]$names)
  
  ##Drop columns with NA's
  result <- as.data.frame(result)
  result <- result[sapply(result, function(x) !all(is.na(x)))]
  
  ##9)  Calculation of mean value of the objective functions for each subset   
  mean.result <- list("standardized.means" = apply(result, MARGIN = 1, FUN = mean),"accessions"=t(sapply(output.opt0, "[[", 2)))
  
  
  ##10)  Selection of solutions with highest standardized mean values (1%) ##seleccionar el %
  
  pos <- order(mean.result[[1]],decreasing=T)[1:(Nsim*(npercen/100))] #save postion of solution with highest standardized mean values 
  
  
  cat("\n")
  result.mean.acces <- mean.result$accessions[pos,]
  colnames(result.mean.acces)<-rep(paste("acces",1:dim(result.mean.acces)[2],sep=""))
  rownames(result.mean.acces)<-rep(paste("sol",1:dim(result.mean.acces)[1],sep=""))
  
  
  
  paste("SubsetOfAccessionsWith",npercen,"%","HighestStandardizedMeanValues",sep="")
  cat(paste("#Subset of accessions with ",npercen,"%"," highest standardized mean values#",sep=""))
  cat("\n")
  print(result.mean.acces)
  cat("\n")
  cat("\n")
  
  write.csv(result.mean.acces, 
            file = paste(getwd(),"/Results/","SubsetOfAccessionsWith",npercen,"%","HighestStandardizedMeanValues.csv", sep=""))
  
  cat("\n")
  result.scale <- result[pos,]
  result.scale <- cbind("solutions" = pos,result.scale)
  cat("#Highest standardized values of subset#")
  cat("\n")
  cat("\n")
  print(result.scale)
  cat("\n")
  cat("\n")
  
  write.csv(result.scale, file = paste(getwd(),"/Results/HighestStandardizedValuesOfSubset_MV.csv", sep=""),
            row.names = FALSE)
  
  
  ##11)  Selection of final set of optimal solutions: 
  
  ## Generate all pairs of solutions
  c <- t(combn(as.character(result.scale[,1]),2)) 
  
  ## Calculate the variance for all pairs of solutions
  variance <-matrix(NA, nrow = dim(c)[1], ncol = 3)
  colnames(variance) <- c("Solution 1", "Solution 2", "Sum of Variance")
  variance <- as.data.frame(variance)
  
  ProgressBar <- winProgressBar(title = "Progress bar", min = 0, max = dim(c)[1], width = 500)
  
  for(i in 1:dim(c)[1]){
    variance[i,]<-cbind(c[i,1], c[i,2],sum(apply(rbind(result.scale[is.element(result.scale[,1],c[i,1]),-1],
                                                       result.scale[is.element(result.scale[,1],c[i,2]),-1]),2,var)))
    
    setWinProgressBar(ProgressBar, i, title=paste(round(i/(dim(c)[1])*100, 0), "% progress"))
  }  
  
  close(ProgressBar)
  cat("\n")
  print("Process completed.................")  
  
  pos.nfinal <- order(variance[,3],decreasing=T)
  pos.max.var <- pos.nfinal[1]
  pos.variance.aux <- sample(unique(as.numeric(c(variance[pos.nfinal,1], variance[pos.nfinal,2]))), (3*num.access))
  
  
  
  
  
  if(any(dir("Results") == "Data.Thresholds.csv") == TRUE){
    Data.Thresholds <- read.csv(paste(getwd(),"/Results/Data.Thresholds.csv", sep=""))
    DataFinal <- Data.Thresholds
  }else if(any(dir("Results") == "Data.Thresholds.csv") == FALSE){
    DataFinal <- object
  } 
  
  final.subset.max.var <- DataFinal[is.element(DataFinal$n_acces,unique(mean.result$accessions[pos.variance.aux,1])[1:num.access]),]
  
  ##Selection of preferred set by max variation  
  cat("\n")
  cat("\n")
  cat("#Subset of optimal solution by maximum variation#")
  cat("\n")
  cat("\n")
  print(final.subset.max.var) 
  cat("\n")
  cat("\n")
  DialogBox(paste("The results should be saved in",getwd(),"/Results"))
  ifelse(file.exists("Results") == "FALSE", dir.create("Results"), "Folder already exists 'Results' ")
  write.csv(final.subset.max.var, file = paste(getwd(),"/Results/subset_optimal_solution_by_MaXVAR","_Solution(",pos.max.var,")",".csv", sep=""),
            row.names = FALSE)
  
  
  
  
  
  
}


PCA.type.opt <- function(output.opt0){
  
  Nsim <- as.numeric(svalue(Nsim))
  npercen <- as.numeric(svalue(npercen))
  nfinal <- as.numeric(svalue(nfinal))
  object <- eval(parse(text=svalue(nom_data)))
  
  ##8)  Standardize values in the sampled subsets:   
  result <- apply(t(sapply(output.opt0, "[[", 1)), MARGIN = 2, FUN = scale)
  colnames(result) <- as.character(output.opt0[[1]]$names)
  
  
  ##Drop columns with NA's
  result <- as.data.frame(result)
  result <- result[sapply(result, function(x) !all(is.na(x)))]
  
  
  
  mean.result <- list("standardized.means" = apply(result, MARGIN = 1, FUN = mean),"accessions"=t(sapply(output.opt0, "[[", 2)))
  pos <- order(mean.result[[1]],decreasing=T)[1:(Nsim*(npercen/100))] #save postion of solution with highest standardized mean values 
  result.mean.acces <- mean.result$accessions[pos,]
  colnames(result.mean.acces)<-rep(paste("acces",1:dim(result.mean.acces)[2],sep=""))
  rownames(result.mean.acces)<-rep(paste("sol",1:dim(result.mean.acces)[1],sep=""))
  
  result.scale <- result[pos,]
  result.scale <- cbind("solutions" = pos,result.scale)
  
  
  ##12)  PCA of optimal solutions Present the set of solutions in a PCA   
  
  ##Data for analysis pca 
  
  if(dim(result.scale)[2] == 2){
    cat("\n")
    cat("\n")
    cat("You can not do principal component analysis because it only has one objective function")
    cat("\n")
    cat("\n")
  }else if(dim(result.scale)[2] > 2){
    label.row <- as.character(result.scale[,1])
    label.row <- label.row[1:nfinal]
    result.scale.pca <- result.scale[1:nfinal,-1]
    row.names(result.scale.pca) <- label.row 
    
    ##Export data for PCA analysis
    write.csv(result.scale, file = paste(getwd(),"/Results/HighestStandardizedValuesOfSubset_PCA.csv", sep=""), row.names = FALSE)
    
    
    cat("\n")
    cat("\n")
    cat("Scale result for PCA analysis")
    cat("\n")
    print(result.scale.pca)
    
    ##Define object PCA
    pca <- NA
    
    if(dim(result.scale.pca)[2] == 2){ 
      pca <- dudi.pca(result.scale.pca , scannf = F, nf = 2, center = FALSE, scale = FALSE)
      
      DialogBox(paste("The graphics should be saved in",getwd(),"/Results"))
      ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
      
      png("Results/Optimal_solution_PCA_1_Vs_PCA_2.png", width = 2000, height = 1000, res = NA)
      s.label(pca$li,sub=paste("PCA 1: ",round(pca$eig[1]/sum(pca$eig)*100,2),"% - ","PCA 2: ",round(pca$eig[2]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 1, yax = 2, clabel=1.5)
      s.corcircle(pca$co, possub= "topright", xax = 1, yax = 2, add.plot=T, clabel=1.5)
      dev.off()
      
      cat("\n")
      cat("\n")
      cat("List of solutions \n")
      print(as.numeric(label.row))
    }else if(dim(result.scale.pca)[2] > 2){
      pca <- dudi.pca(result.scale.pca , scannf = F, nf = 3, center = FALSE, scale = FALSE)
      
      DialogBox(paste("The graphics should be saved in",getwd(),"/Results"))
      ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
      
      png("Results/Optimal_solution_PCA_1_Vs_PCA_2.png", width = 2000, height = 1000, res = NA)
      s.label(pca$li,sub=paste("PCA 1: ",round(pca$eig[1]/sum(pca$eig)*100,2),"% - ","PCA 2: ",round(pca$eig[2]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 1, yax = 2, clabel = 1.5)
      s.corcircle(pca$co, possub= "topright", xax = 1, yax = 2, add.plot=T, clabel = 1.5)
      dev.off()  
      
      png("Results/Optimal_solution_PCA_1_Vs_PCA_3.png", width = 2000, height = 1000, res = NA)
      s.label(pca$li,sub=paste("PCA 1: ",round(pca$eig[1]/sum(pca$eig)*100,2),"% - ","PCA 3: ",round(pca$eig[3]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 1, yax = 3, clabel = 1.5)
      s.corcircle(pca$co, possub= "topright", xax = 1, yax = 3, add.plot = T, clabel = 1.5)
      dev.off()  
      
      png("Results/Optimal_solution_PCA_2_Vs_PCA_3.png", width = 2000, height = 1000, res = NA)
      s.label(pca$li,sub=paste("PCA 2: ",round(pca$eig[2]/sum(pca$eig)*100,2),"% - ","PCA 3: ",round(pca$eig[3]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 2, yax = 3, clabel = 1.5)
      s.corcircle(pca$co, possub= "topright", xax = 2, yax = 3, add.plot = T, clabel = 1.5)
      dev.off()  
      
      cat("\n")
      cat("\n")
      cat("List of solutions \n")
      print(as.numeric(label.row))
    }
    
    ## Progress bar for choose soluction
    progress <- 10
    ProgressBar <- winProgressBar(title = "Progress bar", min = 0, max = progress, width = 500)
    for(i in 1:progress){
      Sys.sleep(0.5)
      setWinProgressBar(ProgressBar, i, title = paste(round(i/progress*100, 0), "% progress"))
    }  
    close(ProgressBar)
    
    
    
    nsol.pca <- SelectSolution(label.row)
    nsol.pca <-  as.numeric(svalue(nsol.pca))
    
    cat("\n")
    
    print(nsol.pca)
    
    
    
    
    if(any(dir("Results") == "Data.Thresholds.csv") == TRUE){
      Data.Thresholds <- read.csv(paste(getwd(),"/Results/Data.Thresholds.csv", sep=""))
      DataFinal <- Data.Thresholds
    }else if(any(dir("Results") == "Data.Thresholds.csv") == FALSE){
      DataFinal <- object
    } 
    
    final.subset.pca <- DataFinal[is.element(DataFinal$n_acces,mean.result$accessions[nsol.pca,]),]
    
    
    ##Selection of preferred set by PCA   
    cat("\n")
    cat("\n")
    cat("#Subset of optimal solution by PCA#")
    cat("\n")
    cat("\n")
    print(final.subset.pca) 
    cat("\n")
    cat("\n")
    DialogBox(paste("The results should be saved in",getwd(),"/Results"))
    ifelse(file.exists("Results") == "FALSE", dir.create("Results"), "Folder already exists 'Results' ")
    write.csv(final.subset.pca, file = paste(getwd(),"/Results/subset_optimal_solution_by_final_subset_PCA","_Solution(",nsol.pca,")",".csv", sep=""),
              row.names = FALSE)
    
    cat("\n")
    cat("\n")
    cat(paste("Process completed.................")) 
    
  }
  
  
  
  
  
}  

WSM.type.opt <- function(output.opt0){
  
  Nsim <- as.numeric(svalue(Nsim))
  npercen <- as.numeric(svalue(npercen))
  num.access <- as.numeric(svalue(num.access))
  object <- eval(parse(text = svalue(nom_data)))
  
  ## Read ranking 
  RI <- read.csv(paste(getwd(),"/Results/RI.csv", sep=""))
  
  
  ri1c <- RI[1,1]
  ri2c <- RI[2,1]
  ri3c <- RI[3,1]
  ri4c <- RI[4,1]
  ri5c <- RI[5,1]
  ri1n <- RI[1,2]
  ri2n <- RI[2,2]
  ri3n <- RI[3,2]
  ri4n <- RI[4,2]
  ri5n <- RI[5,2]
  
  ##Weights
  weight1c <- ri1c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight2c <- ri2c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight3c <- ri3c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight4c <- ri4c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight5c <- ri5c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight1n <- ri1n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight2n <- ri2n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight3n <- ri3n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight4n <- ri4n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight5n <- ri5n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  
  
  weights <- c(weight1c, weight2c, weight3c, 
               weight4c, weight5c, weight1n,
               weight2n, weight3n, weight4n, weight5n)
  
    
  ##Exclude zero-valued weights
  weights <- weights[weights != 0]
  cat("\n")
  cat("Weights:")
  cat("\n")
  print(round(weights,2))
  cat("\n")
  cat("\n")
  
  
  ##8)  Standardize values in the sampled subsets:   
  result <- apply(t(sapply(output.opt0, "[[", 1)), MARGIN = 2, FUN = scale)
  colnames(result) <- as.character(output.opt0[[1]]$names)
  #label.col <- as.character(output.opt0[[1]]$names)
  
  ##Drop columns with NA's
  result <- as.data.frame(result)
  result <- result[sapply(result, function(x) !all(is.na(x)))]
  
  ##9)  Calculation of mean value of the objective functions for each subset   
  mean.result <- list("standardized.means" = apply(result, MARGIN = 1, FUN = mean),"accessions"=t(sapply(output.opt0, "[[", 2)))
  
  
  ##10)  Selection of solutions with highest standardized mean values (1%) ##seleccionar el %
  pos <- order(mean.result[[1]],decreasing=T)[1:(Nsim*(npercen/100))] #save postion of solution with highest standardized mean values 
  
  cat("\n")
  result.mean.acces <- mean.result$accessions[pos,]
  colnames(result.mean.acces)<-rep(paste("acces",1:dim(result.mean.acces)[2],sep=""))
  rownames(result.mean.acces)<-rep(paste("sol",1:dim(result.mean.acces)[1],sep=""))
  
  paste("SubsetOfAccessionsWith",npercen,"%","HighestStandardizedMeanValues",sep="")
  cat(paste("#Subset of accessions with ",npercen,"%"," highest standardized mean values#",sep=""))
  cat("\n")
  print(result.mean.acces)
  cat("\n")
  cat("\n")
  
  
  cat("\n")
  result.scale <- result[pos,]
  
  result.scale <- cbind("solutions" = pos,result.scale)
  cat("#Highest standardized values of subset#")
  cat("\n")
  cat("\n")
  print(result.scale)
  cat("\n")
  cat("\n")
  
  write.csv(result.scale, file = paste(getwd(),"/Results/HighestStandardizedValuesOfSubset_WSM.csv", sep=""),
            row.names = FALSE)
  
  result.scale.values <- as.matrix(abs(result.scale[,-1]))
  weights <- as.matrix(weights, ncol = 1)
  WSM <- as.numeric(result.scale.values%*%weights) #Compute WSM
  
  WSM.df <- data.frame(cbind(result.scale[,1], WSM))
  colnames(WSM.df) <- c("Solution", "WSM.score")
  
  nsol.wsm <-  WSM.df[which.max(WSM.df$WSM.score),1]
  
  cat("#Weighted sum model#")
  cat("\n")
  print(WSM.df)
  
  cat("\n")
  cat("\n")
  
  cat(paste("Solution by WSM: ", nsol.wsm, sep = ""))
  
  if(any(dir("Results") == "Data.Thresholds.csv") == TRUE){
    Data.Thresholds <- read.csv(paste(getwd(),"/Results/Data.Thresholds.csv", sep=""))
    DataFinal <- Data.Thresholds
  }else if(any(dir("Results") == "Data.Thresholds.csv") == FALSE){
    DataFinal <- object
  } 
  
  final.subset.wsm <- DataFinal[is.element(DataFinal$n_acces,mean.result$accessions[nsol.wsm,]),]
  
  
  ##Selection of preferred set by WSM  
  cat("\n")
  cat("\n")
  cat("#Subset of optimal solution by WSM#")
  cat("\n")
  cat("\n")
  
  print(final.subset.wsm) 
  cat("\n")
  cat("\n")
  DialogBox(paste("The results should be saved in",getwd(),"/Results"))
  ifelse(file.exists("Results") == "FALSE", dir.create("Results"), "Folder already exists 'Results' ")
  write.csv(final.subset.wsm , file = paste(getwd(),"/Results/subset_optimal_solution_by_final_subset_WSM","_Solution(",nsol.wsm,")",".csv", sep=""),
            row.names = FALSE)
  
  cat("\n")
  cat("\n")
  cat(paste("Process completed.................")) 
}


## Cluster Function
fcluster <- function(Data.acces, data.mean.result, data.optimization){
  
  
  
  row.names.cluster <- data.optimization[,1]
  data.optimization <- data.optimization[,-1]
  distance <- dist(data.optimization, method = "euclidean")
  dis.matrix <- as.matrix(distance) 
  
  k.means <- kmeans(dis.matrix,2)
  data.cluster <- cbind("solutions" = row.names.cluster, "cluster" = k.means$cluster, data.optimization)
  data.cluster.sol1 <- data.cluster[data.cluster$cluster == 1,]
  data.cluster.sol2 <- data.cluster[data.cluster$cluster == 2,]
  data.cluster.sol1 <- data.cluster.sol1[,-2]
  data.cluster.sol2 <- data.cluster.sol2[,-2]
  
  solution1 <- data.cluster.sol1[which.max(data.cluster.sol1$WSM.score),1]
  solution2 <- data.cluster.sol2[which.min(data.cluster.sol2$WSM.score),1]
  
  
  Data.access1 <- Data.acces[is.element(Data.acces$n_acces, data.mean.result$accessions[solution1,]),]
  Data.access2 <- Data.acces[is.element(Data.acces$n_acces, data.mean.result$accessions[solution2,]),]
  
  
  result <- list("solution.1" = solution1, "solution.2" = solution2,
                 "Data.access.sol1" = Data.access1, "Data.access.sol2" = Data.access2,
                 "Data.cluster.sol1" = data.cluster.sol1, "Data.cluster.sol2" = data.cluster.sol2)
  
  return(result)
}


TableChart <- function(out.solution, i){
  
  Table.Results <- as.data.frame(cbind(round(apply(out.solution$Data.cluster.sol1[,-1],2,median),2),
                                       round(apply(out.solution$Data.cluster.sol2[,-1],2,median),2)))
  
  row.names(Table.Results) <- paste("Median ", row.names(Table.Results), sep = "")
  
  Table.Results <- rbind("n" = c(dim(out.solution$Data.cluster.sol1)[1],
                                 dim(out.solution$Data.cluster.sol2)[1]),
                         "Solution" = c(out.solution$solution.1, out.solution$solution.2), Table.Results)
  
  colnames(Table.Results) <- c("Cluster 1", "Cluster 2")
  
  windows()
  grid.text(paste("Step ",i, "\n", sep = ""),gp=gpar(fontsize=30),0.5,0.9)
  grid.draw(tableGrob(Table.Results))
  
  png(paste("Results/Summary_Cluster1_Cluster2_Step",i,".png",sep=""))
  grid.text(paste("Step ",i, "\n", sep = ""),gp=gpar(fontsize=30),0.5,0.9)
  grid.draw(tableGrob(Table.Results))
  dev.off()
}

DTree.type.opt <- function(output.opt0){
  
  Nsim <- as.numeric(svalue(Nsim))
  npercen <- as.numeric(svalue(npercen))
  num.access <- as.numeric(svalue(num.access))
  object <- eval(parse(text = svalue(nom_data)))
  
  ## Read ranking importance 
  RI <- read.csv(paste(getwd(),"/Results/RI.csv", sep=""))
  
  ri1c <- RI[1,1]
  ri2c <- RI[2,1]
  ri3c <- RI[3,1]
  ri4c <- RI[4,1]
  ri5c <- RI[5,1]
  ri1n <- RI[1,2]
  ri2n <- RI[2,2]
  ri3n <- RI[3,2]
  ri4n <- RI[4,2]
  ri5n <- RI[5,2]
  
  
  ##Weights
  weight1c <- ri1c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight2c <- ri2c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight3c <- ri3c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight4c <- ri4c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight5c <- ri5c/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight1n <- ri1n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight2n <- ri2n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight3n <- ri3n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight4n <- ri4n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  weight5n <- ri5n/sum(ri1c, ri2c, ri3c, ri4c, ri5c, ri1n, ri2n, ri3n, ri4n, ri5n)
  
  
  weights <- c(weight1c, weight2c, weight3c, 
               weight4c, weight5c, weight1n,
               weight2n, weight3n, weight4n, weight5n)
  
  
  
  ##Exclude zero-valued weights
  weights <- weights[weights != 0]
  
  
  ##8)  Standardize values in the sampled subsets:   
  result <- apply(t(sapply(output.opt0, "[[", 1)), MARGIN = 2, FUN = scale)
  colnames(result) <- as.character(output.opt0[[1]]$names)
  
  
  ##Drop columns with NA's
  result <- as.data.frame(result)
  result <- result[sapply(result, function(x) !all(is.na(x)))]
  
  ##9)  Calculation of mean value of the objective functions for each subset   
  mean.result <- list("standardized.means" = apply(result, MARGIN = 1, FUN = mean),"accessions"=t(sapply(output.opt0, "[[", 2)))
  
  
  ##10)  Selection of solutions with highest standardized mean values (1%) ##seleccionar el %
  pos <- order(mean.result[[1]],decreasing=T)[1:(Nsim*(npercen/100))] #save postion of solution with highest standardized mean values 
  
  cat("\n")
  result.mean.acces <- mean.result$accessions[pos,]
  colnames(result.mean.acces)<-rep(paste("acces",1:dim(result.mean.acces)[2],sep=""))
  rownames(result.mean.acces)<-rep(paste("sol",1:dim(result.mean.acces)[1],sep=""))
  
  result.scale <- result[pos,]
  result.scale <- cbind("solutions" = pos, result.scale)
  
  if(dim(result.scale)[2] == 2){
    cat("\n")
    cat("\n")
    cat("You can not do Decision tree analysis because it only has one objective function")
    cat("\n")
    cat("\n")
  }else if(dim(result.scale)[2] > 2){
    result.scale.values <- as.matrix(abs(result.scale[,-1]))
    cat("\n")
    print("Weights: ")
    print(round(weights,2))
    print(result.scale.values)
    
    weights <- as.matrix(weights, ncol = 1)
    WSM <- as.numeric(result.scale.values%*%weights) #Compute WSM
    
    WSM.df <- data.frame(cbind(result.scale[,1], WSM))
    colnames(WSM.df) <- c("Solution", "WSM.score")
    
    
    
    
    #Data for Cluster analysis
    data.HighestStandardizedValues <- result.scale
    data.HighestStandardizedValues  <- cbind(data.HighestStandardizedValues , "WSM.score" = WSM.df$WSM.score)
    row.names.cluster <- data.HighestStandardizedValues[,1]
    row.names(data.HighestStandardizedValues) <- row.names.cluster
    
    #step 1
    out.solution.tree <- list()
    out.solution.tree[[1]] <- fcluster(object, mean.result, data.HighestStandardizedValues)
    TableChart(out.solution.tree[[1]],1) 
    
    
    ## Initialized values
    Decision <- NA
    solution <- character()
    
    Decision <- ginput("Choose the cluster (1 or 2)",text="0", title="ginput", icon="question")
    Decision <- as.numeric(svalue(Decision))
    
    solution[1] <- as.character(out.solution.tree[[1]][Decision])
    items.choose <- c("Yes", "No")
    
    ## Initialized loop
    w <- 2
    
    while(dim(ldply (out.solution.tree[[w-1]][Decision+4], data.frame)[,-1])[1] > 10 ){
      
      out.solution.tree[[w]] <- fcluster(ldply(out.solution.tree[[w-1]][Decision+2], data.frame)[,-1], 
                                         mean.result, ldply(out.solution.tree[[w-1]][Decision+4], data.frame)[,-1])
      
      choose <- DialogBoxDTree(items.choose)
      
      graphics.off() 
      if(choose == "Yes"){
        TableChart(out.solution.tree[[w]],w) 
        Decision <- NA
        Decision <- ginput("Choose the cluster (1 or 2)",text="0", title="ginput", icon="question")
        Decision <- as.numeric(svalue(Decision))
        solution[w] <- as.character(out.solution.tree[[w]][Decision])
      }else if(choose == "No"){break}
      
      if(dim(ldply (out.solution.tree[[w-1]][Decision+4], data.frame)[,-1])[1] <= 10 ){break}
      
      w <- w + 1 
      
      
    }
    
    DialogBox(paste("The graphics should be saved in",getwd(),"/Results"))
    ifelse(file.exists("Results")=="FALSE",dir.create("Results"),"Folder already exists 'Results' ")
    
    nsol.DTree <- as.numeric(solution[length(solution)])
    cat("\n")
    cat("\n")
    
    cat(paste("Solution by Decision Tree: ", nsol.DTree, sep = ""))
    
    if(any(dir("Results") == "Data.Thresholds.csv") == TRUE){
      Data.Thresholds <- read.csv(paste(getwd(),"/Results/Data.Thresholds.csv", sep=""))
      DataFinal <- Data.Thresholds
    }else if(any(dir("Results") == "Data.Thresholds.csv") == FALSE){
      DataFinal <- object
    } 
    
    
    final.subset.DTree <- DataFinal[is.element(DataFinal$n_acces,mean.result$accessions[nsol.DTree,]),]
    
    ##Selection of preferred set by Decision Tree 
    cat("\n")
    cat("\n")
    cat("#Subset of optimal solution by Decision Tree #")
    cat("\n")
    cat("\n")
    
    print(final.subset.DTree) 
    cat("\n")
    cat("\n")
    DialogBox(paste("The results should be saved in",getwd(),"/Results"))
    ifelse(file.exists("Results") == "FALSE", dir.create("Results"), "Folder already exists 'Results' ")
    write.csv(final.subset.DTree , file = paste(getwd(),"/Results/subset_optimal_solution_by_final_subset_DTree","_Solution(",nsol.DTree,")",".csv", sep=""),
              row.names = FALSE)
    
    cat("\n")
    cat("\n")
    cat(paste("Process completed.................")) 
    
  }
}


## Delete auxiliary files
if(file.exists(paste(getwd(),"/Results/RI.csv", sep = "")) == TRUE){
  unlink(paste(getwd(),"/Results/RI.csv", sep = ""))
}  


##GUI---------------------------------------------------------------------------------------------------------------------------

## Principal window
win <- gwindow("Aplication Bioversity International", visible = F , width = 500, height = 300) 
nb <- gnotebook(cont = win, expand = T, tab.pos = 2)


##Load dataset
lyt1 = glayout(homogeneous = F, cont = nb, spacing = 1, label = "Load dataset", expand = T) 
lyt1[1,1:3] = (g = gframe("Load dataset", cont = lyt1, horizontal = T))
lytgb1 = glayout(homogeneous = F,cont = g, spacing = 1, expand = T) 
lytgb1[1,1] = (glabel = (""))
lytgb1[2,1] = (h = gbutton("Change directory", cont = lytgb1, handler = function(h,...)setwd(gfile(text = "Select directory", type = "selectdir"))))
lytgb1[3,1] = (glabel = (""))
lytgb1[4,1] = gbutton("Data set", cont = lytgb1, expand = F, handler = function(h,...){data_set<<-load_dataset()})



##Descriptive analysis
lyt2 = glayout(homogeneous = F, cont = nb, spacing = 1, label = "Descriptive analysis", expand = T)
lyt2[1,1:6] = (g1 <- gframe("Descriptive analysis",cont=lyt2,expand=T,horizontal=F))
lytg2 = glayout(homogeneous = F, cont = g1, spacing = 1, expand = T) 
lytg2[1,1] = glabel("Dataset for analized  ",cont=lytg2)
lytg2[1,2] = (nom_data = gdroplist(c("data_set"), selected = 0, cont = lytg2, expand = T, handler = function(h,...){attach(eval(parse(text=svalue(h$obj))))}))
lytgb1[3,1] = (glabel = (""))
lytg2[4,1] = glabel("Number of continuos variables: ", cont = lytg2)
lytg2[4,2] = (ncon <-gedit("",cont=lyt2,width = 10,initial.msg="")) 
lytg2[5,1] = glabel("Number of categorical variables: ",cont=lytg2)
lytg2[5,2] = (ncat <-gedit("",cont=lyt2,width = 10,initial.msg=""))


lytg2[6,1]=(glabel=(""))
lytg2[7,1]=(glabel=(""))

lytg2[9,1] = gbutton("Descriptive analysis for continuous variables",cont=lytg2, handler=function(h,...){print(descriptives.continuos(eval(parse(text=svalue(nom_data)))))})
lytg2[10,1] = gbutton("Descriptive analysis for nominal variables",cont=lytg2, handler=function(h,...){print(descriptives.nominal(eval(parse(text=svalue(nom_data)))))})

lytg2[11,1]=(glabel=(""))
lytg2[12,1]=(glabel=(""))

lytg2[13,1] = glabel("Level of correlation: ",cont=lytg2)
lytg2[13,2] = (ncor <- gspinbutton(from=0, to = 1, by = 0.1, value=0, cont=lytg2)) 
lytg2[14,1] = gbutton("Correlation analysis",cont=lytg2, handler=function(h,...){print(correlation(eval(parse(text=svalue(nom_data)))))})

lytg2[15,1]=(glabel=(""))
lytg2[16,1]=(glabel=(""))

lytg2[17,1] = glabel("Number accessions final dataset: ", cont = lytg2)
lytg2[17,2] = (num.access <- gedit("10", cont = lyt2, width = 10, initial.msg =" "))
lytg2[18,1] = gbutton("Select number accessions", cont = lyt2, expand=F,
                      handler = function(h,...){print(number.access())})

lytg2[19,1]=(glabel=(""))
lytg2[20,1]=(glabel=(""))

lytg2[21,1] = glabel("Threholds analysis: ", cont = lytg2)
lytg2[22,1] = gbutton("Select variables", cont = lytg2, handler = function(h,...){DialogSelectThreholds(eval(parse(text=svalue(nom_data))))})


##Selection of preferred analysis
lyt5 = glayout(homogeneous = F,cont = nb , spacing=1,label="Selection of preferred analysis",expand=T)
lyt5[1,1:10] = (g5 = gframe("Type selection of preferred",cont = lyt5, expand = T, horizontal = F))
lytg5 = glayout(homogeneous = F, cont = g5, spacing = 1, expand = T) 

lytg5[1,1] = glabel("Enter the number of solutions: ", cont = lytg5)
lytg5[1,2] = (Nsim = gedit("10000", cont = lytg5))
lytg5[2,1] = gbutton("Select the number of solutions", cont = lytg5, expand=F,
                     handler = function(h,...){print(number.solution())})

lytg5[3,1] = (glabel=(""))
lytg5[4,1] = (glabel=(""))

lytg5[5,1] = glabel("Optimization analysis: ", cont = lytg5)
lytg5[6,1] = gbutton("Select variables", cont = lytg5, expand=F,
                     handler = function(h,...){DialogSelectOptimization(eval(parse(text=svalue(nom_data))))})

lytg5[7,1] = (glabel=(""))
lytg5[8,1] = (glabel=(""))

lytg5[9,1] = glabel("Enter the percentage of solutions (%):", cont = lytg5)
lytg5[9,2] = (npercen = gedit("1", cont = lytg5))
lytg5[10,1] = gbutton("Select the percentage of solutions", cont = lytg5, expand=F,
                      handler = function(h,...){print(number.percent())})

lytg5[11,1] = (glabel=(""))
lytg5[12,1] = (glabel=(""))

lytg5[13,1] = glabel("Enter the number of final solutions for \n (Maximum variation or Principal components) :", cont = lytg5)
lytg5[13,2] = (nfinal = gedit("10", cont = lytg5))
lytg5[14,1] = gbutton("Select the number of final solutions", cont = lytg5, expand=F,
                      handler = function(h,...){print(number.final())})

lytg5[15,1] = (glabel=(""))
lytg5[16,1] = (glabel=(""))

lytg5[17,1] = glabel("Select the type selection of preferred: ", cont = lytg5, horizontal = F)

items.option <- c(" ", "Maximum variation", "Principal components",
                  "Weighted sum model", "Decision tree")

lytg5[18,1] = (option.preferred <- gdroplist(items.option, cont = lytg5))
lytg5[18,2] = (btn <- gbutton("Run", cont = lytg5))

addHandlerChanged(btn, handler = function(h,...){
  if(svalue(option.preferred) == "Maximum variation"){MAXVAR.type.opt(output.opt)}
  if(svalue(option.preferred) == "Principal components"){PCA.type.opt(output.opt)}
  if(svalue(option.preferred) == "Weighted sum model"){WSM.type.opt(output.opt)}
  if(svalue(option.preferred) == "Decision tree"){DTree.type.opt(output.opt)}
})



## Principal windows
gwelcome = ggroup(cont=nb,horizontal = F,label="About application")
gimage("LogoAplication.png", dirname = getwd(), size = "button", cont = gwelcome) 

visible(win) <- TRUE 

