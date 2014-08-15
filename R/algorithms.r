#----------------------------------------------------------------------------------------------
#APPLICATION BIOVERSITY                                                                       #
#AUTHOR: JOHANN OSPINA FOR BIOVERSITY, REVISIONS BY RICHARD BRUSKIEWICH @ CROPINFORMATICS.COM #
#VERSION 2.0 - AUGUST-04-2014                                                                 #
#                                                                                             #
#---------------------------------------------------------------------------------------------- 
#' @include configuration.r dialogs.r projects.r

########################################################################
# Not sure how the previously included "cluster" package for 
# Cluster Analysis Extended Rousseeuw et al. is being used 
# in this module since check()does not complain if it is removed...
########################################################################

## Ddescriptor analysis for continuous variables
des.continuous <- function(object){
  n <- length(object)
  average <- round(mean(object, na.rm = TRUE),3)
  variance <- round(var(object, na.rm = TRUE),3)
  Est.Desv <- round(sqrt(variance),3)
  median <- round(as.numeric(quantile(object,probs=0.5, na.rm = TRUE)),3)
  Coef.Var <- round((sqrt(variance)/average)*100,3)
  min <- round(min(object,na.rm = TRUE),3)
  max <- round(max(object,na.rm = TRUE),3)
  NA.Data <-round(sum(is.na(object)),3)
  NA.Percent <- round((NA.Data/length(object))*100,3)
  result <- cbind(n, min, max, average, variance, Est.Desv, median, Coef.Var, NA.Data, NA.Percent)
}

## Used to function 'des.continuous' 
descriptors.continuous <- function(object){
  
  ncon <- as.numeric( svalue( numberOfContinuousVariables(analysis) ))
  object <- object[,-1]
  object <- object[,1:ncon]
  
  results <- sapply(object, des.continuous)
  
  row.names(results) <- c("n","Min","Max","Average","Variance","Est.Desv","Median","CV %","NA","NA %")
  results <- as.table(results)
  names(dimnames(results)) <- c(" ", paste("Variable",svalue( datasetSelector(analysis) )))
  
  saveResults( results, "ResultsDdescriptorAnalysisContinuousVariables" )   	
  
  return(results)
}


des.nominal <- function(object){## Ddescriptor analysis for nominal variables
  n<-length(object)
  category   <- names(table(object)) 
  frequence  <- table(object)
  percentage <- round((table(object)/sum(object))*100,2)
  NA.Data    <- round(sum(is.na(object)),0)
  NA.Percent <- round((NA.Data/length(object))*100,2)
  result     <-cbind(n, category, frequence, percentage, NA.Data, NA.Percent)
}

#' @importFrom plyr ldply

## Used to function 'des.nominal'
descriptors.nominal <- function(object){
  
  ncon <-as.numeric( svalue( numberOfContinuousVariables(analysis) ) )
  ncat <-as.numeric( svalue( numberOfCategoricalVariables(analysis) ) )
  
  object <- object[,-1]
  object <- object[,(ncon+1):dim(object)[2]]
  
  results<-lapply(object, des.nominal)
  results<-ldply(results, data.frame)
  colnames(results) <-c("Variable", "n", "Category", "Freq.Cat","%.Cat", "NA","NA %")
  
  names(dimnames(results)) <- c(" ", paste("Variable", svalue( datasetSelector(analysis) )))
  
  saveResults( results, "ResultsDdescriptorAnalysisNominalVariables", row.names = FALSE )     
  
  return(results)
}

## Correlation analysis
correlation <- function(object){
  
  ncon <-as.numeric( svalue( numberOfContinuousVariables(analysis) ))
  
  object      <- object[,-1]
  object      <- object[,1:ncon]
  correlation <- round(cor(object),2)
  correlation[ lower.tri(correlation,diag = TRUE) ] <- NA 
  correlation <- as.data.frame(as.table(correlation))
  correlation <- na.omit(correlation)        
  direction   <- rep("",times = dim(correlation)[1])
  
  for(i in 1:dim(correlation)[1]){ ## Direction of correlation
    if(correlation[i,3]>=0 & correlation[i,3]<=1)     { direction[i] <- "positive" }
    else if(correlation[i,3]<0 & correlation[i,3]>=-1){ direction[i] <- "negative" }
  }
  
  correlation = cbind(correlation, direction)
  
  colnames(correlation) <- c("Variable 1", "Variable 2", "Correlation", "Direction") 
  names(dimnames(correlation)) <- c(" ", paste("Variable",svalue( datasetSelector(analysis) )))
  
  cat("\n")
  cat("\n")
  
  ncor <-as.numeric(svalue(ncor))
  ncor <- abs(ncor)
  
  if(ncor==0){
    
    saveResults( correlation,  "ResultsCorrelationAnalysisGlobal", row.names = FALSE )     
    
    return(correlation)
    
  }else if(ncor>0){
    
    correlation.ncor <- subset(correlation, abs(correlation$Correlation)>=ncor)
    
    saveResults( correlation.ncor,  paste("ResultsCorrelationAnalysisLevel",ncor,sep=""), row.names = FALSE )     
    
    cat("\n")
    cat("\n")
    cat(paste("#Correlations >= ",ncor,"#\n"))     
    return(correlation.ncor)
  }
  
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
  
  ncon <- as.numeric(svalue( numberOfContinuousVariables(analysis) )) 
  object.optimization <- object[,-1]
  object.continuous <- object.optimization[,1:ncon]
  object.nominal <- object.optimization[,(ncon+1):dim(object.optimization)[2]]
  names.continuous <- names(object.continuous)
  names.nominal <- names(object.nominal)
  nsoln <- as.numeric(svalue( numberOfSolutions(analysis) ))
  
  pcategory.v1 <- 0
  pcategory.v2 <- 0
  pcategory.v3 <- 0
  pcategory.v4 <- 0
  pcategory.v5 <- 0
  
  win <- gbasicdialog("Selection variable for optmization", visible = F , width = 700, height = 450,
                      handler = function(h,...){varop1c<<-svalue(varop1c);varop2c<<-svalue(varop2c);varop3c<<-svalue(varop3c);varop4c<<-svalue(varop4c);varop5c<<-svalue(varop5c);
                                                fvarop1c<<-svalue(fvarop1c);fvarop2c<<-svalue(fvarop2c);fvarop3c<<-svalue(fvarop3c);fvarop4c<<-svalue(fvarop4c);fvarop5c<<-svalue(fvarop5c);
                                                ri1c<<-svalue(ri1c);ri2c<<-svalue(ri2c);ri3c<<-svalue(ri3c);ri4c<<-svalue(ri4c);ri5c<<-svalue(ri5c);
                                                fvarop1n<<-svalue(fvarop1n);fvarop2n<<-svalue(fvarop2n);fvarop3n<<-svalue(fvarop3n);fvarop4n<<-svalue(fvarop4n);fvarop5n<<-svalue(fvarop5n);
                                                ri1n<<-svalue(ri1n);ri2n<<-svalue(ri2n);ri3n<<-svalue(ri3n);ri4n<<-svalue(ri4n);ri5n<<-svalue(ri5n)}) 
  
  nb <- gnotebook( container = win, expand = T, tab.pos = 3)
  
  
  lyt4=glayout(homogeneous = F, container = nb , spacing=1,label="Optimization",expand = TRUE)
  lyt4[1,1:25] = (g4 = gframe("Optimization", container = lyt4, expand = T, horizontal = F))
  lytg4 = glayout(homogeneous = F,  container = g4, spacing = 1, expand = T) 
  lytg4[1,1]=(glabel=(""))
  lytg4[2,1]=(glabel=(""))
  
  lytg4[3,1] = glabel("Optimization of continuous variables: ",  container =lytg4)
  lytg4[4,1]=(glabel=(""))
  lytg4[5,1] = glabel("Variable: ",  container =lytg4)
  
  lytg4[5,2] = glabel("Objective function: ",  container =lytg4)
  lytg4[5,3] = glabel("Ranking of importance (1: Low ; 10: High):",  container =lytg4)
  
  lytg4[6,1] = (varop1c = gdroplist(c("NA", names.continuous),  container = lytg4))
  lytg4[6,2] = (fvarop1c = gdroplist(f.items.cont,  container = lytg4))
  lytg4[6,3] = (ri1c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[7,1] = (varop2c = gdroplist(c("NA", names.continuous),  container = lytg4))
  lytg4[7,2] = (fvarop2c = gdroplist(f.items.cont,  container = lytg4))
  lytg4[7,3] = (ri2c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[8,1] = (varop3c = gdroplist(c("NA", names.continuous),  container = lytg4))
  lytg4[8,2] = (fvarop3c = gdroplist(f.items.cont,  container = lytg4))
  lytg4[8,3] = (ri3c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[9,1] = (varop4c = gdroplist(c("NA", names.continuous),  container = lytg4))
  lytg4[9,2] = (fvarop4c = gdroplist(f.items.cont,  container = lytg4))
  lytg4[9,3] = (ri4c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[10,1] = (varop5c = gdroplist(c("NA", names.continuous),  container = lytg4))
  lytg4[10,2] = (fvarop5c = gdroplist(f.items.cont,  container = lytg4))
  lytg4[10,3] = (ri5c <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  
  
  lytg4[11,1]=(glabel=(""))
  lytg4[12,1]=(glabel=(""))
  
  lytg4[13,1] = glabel("Optimization of nominal variables: ",  container =lytg4)
  lytg4[14,1]=(glabel=(""))
  lytg4[15,1] = glabel("Variable: ",  container =lytg4)
  lytg4[15,2] = glabel("Objective function: ",  container =lytg4)
  lytg4[15,3] = glabel("Ranking of importance (1: Low ; 10: High):",  container =lytg4)
  lytg4[16,1] = (varop1n = gdroplist(c("NA",names.nominal),  container = lytg4))
  lytg4[16,2] = (fvarop1n = gdroplist(f.items.nom,  container = lytg4, handler = function(h,...){
    if(svalue(fvarop1n) == "MAX.PROP: Maximize proportion" & svalue(varop1n) != "NA"){pcategory.v1 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop1n)])))))}}))
  lytg4[16,3] = (ri1n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[17,1] = (varop2n = gdroplist(c("NA",names.nominal),  container = lytg4))
  lytg4[17,2] = (fvarop2n = gdroplist(f.items.nom,  container = lytg4, handler = function(h,...){
    if(svalue(fvarop2n) == "MAX.PROP: Maximize proportion" & svalue(varop2n) != "NA"){pcategory.v2 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop2n)])))))}}))
  lytg4[17,3] = (ri2n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[18,1] = (varop3n = gdroplist(c("NA",names.nominal),  container = lytg4))
  lytg4[18,2] = (fvarop3n = gdroplist(f.items.nom,  container = lytg4, handler = function(h,...){
    if(svalue(fvarop3n) == "MAX.PROP: Maximize proportion" & svalue(varop3n) != "NA"){pcategory.v3 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop3n)])))))}}))
  lytg4[18,3] = (ri3n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[19,1] = (varop4n = gdroplist(c("NA",names.nominal),  container = lytg4))
  lytg4[19,2] = (fvarop4n = gdroplist(f.items.nom,  container = lytg4, handler = function(h,...){
    if(svalue(fvarop4n) == "MAX.PROP: Maximize proportion" & svalue(varop4n) != "NA"){pcategory.v4 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop4n)])))))}}))
  lytg4[19,3] = (ri4n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  lytg4[20,1] = (varop5n = gdroplist(c("NA",names.nominal),  container = lytg4))
  lytg4[20,2] = (fvarop5n = gdroplist(f.items.nom,  container = lytg4, handler = function(h,...){
    if(svalue(fvarop5n) == "MAX.PROP: Maximize proportion" & svalue(varop1n) != "NA"){pcategory.v5 <<- svalue(as.numeric(DialogSelect(names(table(object[colnames(object) == svalue(varop5n)])))))}}))
  lytg4[20,3] = (ri5n <- gspinbutton(from = 1, to = 10, by = 1, value = 0, container = lytg4)) 
  
  lytg4[21,1]=(glabel=(""))
  lytg4[22,1]=(glabel=(""))
  lytg4[23,1]=(glabel=("Note: Run the optimization procedure before close the window"))
  
  
  lytg4[25,2] = gbutton("Optimization",  container = lytg4,
                        handler = function(h,...){
                          optimizationResult(analysis) <<- f.optimization(varop1c, varop2c, varop3c, varop4c, varop5c,
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
  
  saveResults( RI, paste("RI", sep=""), row.names = FALSE )     
}

#' @importFrom vegan diversity

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

#
# This function retrieves any available data threshold values for the current project
#
getDataThresholds <- function() {
  
  if( any( dir( currentProjectFolder(analysis) ) == "Data.Thresholds.csv") == TRUE ){
    
    return( readResults(  "Data.Thresholds" ) )
    
  } else {
    
    return(NA)
  }
}

f.optimization <- function(varop1c, varop2c, varop3c, varop4c, varop5c,
                           fvarop1c, fvarop2c, fvarop3c, fvarop4c, fvarop5c,                           
                           varop1n, varop2n, varop3n, varop4n, varop5n,
                           fvarop1n, fvarop2n, fvarop3n, fvarop4n, fvarop5n,
                           pcategory.v1, pcategory.v2, pcategory.v3, pcategory.v4, pcategory.v5){## load the function "optimization" and integrated into the GUI
  
  object <- eval(parse(text=svalue( datasetSelector(analysis) )))
  
  ncon   <- as.numeric(svalue( numberOfContinuousVariables(analysis) )) 
  ncat   <- as.numeric(svalue( numberOfCategoricalVariables(analysis) ))
  
  nsoln  <- as.numeric(svalue( numberOfSolutions(analysis) )) 
  
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
  
  Data <- getDataThresholds()
  if( is.na(Data) ) { Data <- object } 
  
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
  ProgressBar <- winProgressBar(title = "Progress bar", min = 0, max = nsoln, width = 500)
  for(j in 1:nsoln){
    output.opt0[[j]] <- optimization(Data, option.objective.con, option.objective.nom, num.access)
    setWinProgressBar(ProgressBar, j, title=paste(round(j/nsoln*100, 0), "% progress"))
  }
  close(ProgressBar)
  
  return(output.opt0)
  
}

MAXVAR.type.opt <- function(output.opt0){
  
  nsoln    <- as.numeric( svalue( numberOfSolutions(analysis) ))
  npercent <- as.numeric( svalue( percentageOfSolutions(analysis) ))
  
  nfinal <- as.numeric(svalue(nfinal))
  num.access <- as.numeric(svalue(num.access))
  object <- eval(parse(text=svalue( datasetSelector(analysis) )))
  
  ##8)  Standardize values in the sampled subsets:   
  result <- apply(t(sapply(output.opt0, "[[", 1)), MARGIN = 2, FUN = scale)
  colnames(result) <- as.character(output.opt0[[1]]$names)
  
  ##Drop columns with NA's
  result <- as.data.frame(result)
  result <- result[sapply(result, function(x) !all(is.na(x)))]
  
  ##9)  Calculation of mean value of the objective functions for each subset   
  mean.result <- list("standardized.means" = apply(result, MARGIN = 1, FUN = mean),"accessions"=t(sapply(output.opt0, "[[", 2)))
  
  ##10)  Selection of solutions with highest standardized mean values (1%) ##seleccionar el %
  
  pos <- order(mean.result[[1]],decreasing = TRUE)[1:(nsoln*(npercent/100))] #save postion of solution with highest standardized mean values 
  
  cat("\n")
  result.mean.accessions <- mean.result$accessions[pos,]
  colnames(result.mean.accessions)<-rep(paste("acces",1:dim(result.mean.accessions)[2],sep=""))
  rownames(result.mean.accessions)<-rep(paste("sol",1:dim(result.mean.accessions)[1],sep=""))
  
  paste("SubsetOfAccessionsWith",npercent,"%","HighestStandardizedMeanValues",sep="")
  cat(paste("#Subset of accessions with ",npercent,"%"," highest standardized mean values#",sep=""))
  cat("\n")
  print(result.mean.accessions)
  cat("\n")
  cat("\n")
  
  saveResults( 
    result.mean.accessions, 
    
    paste("SubsetOfAccessionsWith",npercent,"%","HighestStandardizedMeanValues", sep="")
  )     
  
  cat("\n")
  result.scale <- result[pos,]
  result.scale <- cbind("solutions" = pos,result.scale)
  cat("#Highest standardized values of subset#")
  cat("\n")
  cat("\n")
  print(result.scale)
  cat("\n")
  cat("\n")
  
  saveResults( result.scale,  "HighestStandardizedValuesOfSubset_MV", row.names = FALSE )     
  
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
  
  pos.nfinal <- order(variance[,3],decreasing = TRUE)
  pos.max.var <- pos.nfinal[1]
  pos.variance.aux <- sample(unique(as.numeric(c(variance[pos.nfinal,1], variance[pos.nfinal,2]))), (3*num.access))
  
  DataFinal <- getDataThresholds()
  if( is.na(DataFinal) ) { DataFinal <- object } 
  
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
  
  saveResults( 
    final.subset.max.var, 
    
    paste( "subset_optimal_solution_by_MaXVAR_Solution(",pos.max.var,")", sep="" ),
    row.names = FALSE 
  )     
  
}

#' @importFrom ade4 dudi.pca
#' @importFrom ade4 s.label
#' @importFrom ade4 s.corcircle

PCA.type.opt <- function(output.opt0){
  
  nsoln    <- as.numeric( svalue( numberOfSolutions(analysis) ))
  npercent <- as.numeric( svalue( percentageOfSolutions(analysis) ))
  nfinal   <- as.numeric( svalue(nfinal))
  object   <- eval(parse( text = svalue( datasetSelector(analysis) )))
  
  ##8)  Standardize values in the sampled subsets:   
  result <- apply(t(sapply(output.opt0, "[[", 1)), MARGIN = 2, FUN = scale)
  colnames(result) <- as.character(output.opt0[[1]]$names)
  
  ##Drop columns with NA's
  result <- as.data.frame(result)
  result <- result[sapply(result, function(x) !all(is.na(x)))]
  
  mean.result <- list("standardized.means" = apply(result, MARGIN = 1, FUN = mean),"accessions"=t(sapply(output.opt0, "[[", 2)))
  pos <- order(mean.result[[1]],decreasing = TRUE)[1:(nsoln*(npercent/100))] #save postion of solution with highest standardized mean values 
  result.mean.accessions <- mean.result$accessions[pos,]
  colnames(result.mean.accessions)<-rep(paste("acces",1:dim(result.mean.accessions)[2],sep=""))
  rownames(result.mean.accessions)<-rep(paste("sol",1:dim(result.mean.accessions)[1],sep=""))
  
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
    saveResults( result.scale,  "HighestStandardizedValuesOfSubset_PCA", row.names = FALSE )     
    
    cat("\n")
    cat("\n")
    cat("Scale result for PCA analysis")
    cat("\n")
    print(result.scale.pca)
    
    ##Define object PCA
    pca <- NA
    
    if(dim(result.scale.pca)[2] == 2) {
      
      pca <- dudi.pca(result.scale.pca , scannf = F, nf = 2, center = FALSE, scale = FALSE)
      
      plotImage(  "Optimal_solution_PCA_1_Vs_PCA_2" )
      
      s.label(pca$li,sub=paste("PCA 1: ",round(pca$eig[1]/sum(pca$eig)*100,2),"% - ","PCA 2: ",round(pca$eig[2]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 1, yax = 2, clabel=1.5)
      s.corcircle(pca$co, possub= "topright", xax = 1, yax = 2, add.plot = TRUE, clabel=1.5)
      dev.off()
      
      cat("\n")
      cat("\n")
      cat("List of solutions \n")
      print(as.numeric(label.row))
      
    } else if(dim(result.scale.pca)[2] > 2){
      
      pca <- dudi.pca(result.scale.pca , scannf = F, nf = 3, center = FALSE, scale = FALSE)
      
      plotImage(  "Optimal_solution_PCA_1_Vs_PCA_2" )
      
      s.label(pca$li,sub=paste("PCA 1: ",round(pca$eig[1]/sum(pca$eig)*100,2),"% - ","PCA 2: ",round(pca$eig[2]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 1, yax = 2, clabel = 1.5)
      s.corcircle(pca$co, possub= "topright", xax = 1, yax = 2, add.plot = TRUE, clabel = 1.5)
      dev.off()  
      
      plotImage(  "Optimal_solution_PCA_1_Vs_PCA_3" )
      
      s.label(pca$li,sub=paste("PCA 1: ",round(pca$eig[1]/sum(pca$eig)*100,2),"% - ","PCA 3: ",round(pca$eig[3]/sum(pca$eig)*100,2),"%",sep = ""),
              possub= "topright",
              xax = 1, yax = 3, clabel = 1.5)
      s.corcircle(pca$co, possub= "topright", xax = 1, yax = 3, add.plot = T, clabel = 1.5)
      dev.off()  
      
      plotImage(  "Optimal_solution_PCA_2_Vs_PCA_3" )
      
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
    
    DataFinal <- getDataThresholds()
    if( is.na(DataFinal) ) { DataFinal <- object } 
    
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
    
    saveResults( 
      final.subset.pca, 
      
      paste("subset_optimal_solution_by_final_subset_PCA_Solution(",nsol.pca,")", sep=""),
      row.names = FALSE
    )     
    
    cat("\n")
    cat("\n")
    cat(paste("Processing completed.................")) 
    
  }
}  

WSM.type.opt <- function(output.opt0){
  
  nsoln      <- as.numeric( svalue( numberOfSolutions(analysis) ))
  npercent   <- as.numeric( svalue( percentageOfSolutions(analysis) ))
  num.access <- as.numeric( svalue( num.access )) ///
  object     <- currentDataSet(analysis) 
  
  ## Read ranking
  
  RI <- readResults(  "RI" )  
  
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
  pos <- order(mean.result[[1]],decreasing = TRUE)[1:(nsoln*(npercent/100))] #save postion of solution with highest standardized mean values 
  
  cat("\n")
  result.mean.accessions <- mean.result$accessions[pos,]
  colnames(result.mean.accessions)<-rep(paste("acces",1:dim(result.mean.accessions)[2],sep=""))
  rownames(result.mean.accessions)<-rep(paste("sol",1:dim(result.mean.accessions)[1],sep=""))
  
  paste("SubsetOfAccessionsWith",npercent,"%","HighestStandardizedMeanValues",sep="")
  cat(paste("#Subset of accessions with ",npercent,"%"," highest standardized mean values#",sep=""))
  cat("\n")
  print(result.mean.accessions)
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
  
  saveResults( 
    result.scale, 
    
    "HighestStandardizedValuesOfSubset_WSM",
    row.names = FALSE
  )     
  
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
  
  DataFinal <- getDataThresholds()
  if( is.na(DataFinal) ) {DataFinal <- object } 
  
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
  
  saveResults( 
    final.subset.wsm, 
    
    paste("subset_optimal_solution_by_final_subset_WSM_Solution(",nsol.wsm,")", sep=""),
    row.names = FALSE
  ) 
  
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

#' @importFrom grid grid.text
#' @importFrom grid gpar
#' @importFrom grid grid.draw
#' @importFrom gridExtra tableGrob

chart <- function(out.solution, i){
  
  Table.Results <- as.data.frame(cbind(round(apply(out.solution$Data.cluster.sol1[,-1],2,median),2),
                                       round(apply(out.solution$Data.cluster.sol2[,-1],2,median),2)))
  
  row.names(Table.Results) <- paste("Median ", row.names(Table.Results), sep = "")
  
  Table.Results <- rbind("n" = c(dim(out.solution$Data.cluster.sol1)[1],
                                 dim(out.solution$Data.cluster.sol2)[1]),
                         "Solution" = c(out.solution$solution.1, out.solution$solution.2), Table.Results)
  
  colnames(Table.Results) <- c("Cluster 1", "Cluster 2")
  
  grDevices::windows()
  
  grid.text(paste("Step ",i, "\n", sep = ""),gp=gpar(fontsize=30),0.5,0.9)
  grid.draw(tableGrob(Table.Results))
  
  plotImage( paste("Summary_Cluster1_Cluster2_Step",i,sep="") )
  
  grid.text(paste("Step ",i, "\n", sep = ""),gp=gpar(fontsize=30),0.5,0.9)
  grid.draw( tableGrob(Table.Results) )
  dev.off()
}

DTree.type.opt <- function(output.opt0){
  
  nsoln      <- as.numeric(svalue( numberOfSolutions(analysis) ))
  npercent   <- as.numeric(svalue( percentageOfSolutions(analysis) ))
  num.access <- as.numeric(svalue(num.access))
  object     <- currentDataSet(analysis) 
  
  ## Read ranking importance 
  RI <- readResults(  "RI" )  
  
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
  pos <- order(mean.result[[1]],decreasing = TRUE)[1:(nsoln*(npercent/100))] #save postion of solution with highest standardized mean values 
  
  cat("\n")
  result.mean.accessions <- mean.result$accessions[pos,]
  colnames(result.mean.accessions)<-rep(paste("acces",1:dim(result.mean.accessions)[2],sep=""))
  rownames(result.mean.accessions)<-rep(paste("sol",1:dim(result.mean.accessions)[1],sep=""))
  
  result.scale <- result[pos,]
  result.scale <- cbind("solutions" = pos, result.scale)
  
  if(dim(result.scale)[2] == 2){
    cat("\n")
    cat("\n")
    cat("You can not do Decision tree analysis because you only have one objective function")
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
    chart(out.solution.tree[[1]],1) 
    
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
        chart(out.solution.tree[[w]],w) 
        Decision <- NA
        Decision <- ginput("Choose the cluster (1 or 2)",text="0", title="ginput", icon="question")
        Decision <- as.numeric(svalue(Decision))
        solution[w] <- as.character(out.solution.tree[[w]][Decision])
      }else if(choose == "No"){break}
      
      if(dim(ldply (out.solution.tree[[w-1]][Decision+4], data.frame)[,-1])[1] <= 10 ){break}
      
      w <- w + 1 
      
      
    }
    
    nsol.DTree <- as.numeric(solution[length(solution)])
    cat("\n")
    cat("\n")
    
    cat(paste("Solution by Decision Tree: ", nsol.DTree, sep = ""))
    
    DataFinal <- getDataThresholds()
    if( is.na(DataFinal) ) { DataFinal <- object } 
    
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
    
    saveResults( 
      final.subset.DTree,
      paste("subset_optimal_solution_by_final_subset_DTree_Solution(",nsol.DTree,")", sep=""),
      row.names = FALSE
    ) 
    
    cat("\n")
    cat("\n")
    cat(paste("Process completed.................")) 
    
  }
  
}
