require(e1071)

########################################################################################################
# Function to compute statistical measures  
# Input: 
#       x: numeric vector
# Output: 
#       result: vector with statistical measures
#               "Count","Number of NAs","Porc of NAs","Minimum","Q1","Median","Q3","Maximum",
#               "Inferior Whisker","Superior Whisker", "IQ","mean","mode","variance",
#               "standarDeviation","coef.var Pearson","coef.Asimetry","TypeSkwedDistribution",
#               "coef.Kurtosis","Number Outliers")
#
# Dependencies: 
#             library (e1071)
#             Functions: ModeFunction,detectOutliers
#
########################################################################################################
StatsNumericColumn<-function(x){
  
  # Number values
  CountValues<-length(x)
  
  # 0- Detect NA values
  NumberOfNAs<-sum(is.na(x))
  NumberofData<-length(x)
  PorcNAS<-NumberOfNAs/NumberofData
  
  # 1-Minimum, 1st Q, median, 3rd Q, y maximum
  Result5num<-fivenum(x)
  
  # 2- Whiskers (Q3+1.5IQR y Q1-1.5QR)
  WhiInf<-boxplot.stats(x)$stats[1]
  WhiSup<-boxplot.stats(x)$stats[5]
  # 3- IQR
  IQ<-Result5num[4]-Result5num[2]
  
  # 4-Median
  meanValue<-mean(x)
  
  # 5-Mode
  # por si existe más de una moda pegare las dos una junto a otra
  mode<-paste(ModeFunction(x),collapse = ",")
  
  # 6- Varianza
  variance<-var(x)
  
  # 7- Desviación típica
  desv<-sd(x)
  
  # 8- Coeficiente variación Pearson
  coefVarPear<-desv/abs(meanValue)
  
  # 9- Coeficiente de asimetría
  coefSim<-skewness(x)
  
  typeSke<-ifelse(meanValue>Result5num[3],"right","left")
  
  # 10- Coeficiente de curtosis 
  coefCurto<-kurtosis(x)
  
  # 11- Number of Outliers
  numOutliers<-apply(detectOutliers(x),2,sum)
  
  # Aglutino todos los estadísticos
  results<-c(CountValues,NumberOfNAs,PorcNAS,Result5num,WhiInf,WhiSup,IQ,meanValue,mode,variance,desv,coefVarPear,coefSim,typeSke,coefCurto,numOutliers)
  names(results)<-c("Count","Number of NAs","Porc of NAs","Minimum","Q1","Median","Q3","Maximum","Inferior Whisker","Superior Whisker",
                    "IQ","mean","mode","variance","standarDeviation","coef.var Pearson","coef.Asimetry","TypeSkwedDistribution","coef.Kurtosis","Number Outliers")
  
  return(results)
  
}

########################################################################################################
# Mode function
# Input: numeric vector
# Output: mode
########################################################################################################

ModeFunction<-function(x){
  
  frequencies<-table(x)
  mode<-frequencies[frequencies==max(frequencies)]
  
  # All values same frequency
  if(length(frequencies) == length(mode)) {mode<-NA}
  
  return(mode)
  
}



########################################################################################################
# Function to detect outliers using IQR or standar deviation method
# over a numeric vector or all numeric columns of a dataframe
# Input: 
#       df: it could be a dataframe or a vector that will be convert to a one-row dataframe
#       method: "IQR" (by default) or "sd"
# Output: 
#       result: boolean dataframe (TRUE is an outlier, FALSE isn't)
########################################################################################################
detectOutliers<-function(df,method = "IQR"){
  
  df<-as.data.frame(df)
  
  columns<-numericColumns(df)
  result <- NULL
  for (col in columns){
    if (method == "sd"){
      # std method
      mu<-mean(df[,col],na.rm = T)
      sigma<-sd(df[,col],na.rm = T)
      outliers<-sapply(df[,col],function(x) x>mu+2*sigma || x< mu-2*sigma) 
    }else if (method == "IQR"){
      Q25<-quantile(df[,col],na.rm = T,c(0.25,0.75))[1] # 25% quartile
      Q75<-quantile(df[,col],na.rm = T,c(0.25,0.75))[2] # 75% quartile
      iqrValue <- IQR(df[,col],na.rm = T)
      outliers<-sapply(df[,col],function(x) x>Q75+1.5*iqrValue || x< Q25-1.5*iqrValue) 
    }else{
      return(0)
    }
    result<-cbind(result,outliers)
  }
  df<-as.data.frame(result)
  names(df)<-columns
  return(df)
}

########################################################################################################
# Function to return name of numeric columns of a dataframe
# Input: 
#       df: it could be a dataframe 
# Output: 
#       result: vector with names of the numeric columns. O if error
########################################################################################################

numericColumns<-function(df){
  res<-NULL
  if(is.data.frame(df)){
    numColbool<-lapply(df,is.numeric)
    numCol<-names(numColbool)[which(numColbool==T)]
    res<-numCol
  }else{
    res<-0
  }  
  return(res)
}
