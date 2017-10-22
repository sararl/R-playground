my_data<-read.csv("https://raw.githubusercontent.com/delcacho/rcourse/master/supermarket.csv")

# 1- Are there NAs?
complete.cases(my_data) # all is True, so there is no NAs 

# Si tuvieramos Nas, 
# y hubiera valores numéricos, lo sustituriamos por la media del resto de datos
# si fuera categórical, el más común

# 3- 

sapply(my_data, class)

apply(my_data,2,mean)
apply(my_data,2,max)
apply(my_data,2,min)
apply(my_data,2,sd)
apply(my_data,2,table)

apply(my_data,2,function(x){ if (is.numeric(x)) sd(x) else NA})
data2<-my_data
#####ESTO NO FUNCIONA
data2$nueva<-rep("hello",30)
apply(data2,2,function(x){ if (is.numeric(x)){sd(x)} else{NA}})
#####


boxplot(my_data$Sales)

boxplot(my_data$Price.Eggs) # aquí ya se ve left skew
plot(density(my_data$Price.Eggs)) # left skew, the length of the tail is longer in left side
# tiene dos jorobas de dromedario, por lo que se llama distribución bimodal

OutliersDetected<-detectOutliers(my_data)
table(OutliersDetected)# Para obtener recuento

# Correlaciones
plot(my_data$Sales,my_data$Price.Eggs)
cor(my_data$Sales,my_data$Price.Eggs)

cor(my_data) # si hay fuerte correlación entre dos variables, quédate solo con una de ellas.


# 
my_data[my_data$Price.Eggs<quantile(my_data$Price.Eggs,0.25) -1.5*IQR(my_data$Price.Eggs),"Price.Eggs"]<-mean(my_data$Price.Eggs)
boxplot(my_data$Price.Eggs) # Para dataset muuy grandes no te va a ocurrir que al corregir
# valores con media se modique tanto los valores de IQR,,etc y por tanto te aparezcan nuevo outliers 



# Elasticity = variation sell/ variation price 
# Si es 1 significa que el precio varia un 10%, las ventas varían un 10%. 
# (dQ*P) / (dP*Q)
# y dQ/dP se obtiene en la regresión lineal 
lm(Sales ~ Price.Eggs, data)

-19.29*mean(my_data$Price.Eggs) /30

plot(lm(Sales~Price.Eggs,data))

detectOutliers<-function(df,method = "IQR"){
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

numericColumns<-function(df){
  res<-NULL
  if(is.data.frame(df)){
    for(icol in colnames(df)){
      if(is.numeric(df[,icol])){
        res<-c(res,icol)
      }
    }
  }else{
    print("Input must be a dataframe")
  }  
  return(res)
}

my_data2<-my_data
my_data2$texto<-rep("hh",30)

