

########################################################################################################
# Function to print all plots related to statiscal analysis of numeric variables
# Input: 
#       df: it could be a dataframe or a vector that will be convert to a one-row dataframe
#       method: "IQR" (by default) or "sd"
# Output: 
#       result: boolean dataframe (TRUE is an outlier, FALSE isn't)
########################################################################################################


plotStudyNumericColumn<-function(df,pathSave){
  
  a<-sapply(names(df),function(x){histogramSave(df,pathSave,x)})
  a<-sapply(names(df),function(x){boxplot2(df,pathSave,x)})
  a<-sapply(names(df),function(x){DensityPlotSave(df,pathSave,x)})
  
}
# ##################################################################################
# Create density plot ans save it in jpeg image
# Dependencies
# library(ggplot2)
# ##################################################################################

library(ggplot2)
DensityPlotSave<-function(df,savePath,nameVariable){
  
  varStudy<-df[,grepl(nameVariable,names(df))]
  ggplot(data = df, aes(varStudy)) +
    geom_density()+
    xlab(paste(nameVariable,sep=" "))+ggtitle(paste("Density plot",nameVariable))
  ggsave(filename=paste0(savePath,"DensityPlot-",nameVariable,".jpeg"), plot=last_plot())
}  
  
# ##################################################################################
# Create histogram ans save it in jpeg image
# Dependencies
# library(ggplot2)
# ##################################################################################

library(ggplot2)
histogramSave<-function(df,savePath,nameVariable){
  
  varStudy<-df[,grepl(nameVariable,names(df))]
  
  # Elegir el ancho del histograma automáticamente
  # nclass.FD--> devuelve numero de clases del histograma
  # pretty(x,n) --> devuelve secuencia de un entorno de n+1 puntos para cubrir todos los valores de x 
  breaks <- pretty(range(varStudy), n = nclass.FD(varStudy), min.n = 1)
  bwidth <- breaks[2]-breaks[1]
  
  histogram<-ggplot(data=df, aes(varStudy))
  histogram+geom_histogram(binwidth=bwidth,color="black")+
    xlab(paste(nameVariable,sep=" "))+ylab("Frequencies")+ggtitle(paste("Histogram",nameVariable))
  
  ggsave(filename=paste0(savePath,"Histogram-",nameVariable,".jpeg"), plot=last_plot())
  
}
# ##################################################################################
# Create boxplot ans save it in jpeg image
# Dependencies
# library(ggplot2)
#
# ##################################################################################

boxplot2<-function(df,savePath,nameVariable){
  
  variable<-df[,grepl(nameVariable,names(df))]
  
  stats=boxplot.stats(variable)$stats
  f=fivenum(variable) # minimo y max los extremos
  stats2<-stats[1]#bigote inferior, 
  stats3<-stats[5]#bigote superior
  
  #boxplot(x,main=y,col="steelblue3")
  p <- ggplot(data=df, aes(x="",y=variable))+ labs(title=paste("Boxplot",nameVariable,sep=" "),
                                                   y = nameVariable)
  p + geom_boxplot() + geom_hline(yintercept = stats[1],color="black")+
    annotate("text", x = 0.85, y = stats[1], label = "INF WHISKER") +
    geom_hline(yintercept = stats[5],color="black")+
    annotate("text", x = 0.85, y = stats[5] , label = "SUP WHISKER") +
    geom_hline(yintercept = f[1],linetype="dashed",color="red")+
    annotate("text", x = 1.25, y = f[1] , label = "MIN") +
    geom_hline(yintercept = f[5],linetype="dashed",color="red")+
    annotate("text", x = 1.25, y = f[5] , label = "MAX") +
    annotate("text", x = 1.25, y = f[3] , label = "MEDIAN") +
    stat_summary(fun.y="mean", geom="point", shape=18, size=5,
                 position=position_dodge(width=0.75), color="blue")
  
  ggsave(filename=paste0(savePath,"Boxplot-",nameVariable,".jpeg"), plot=last_plot())
  
  
}


# ##################################################################################
# 
# Definimos una función para construir con más detalle el diagrama de cajas de cada variable cuantitativa.
# cuando queramos tratar más una región a la vez
# ##################################################################################

boxplot_perFactor<-function(df,savePath,nameVariable,factorColumn){
  
  variable<-df[,grepl(nameVariable,names(df))]
  
  stats=boxplot.stats(variable)$stats
  f=fivenum(variable) # minimo y max los extremos
  stats2<-stats[1]#bigote inferior, 
  stats3<-stats[5]#bigote superior
  
  
  box<-ggplot(data=df, aes_string(x=factorColumn, y=nameVariable))
  box+geom_boxplot(aes_string(fill=factorColumn))+
    labs(title=paste("Comparative Boxplot",nameVariable,sep=" "),
         y = nameVariable)+
     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    stat_summary(fun.y="mean", geom="point", shape=18, size=5,color="blue")
  
  
  ggsave(filename=paste0(savePath,"Comparative_Boxplot-",nameVariable,".jpeg"), plot=last_plot(),
         width = 15 , height = 15, units = "cm")
  
  
}

# https://stats.stackexchange.com/questions/11406/boxplot-with-respect-to-two-factors-using-ggplot2-in-r