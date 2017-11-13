#####################################################
### backward selection based on pvalue

#
# INPUT: predictors: string vector with predictors names
#        dependantValue: string
#        dataset
# OUTPUT: predictors names resultant of backward selection based on pvalue



BackwardPredictorsSelectionpValue<-function(predictors,dependantValue,dataset){
  maxCoef = 1
  while(maxCoef>=0.05){
    PredictorsCollapse<-paste(predictors,collapse =" + ")
    formul<- paste(dependantValue," ~ ",PredictorsCollapse)
    model<-lm(as.formula(formul),dataset)
  
    coefmodel <- data.frame(coef(summary(model)))
    maxCoef<-max(coefmodel$Pr...t..[-1]) # intercept not included
    nameMaxCoef<-rownames(coefmodel)[(coefmodel$Pr...t.. == maxCoef)]
    if(maxCoef>=0.05) predictors<-predictors[predictors != nameMaxCoef]
  }
  return(predictors)
}


# test
#predictors<-c("tract" , "zn" , "indus" , "rm" , "age" , "dis" , "rad" , "tax", "ptratio" , "b" , "lstat")
#dependantValue<-"cmedv"
#BackwardPredictorsSelectionpValue(predictors,dependantValue,boston)
# "rm"      "dis"     "rad"     "tax"     "ptratio" "b"       "lstat"  
