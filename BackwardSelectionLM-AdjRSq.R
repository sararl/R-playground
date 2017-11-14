#####################################################
### backward selection based on R2ajusted

#
# INPUT: values: string vector with predictors names
#        dependantValue: string
#        dataset
# OUTPUT: predictors names resultant of backward selection based on R2adjusted

BackwardPredictorsSelectionAdjR2<-function(values,dependantValue,dataset){
  
  # AdjRsq for all predictors
  model<-lm(as.formula(paste(dependantValue," ~ ",values)),dataset) 
  adjustedRSquaredorig<-summary(model)$adj.r.squared
  adjustedRSquared<-adjustedRSquaredorig
  valuesPredictorsorig<-values
  
  # 
  while(adjustedRSquared>=adjustedRSquaredorig){
    adjustedRsCollection<-RemoveOnePredictorComputeR2(dependantValue,values,dataset)
    varToRemove<-names(adjustedRsCollection)[adjustedRsCollection==max(adjustedRsCollection)]
    
    # Update values
    values<-values[varToRemove != values]
    adjustedRSquared<-max(adjustedRsCollection)
    if(adjustedRSquared>=adjustedRSquaredorig){
      adjustedRSquaredorig = adjustedRSquared
      valuesPredictorsorig = values
    }
  }
  return(valuesPredictorsorig)
}


# Compute the adjRsquared of the linear models resultant of 
# removing one by one a variable in predictors. 
# Return all adjRsquared values with the name of the removed predictor. 
RemoveOnePredictorComputeR2<-function(dependantValue,predictors,dataset){
  adjustedRsCollection<-NULL
  for(i in predictors){
    #print(i)
    valuesI<-predictors[i != predictors]
    valuesTotal<-paste(valuesI,collapse =" + ")
    formul<- paste(dependantValue," ~ ",valuesTotal)
    model<-lm(as.formula(formul),dataset) # str(model) str(summary(model))
    adjustedRSquared<-summary(model)$adj.r.squared
    adjustedRsCollection<-c(adjustedRsCollection,adjustedRSquared)
  }
  names(adjustedRsCollection)<-predictors
  return(adjustedRsCollection)
}

# test
#values<-c("tract" , "zn" , "indus" , "rm" , "age" , "dis" , "rad" , "tax", "ptratio" , "b" , "lstat")
#BackwardPredictorsSelectionAdjR2(values,"cmedv",boston)
# "rm"      "dis"     "rad"     "tax"     "ptratio" "b"       "lstat"  
