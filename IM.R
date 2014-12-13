# IM - Infuence Model (no auto-weights; diagonals all zero)
# Saima Aman, 2014
# LA: look-ahead intervals: 1,4,8,12,16,20,24,28,32 (8 hrs)
# Data 2013

library(zoo)
library(rpart)

case = 2  # 1 - previous week, 2 - previous day
lookahead = 32
inFolder = "New Code/Cause Matrices/"
if (case == 1){
  outFolder = paste("New Code 2/IM/Errors-zero-diagonal-daily-LA",
                    toString(lookahead),"/",sep="") 
}else{
  outFolder = paste("New Code 2/IM/PrevDay/Errors-zero-diagonal-daily-LA",
                    toString(lookahead),"/",sep="") 
}

year = 2013
springDays = 136 
lag = 4

setwd("C:/SmartGrid-All/")
fNames = scan('Occupancy/sorted-115.csv',sep=",",skip=1,what = list(""))
buildings = paste(fNames[[1]],"_",sep="")
nSeries = length(buildings)   

kwh = readKwh(buildings,year)
temp = readTemp(year)
hmdt = readHmdt(year)
highT = readHighTemp(year)
lowT = readLowTemp(year)
  
for (day in 8:springDays){
  cat("\n Day ",day, "...\n")
  
  today = c(((day-1)*96+1):(day*96))
  kwhData = kwh[,today]
  tempData = temp[today]
  highTemp = highT[today]
  lowTemp = lowT[today]
  hmdtData = hmdt[today]
  
  if (case == 1){
    similarity = day - 7  
  }else{
    similarity = day - 1
  }
  similarDay = c(((similarity-1)*96+1):(similarity*96))
  kwhData2 = kwh[,similarDay]
  tempData2 = temp[similarDay]
  highTemp2 = highT[similarDay]
  lowTemp2 = lowT[similarDay]
  hmdtData2 = hmdt[similarDay]
  
  inFile = paste(inFolder,similarity,"_",year,"_",lag,".csv",sep="")
  adj = scan(inFile,sep=",")
  weights = matrix(adj, nrow = sqrt(length(adj)), sqrt(length(adj)), byrow=TRUE)
  
  #case 4 set all auto-weights to 0
  for(i in 1:nSeries){
    weights[i,i] = 0
  }
   
  mapeVector_bldngs = NULL  
  cvrmseVector_bldngs = NULL
  
  for (k in 1:nSeries){
    cat(k, ",")
    
    # check if weights are all zero; can't predict, err = NA
    sumWeights = sum(weights[k,])  
    if (sumWeights == 0){
      mapeVector_bldngs = c(mapeVector_bldngs,NA)
      cvrmseVector_bldngs = c(cvrmseVector_bldngs,NA) 
      next # can't predict for this series, move to next one
    }
    
    #----------training data
    response2 = putInAMatrix(kwhData2[k,],lag+1)[,(lag+1)]
    response2 = response2[lookahead:length(response2)]
    
    predictors2 = NULL
    # kwh
    for(i in 1:nSeries){
      if (weights[k,i] != 0){
        predictors2 = cbind(predictors2,
                      normalize(putInAMatrix(kwhData2[i,],lag)))
      }
    }
    # weather
    if (weights[k,(nSeries+1)] != 0){
      predictors2 = cbind(predictors2,
                    normalize(putInAMatrix(tempData2,lag)))
    }
    if (weights[k,(nSeries+2)] != 0){
      predictors2 = cbind(predictors2,
                    normalize(putInAMatrix(highTemp2,lag)))
    }
    if (weights[k,(nSeries+3)] != 0){
      predictors2 = cbind(predictors2,
                    normalize(putInAMatrix(lowTemp2,lag)))
    }
    if (weights[k,(nSeries+4)] != 0){
      predictors2 = cbind(predictors2,
                    normalize(putInAMatrix(hmdtData2,lag)))
    }  
    # remove last line from predictors; no response available for that
    predictors2 = predictors2[(1:length(response2)),] 
    
    # put all train data in a frame
    trainData = data.frame(response = response2, predictors = predictors2)
    
    # -------------test data
    response = putInAMatrix(kwhData[k,],lag+1)[,(lag+1)]
    response = response[lookahead:length(response)]
    
    predictors = NULL
    # kwh
    for(i in 1:nSeries){
      if (weights[k,i] != 0){
        predictors = cbind(predictors,
                     normalize(putInAMatrix(kwhData[i,],lag)))
      }
    }
    # weather
    if (weights[k,(nSeries+1)] != 0){
      predictors = cbind(predictors,
                   normalize(putInAMatrix(tempData,lag)))
    }
    if (weights[k,(nSeries+2)] != 0){
      predictors = cbind(predictors,
                    normalize(putInAMatrix(highTemp,lag)))
    }
    if (weights[k,(nSeries+3)] != 0){
      predictors = cbind(predictors,
                   normalize(putInAMatrix(lowTemp,lag)))
    }
    if (weights[k,(nSeries+4)] != 0){
      predictors = cbind(predictors,
                   normalize(putInAMatrix(hmdtData,lag)))
    }  
    # remove last line from predictors; no response available for that
    predictors = predictors[(1:length(response)),] 
    
    # put all test data in a frame
    testData = data.frame(response = response, predictors = predictors)
    
    #-----------------------------
    # train on SIMILAR DAY and predict for TODAY
    
    fit = rpart(trainData$response~.,data = trainData, method="anova")
    # prune the tree
    opt = which.min(fit$cptable[,"xerror"])
    cp = fit$cptable[opt, "CP"]
    prunedTree = prune(fit, cp = cp)
    
    # make predictions
    preds = predict(prunedTree,newdata = testData[,-1]) 
    
    # calculate errors
    indices = which(response > 0)
    mape = mean(abs(preds[indices]-response[indices])/response[indices])
    mse = mean((preds - response)^2)
    rmse = sqrt(mse)
    cvrmse = rmse/mean(response)
    
    # calculate and save errors
    mapeVector_bldngs = c(mapeVector_bldngs,mape)
    cvrmseVector_bldngs = c(cvrmseVector_bldngs,cvrmse)
    
  } # done for all buildings
  
  op = data.frame(building = substr(buildings,1,nchar(buildings)-1),
                  mape = mapeVector_bldngs,
                  cvrmse = cvrmseVector_bldngs)
  opFile = paste(outFolder,day,"_",year,"_",lag,".csv",sep="")
  write.csv(op,opFile,row.names=F)

} # done for all days
