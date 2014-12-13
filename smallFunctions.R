# functions used by ART models

library(zoo)

# outliers
remOutliers = function(x, na.rm = TRUE, ...) {
  qnt = quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1]-H)] = NA
  y[x > (qnt[2]+H)] = NA
  y
}

# windowing transformation of a time series
putInAMatrix = function(series,cols){
  rows = length(series) - cols + 1
  dataMatrix = matrix(numeric(0),rows,cols) 
  for(i in 1:rows){
    dataMatrix[i,] = series[i:(i+cols-1)]
  }
  dataMatrix
}

# normalize a matrix
normalize = function(x){
  r = dim(x)[1]
  c = dim(x)[2]
  avg = apply(x,2,mean) # calculate the mean for each column 
  std = apply(x,2,sd) # calculate the standard deviation within each column

  z = matrix(0,r,c) 
  for(i in 1:r){ 
    for(j in 1:c){ 
      z[i,j] = (x[i,j] - avg[j])/std[j] 
    } 
  } 
  z 
}

# normalize matrix rows between 0 and 1
normalizeMatrix01 = function(x){
  r = dim(x)[1]
  c = dim(x)[2]
  
  z = matrix(0,r,c) 
  for(i in 1:r){
      norm = (x[i,] - min(x[i,]))/(max(x[i,]) - min(x[i,]))
      z[i,] = norm
    }  
  z 
}

# normalize a vector
normalizeVector = function(x){
  avg = mean(x)
  std = sd(x)
  z = (x - avg)/std 
  z
}

# count non-zero columns
nonZeroColumnCount = function(x){
  r = dim(x)[1]
  c = dim(x)[2]
  
  count = numeric(r)
  for(i in 1:r){
    count[i] = 0
    for(j in 1:c){
      if(x[i,j]!=0){
        count[i] = count[i] + 1
      }
    }
  }
  count  
}

# set all entries to zero except top n in each row
onlyTopNWeights = function(x,n){
  r = dim(x)[1]
  c = dim(x)[2]
  
  z = matrix(0,r,c)
  for (i in 1:r){
    for(j in 1:c){
      w = rev(sort(x[i,]))[1:n] # top n weights
      if(x[i,j] %in% w){
        z[i,j] = x[i,j]   
      }
    }
  }
  z
}

# set all entries to zero except top n in a vector x
onlyTopN = function(x,n){
  l = length(x)
  z = numeric(l) # set all to zero
  w = rev(sort(x))[1:n] # top n weights
  for (i in 1:l){
    if(x[i] %in% w){
      z[i] = x[i] # retain only non-zero weights  
    }
  }
  # check for duplicates in z
  extras = length(which(z!=0)) - n
  
  if(extras > 0){
    minz = rev(sort(z))[sum(z!=0)]
    for (i in 1:l){
      if(z[i]==minz){
        z[i] = 0
        extras = extras - 1
        if (extras == 0){
          break
        }
      }
    } 
  }
  # return z
  z    
}
#---------

# display diagonal elements of a matrix
displayDiagonal = function(x){
  r = dim(x)[1]
  c = dim(x)[2]
  
  for (i in 1:r){
    cat(x[i,i],",")
  }
}

# set all non-zero entries to 1
SetNonZeroToOne = function(x){
  r = dim(x)[1]
  c = dim(x)[2]
  
  for (i in 1:r){
    for(j in 1:c){
      if(x[i,j] != 0){
        x[i,j] = 1
      }      
    }
  }
  x
}

#---------------------------

readPredictorMatrix = function(year){
  inputFile = paste("New Code/Matrix",year,".csv",sep="")
  data = scan(inputFile,skip=1,sep=",",what=list(0,0,0,0)) 
  data
}

# kwh
readKwh = function(buildings,year){
  kwhData = NULL
  nSeries = length(buildings)
  for (i in 1:nSeries){
    inputFile = paste("kwh/",buildings[i],year,".csv",sep="")
    data = scan(inputFile,skip=1,sep=",",what=list(0)) 
    kwh = data[[1]]
    kwh = remOutliers(kwh)
    kwh = na.fill(kwh, "extend")  
    kwhData = rbind(kwhData,kwh)  
  }
  kwhData
}

# occ
readOcc = function(buildings,year){
  occupancyData = NULL
  nSeries = length(buildings)
  for (i in 1:nSeries){
    inputFile = paste("Occupancy/",buildings[i],'occ_',year,'.csv',sep="")
    if (file.exists(inputFile)){
      data = read.csv(inputFile)
      data = data[[1]]
    }else{
      inFile = paste("kwh/",buildings[i],year,".csv",sep="")
      kwh = scan(inFile,skip=1,sep=",",what=list(0)) 
      data = numeric(length(kwh[[1]]))    
    }
    occupancyData = rbind(occupancyData,data)
  }
  occupancyData
}

# temperature
readTemp = function(year){
  inputFile = paste("Weather/","tmp-",year,'.csv',sep="")
  data = scan(inputFile,skip=1,sep=",",what=list("",0)) 
  tempData = data[[2]]
  tempData
}

# extreme temp 
readHighTemp = function(year){
  inputFile = paste("Weather/","tmp-",year,'.csv',sep="")
  data = scan(inputFile,skip=1,sep=",",what=list("",0)) 
  tempData = data[[2]]
  
  highTemp = tempData
  highTemp[highTemp < 70] = 70
  highTemp
}

# low temp 
readLowTemp = function(year){
  inputFile = paste("Weather/","tmp-",year,'.csv',sep="")
  data = scan(inputFile,skip=1,sep=",",what=list("",0)) 
  tempData = data[[2]]
  
  lowTemp = tempData
  lowTemp[lowTemp > 50 ] = 50
  lowTemp
}

# humidity
readHmdt = function(year){
  inputFile = paste("Weather/","hmdt-",year,'.csv',sep="")
  data = scan(inputFile,skip=1,sep=",",what=list("",0)) 
  hmdtData = data[[2]]
  hmdtData
}
