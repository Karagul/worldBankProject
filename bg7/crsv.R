
#library("rpart")
#library("rpart.plot")
#library(Rcpp)
#Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
#Sys.setenv("PKG_LIBS"="-fopenmp")
#sourceCpp("~/Desktop/splitc.cpp")
library("rpart",lib.loc="/home/scratch/jzhao/wbproject/wb/R_libs/")
library("rpart.plot",lib.loc="/home/scratch/jzhao/wbproject/wb/R_libs/")
library("Rcpp",lib.loc="/home/scratch/jzhao/wbproject/wb/R_libs/")
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")
sourceCpp("/home/scratch/jzhao/wbproject/wb/splitc.cpp")
test <- read.csv("/home/scratch/jzhao/wbproject/wb/test.csv")
test = test[,2:306]
### user defined split function

ctev <- function(y, wt,parms) {
  out = node_evaluate(y)
  list(label= out[1], deviance=out[2])
}

ctsplit <- function(y, wt, x, parms, continuous) {
  if (continuous) {
    n = nrow(y)
    res = splitc(y)
    list(goodness=res[1:(n-1)], direction=res[n:(2*(n-1))])
  }
  else{
    res = splitnc(y,x)
    n=(length(res)+1)/2
    list(goodness=res[1:(n-1)], direction=res[n:(2*n-1)])
  }
}


ctinit <- function(y, offset, parms, wt) {
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    print(yval)
    paste("events=", round(yval[,1]),
          ", coef= ", format(signif(yval[,2], digits)),
          ", deviance=" , format(signif(dev, digits)),
          sep = '')}
  environment(sfun) <- .GlobalEnv
  list(y =y, parms = 0, numresp = 1, numy = 4,
       summary = sfun)
}


alist <- list(eval=ctev, split=ctsplit, init=ctinit)
# y : outcome, treatment(W), "pscore", tansformaed outcome



### cross validation 



cross_validate <- function (fit, alpha, index, alphalist){
  res = cv(fit, alpha, index)
  alpha = res[1]
  nodeid = round(res[2])
  if(nodeid < 0){
    return (alphalist) 
  }
  else{
    alphalist = c(alphalist, alpha)
    #print("alphalist")
    #print(alphalist)
    rowid = round(res[2:length(res)])
    fit[nodeid+1,1] = 1
    rowid = sort(rowid+1)
    index = index[rowid]
    fit = fit[rowid,]
    cross_validate(fit, alpha,index,alphalist)
  }
}
  db = test
  k = 10 
  n = dim(db)[1]
  sample_size = floor(n/20)
  ridx = sample(1:n,sample_size,replace=FALSE)
  crxvdata= db[ridx,]
  crxvdata$id <- sample(1:k, nrow(crxvdata), replace = TRUE)
  list = 1:k
  
  fit1 = rpart (cbind(outcome,TrtBin,pscore,trans_outcome) ~ . -id,
                crxvdata,
                control = rpart.control(cp = 0,minsplit = 10),
                method=alist)
  fit = data.matrix(fit1$frame)
  index = as.numeric(rownames(fit1$frame))
  alpha = 0
  alphalist = 0
  alphalist = cross_validate(fit, alpha, index,alphalist)
  res = rep(0,length(alphalist)-1)
  for(j in 2:(length(alphalist)-1)){
    res[j] = sqrt(alphalist[j]*alphalist[j+1])
  }
  
  alphacandidate = res
  alphaset = rep(0,length(alphacandidate))
  errset = rep(0,length(alphacandidate))
  for(l in 1:length(alphacandidate)){
    alpha = alphacandidate[l]
    error = 0
    for (i in 1:k){
      trainingset <- subset(crxvdata, id %in% list[-i])
      testset <- subset(crxvdata, id %in% c(i))
      fit1 = rpart (cbind(outcome,TrtBin,pscore,trans_outcome) ~ . -id,
                    trainingset,
                    control = rpart.control(cp = alpha,minsplit = 10),
                    method=alist)
      
      pt = predict(fit1,testset,type = "matrix")
      y = data.frame(pt)
      val = data.matrix(y)
      idx = as.numeric(rownames(y))
      dbidx = as.numeric(rownames(db))
      
      for(pid in 1:(dim(y)[1])){
        id = match(idx[pid],dbidx)
        #print(id)
        error = error + (db$trans_outcome[id] - val[pid])^2
      }
      
    }
    errset[l] = error/k 
    print(error)
  }
  
  
  alpha_res = alphacandidate[which.min(errset)]
  print("alpha value: ")
  print(alpha_res)  
 
 
 
 
 
