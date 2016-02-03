library("rpart")
library("rpart.plot")
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")
sourceCpp("~/splitc.cpp")
nums <- sapply(db, is.numeric)
#test = db[,!nums]
test = db[,nums]
test[,"outcome"] = db$outcome
test[,"pscore"] = db$pscore
test[,"trans_outcome"] = db$trans_outcome
test[,"TrtBin"] =db$TrtBin
#test = test[1:10,]
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

# fit1 <- rpart(cbind(outcome,TrtBin,pscore,trans_outcome) ~ . ,
#               test,
#               control=rpart.control(minsplit=2,cp = 0.004),
#               method=alist)
fit1 <- rpart(cbind(outcome,TrtBin,pscore,trans_outcome) ~ . ,
              crxvdata,
              #=rpart.control(minsplit=20,cp = 0.001,xval = 10),
              method=alist)


#pfit<- prune(fit1, cp=fit1$cptable[which.min(fit$cptable[,"xerror"]),"CP"])


prp(fit1)
print(fit1)