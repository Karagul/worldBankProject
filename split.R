# Any necessary setup
library(rpart)
options(na.action="na.omit")
options(digits=4) # to match earlier output
set.seed(1234)

mystate <- data.frame(state.x77, region=factor(state.region))
names(mystate) <- c("population","income" , "illiteracy","life" ,
                    "murder", "hs.grad", "frost",     "area",      "region")

# y : outcome, treatment(W), "pscore", tansformaed outcome

ctev <- function(y, wt,parms) {
  sumTrt = 0
  sumUntrt = 0
  sumTrtWt = 0
  sumUntrtWt = 0
  for(i in 1:nrow(y)){
    if(y[i,2] >= 1){
      sumTrt = sumTrt + y[i,1]/y[i,3]
      sumTrtWt = sumTrt + y[i,3]
    }
    else{
      sumUntrt = sumUntrt + y[i,1]/(1-y[i,3])
      sumUntrtWt = sumUntrtWt + (1-y[i,3])
    }
  }
  wmean =  sumTrt/sumTrtWt - sumUntrt/sumUntrtWt
  rss = 0
  for(i in 1:nrow(y)){
    rss = rss + (y[i,1] - wmean)^2
  }
  list(label= wmean, deviance=rss)
}

# The split function, where most of the work occurs.
#   Called once per split variable per node.
# If continuous=T
#   The actual x variable is ordered
#   y is supplied in the sort order of x, with no missings,
#   return two vectors of length (n-1):
#      goodness = goodness of the split, larger numbers are better.
#                 0 = couldn't find any worthwhile split
#        the ith value of goodness evaluates splitting obs 1:i vs (i+1):n
#      direction= -1 = send "y< cutpoint" to the left side of the tree
#                  1 = send "y< cutpoint" to the right
#         this is not a big deal, but making larger "mean y's" move towards
#         the right of the tree, as we do here, seems to make it easier to
#         read
# If continuos=F, x is a set of integers defining the groups for an
#   unordered predictor.  In this case:
#       direction = a vector of length m= "# groups".  It asserts that the
#           best split can be found by lining the groups up in this order
#           and going from left to right, so that only m-1 splits need to
#           be evaluated rather than 2^(m-1)
#       goodness = m-1 values, as before.
#
# The reason for returning a vector of goodness is that the C routine
#   enforces the "minbucket" constraint. It selects the best return value
#   that is not too close to an edge.

ctsplit <- function(y, wt, x, parms, continuous) {
  if (continuous) {
    n = nrow(y)
    goodness <- double(n-1)
    direction <- goodness
    # continuous x variable
    for(j in 1:(n-1)){
      rss = 0
      sumTrt.left = 0
      sumUntrt.left = 0
      sumTrtWt.left = 0
      sumUntrtWt.left = 0
      sumTrt.right = 0
      sumUntrt.right = 0
      sumTrtWt.right = 0
      sumUntrtWt.right = 0
      # left child
      for(i in 1:j){
        if(y[i,2] >= 1){
          sumTrt.left = sumTrt.left + y[i,1]/y[i,3]
          sumTrtWt.left = sumTrt.left + y[i,3]
        }
        else{
          sumUntrt.left = sumUntrt.left + y[i,1]/(1-y[i,3])
          sumUntrtWt.left = sumUntrtWt.left + (1-y[i,3])
        }
      }
      wmean.left =  sumTrt.left/sumTrtWt.left - sumUntrt.left/sumUntrtWt.left
      for(m in 1:j){
        rss = rss + (y[m,1] - wmean.left)^2
      }
      # right child
      for(i in (j+1):n){
        if(y[i,2] >= 1){
          sumTrt.right = sumTrt.right + y[i,1]/y[i,3]
          sumTrtWt.right = sumTrt.right + y[i,3]
        }
        else{
          sumUntrt.right = sumUntrt.right + y[i,1]/(1-y[i,3])
          sumUntrtWt.right = sumUntrtWt.right + (1-y[i,3])
        }
      }
      wmean.right =  sumTrt.right/sumTrtWt.right - sumUntrt.right/sumUntrtWt.right
      for(k in (j+1):n){
        rss = rss + (y[k,1] - wmean.right)^2
      }
      goodness[j] = -rss 
      direction[j] = sign(sumUntrtWt.left+sumTrtWt.left)
    }
    list(goodness=goodness, direction=direction)
  }
  else {
    # Categorical X variable
    ux <- sort(unique(x))
    n = length(ux)
    goodness <- double(n-1)
    direction <- goodness
    mean = double(n)
    count = double(n)
    sum = double(n)
    for(i in 1:nrow(y)){
      for(j in 1:n){
        if(x[i] == ux[j]){
          count[j] = count[j] + 1
          sum[j] = sum[j] + y[i,1]
        }
      }
    }
    mean = sum / count
    #rank X
    idx = order(mean)
    ux = ux[idx]
    for(j in 1:(n-1)){
      rss = 0
      sumTrt.left = 0
      sumUntrt.left = 0
      sumTrtWt.left = 0
      sumUntrtWt.left = 0
      sumTrt.right = 0
      sumUntrt.right = 0
      sumTrtWt.right = 0
      sumUntrtWt.right = 0
      for(i in 1:nrow(y)){
        if(grep(x[i],ux) <= j){
          if(y[i,2] >= 1){
            sumTrt.left = sumTrt.left + y[i,1]/y[i,3]
            sumTrtWt.left = sumTrt.left + y[i,3]
          }
          else{
            sumUntrt.left = sumUntrt.left + y[i,1]/(1-y[i,3])
            sumUntrtWt.left = sumUntrtWt.left + (1-y[i,3])
          }
          
        }
        else{
          if(y[i,2] >= 1){
            sumTrt.right = sumTrt.right + y[i,1]/y[i,3]
            sumTrtWt.right = sumTrt.right + y[i,3]
          }
          else{
            sumUntrt.right = sumUntrt.right + y[i,1]/(1-y[i,3])
            sumUntrtWt.right = sumUntrtWt.right + (1-y[i,3])
          }
        }
      }
      wmean.left = sumTrt.left/sumTrtWt.left - sumUntrt.left/sumUntrtWt.left
      wmean.right = sumTrt.right/sumTrtWt.right - sumUntrt.right/sumUntrtWt.right
      for(i in 1:nrow(y)){
        if(grep(x[i],ux) <= j){
          rss = rss + (y[i,1]-wmean.left)^2
        }
        else{
          rss = rss + (y[i,1]-wmean.right)^2
        }
      }
      goodness[j] = -rss 
      #direction[j] = sign(sumUntrtWt.left+sumTrtWt.left)
    }
    list(goodness=goodness, direction=ux)
  }
}

# The init function:

ctinit <- function(y, offset, parms, wt) {
  
  sfun <- function(yval, dev, wt, ylevel, digits ) {
    print(yval)
    paste("events=", round(yval[,1]),
          ", coef= ", format(signif(yval[,2], digits)),
          ", deviance=" , format(signif(dev, digits)),
          sep = '')}
  environment(sfun) <- .GlobalEnv
  list(y = cbind(y, offset), parms = 0, numresp = 1, numy = 3,
       summary = sfun)
}


alist <- list(eval=ctev, split=ctsplit, init=ctinit)

fit1 <- rpart(cbind(income,illiteracy,murder) ~ .,
              mystate, control=rpart.control(minsplit=10, xval=0),
              method=alist)

