library("rpart")
library("rpart.plot")
library("MatchIt")
#number of variables 
variables = 4

#coefficients for treatment 
beta_11 = 0
beta_21 = 0
beta_31 = 1
beta_41 = 1
beta_51 = 1
beta_61 = 1
beta_71 = 1
beta_81 = 1
beta_91 = 1
#coefficients for pscore and untreatment
beta_0 = 1
beta_1 = 100
beta_2 = 100
beta_3 = 1
beta_4 = 1
beta_5 = 1
beta_6 = 1
beta_7 = 1
beta_8 = 1
beta_9 = 1
beta_outcome_trm = 100
#load world bank data set
#wb_pc1_data_all <- read.csv("~/Downloads/wb_pc1_data_all.csv")

# delete rows with NA
row.has.na <- apply(wb_pc1_data_all, 1, function(x){any(is.na(x))})
wb = wb_pc1_data_all[!row.has.na,]
wb = wb[1:100,]
# 9 variables example
if(variables == 9){
  beta = c(beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9)
  betaTrt = c(beta_11,beta_21,beta_31,beta_41,beta_51,beta_61,beta_71,beta_81,beta_91)
  c = grep("2000",colnames(wb))
  db = data.frame(wb[,c])
  names(db) = colnames(wb)[c]
}

# 4 variables example
if(variables == 4){
  beta = c(beta_1,beta_2,beta_3,beta_4)
  betaTrt = c(beta_11,beta_21,beta_31,beta_41)
  db = data.frame(cbind(wb$lnye_2000e,wb$sslp_e,wb$pc41_2000m,wb$at41_2000e))
  names(db) = c("lnye_2000e", "sslp_e","pc41_2000m","at41_2000e")
}

# 2 variables example
if(variables == 2){
  beta = c(beta_1,beta_2)
  betaTrt = c(beta_11,beta_21)
  db = data.frame(cbind(wb$lnye_2000e,wb$sslp_e))
  names(db) = c("lnye_2000e", "sslp_e")
}

# scale eahc column to range [0,1]
for(i in 1:ncol(db)){
  maxv = max(db[,i])
  db[,i] = 0.9*db[,i] / maxv
}

# pscore caculation 
db$pscore = data.matrix(db) %*% beta + beta_0
db$Trt = data.matrix(db[,1:(ncol(db)-1)]) %*% betaTrt
maxv = max(db$pscore)
db$pscore =  db$pscore / maxv

# set pecentile for treatment and undtreament
thresh = quantile(db$pscore,0.5)

# calculate outcome, set treatment
outcome = list(rep(0,nrow(db)))
Treatment = list(rep(0,nrow(db)))

for(i in 1:nrow(db)){
  if(db$pscore[i] > thresh){
    outcome[i] = db$Trt[i] + db$pscore[i] + beta_outcome_trm
    Treatment[i] = 1
  }
  else{
    outcome[i] = db$pscore[i]
    Treatment[i] = 0
  }
}

df <- data.frame(matrix(unlist(outcome), byrow = T))
Trt = data.frame(matrix(unlist(Treatment),byrow = T))
db = cbind(db,df)
names(db)[ncol(db)] = "outcome" 
db = cbind(db,Trt)
names(db)[ncol(db)] = "Treatment"

# calculate transformed outcome 
trans_outcome = list(rep(0,nrow(db)))
for(i in 1:nrow(db)){
  if(db$Treatment[i] == 1){
    trans_outcome[i] = db$outcome[i] / db$pscore[i]
  }
  else{
    trans_outcome[i] = -db$outcome[i] /(1- db$pscore[i])
  }
}
df <- data.frame(matrix(unlist(trans_outcome), byrow=T))
db = cbind(db,df)
names(db)[ncol(db)] = "trans_outcome"


# user defined split function

# y : outcome, treatment(W), "pscore", tansformed outcome

ctev <- function(y, wt,parms) {
  sumTrt = 0
  sumUntrt = 0
  sumTrtWt = 0
  sumUntrtWt = 0
  #print(nrow(y))
  for(i in 1:nrow(y)){
    if(y[i,2] == 1){
      sumTrt = sumTrt + y[i,1]/y[i,3]
      sumTrtWt = sumTrtWt + 1/y[i,3]
    }
    else{
      sumUntrt = sumUntrt + y[i,1]/(1-y[i,3])
      sumUntrtWt = sumUntrtWt + 1/(1-y[i,3])
    }
  }
  #wmean =  sumTrt/sumTrtWt - sumUntrt/sumUntrtWt
  rss = 0
  if(sumTrtWt != 0 & sumUntrtWt !=0 ){
    wmean =  sumTrt/sumTrtWt - sumUntrt/sumUntrtWt
  }
  if(sumTrtWt == 0 & sumUntrtWt !=0 ){
    wmean = - sumUntrt/sumUntrtWt
  }
  if(sumTrtWt != 0 & sumUntrtWt ==0 ){
    wmean = sumTrt/sumTrtWt
  }
  if(sumTrtWt == 0 & sumUntrtWt ==0 ){
    print("error!")
  }
  
  for(i in 1:nrow(y)){
    rss = rss + (y[i,4] - wmean)^2
    #rss = rss +  wmean^2
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
        if(y[i,2] == 1){
          sumTrt.left = sumTrt.left + y[i,1]/y[i,3]
          sumTrtWt.left = sumTrtWt.left + 1/y[i,3]
        }
        else{
          sumUntrt.left = sumUntrt.left + y[i,1]/(1-y[i,3])
          sumUntrtWt.left = sumUntrtWt.left + 1/(1-y[i,3])
        }
      }
      if(sumTrtWt.left != 0 & sumUntrtWt.left !=0 ){
        wmean.left =  sumTrt.left/sumTrtWt.left - sumUntrt.left/sumUntrtWt.left
      }
      if(sumTrtWt.left == 0 & sumUntrtWt.left !=0 ){
        wmean.left = - sumUntrt.left/sumUntrtWt.left
      }
      if(sumTrtWt.left != 0 & sumUntrtWt.left ==0 ){
        wmean.left = sumTrt.left/sumTrtWt.left
      }
      if(sumTrtWt.left == 0 & sumUntrtWt.left ==0 ){
        print("error!")
      }

      for(i in 1:j){
        rss = rss + (y[i,4] - wmean.left)^2
        #rss = rss + ( wmean.left)^2
      }
      # right child
      for(i in (j+1):n){
        if(y[i,2] == 1){
          sumTrt.right = sumTrt.right + y[i,1]/y[i,3]
          sumTrtWt.right = sumTrtWt.right + 1/y[i,3]
        }
        else{
          sumUntrt.right = sumUntrt.right + y[i,1]/(1-y[i,3])
          sumUntrtWt.right = sumUntrtWt.right + 1/(1-y[i,3])
        }
      }
      if(sumTrtWt.right != 0 & sumUntrtWt.right !=0 ){
        wmean.right =  -sumTrt.right/sumTrtWt.right + sumUntrt.right/sumUntrtWt.right
      }
      if(sumTrtWt.right == 0 & sumUntrtWt.right !=0 ){
        wmean.right =  sumUntrt.right/sumUntrtWt.right
      }
      if(sumTrtWt.right != 0 & sumUntrtWt.right ==0 ){
        wmean.right = -sumTrt.right/sumTrtWt.right
      }
      if(sumTrtWt.right == 0 & sumUntrtWt.right ==0 ){
        print("error!")
      }
      
      for(i in (j+1):n){
        rss = rss + (y[i,4] + wmean.right)^2
        #rss = rss + ( wmean.right)^2
      }
      goodness[j] =  1 / rss 
      direction[j] = sign(wmean.left)
      

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
            sumTrtWt.left = sumTrtWt.left + 1/y[i,3]
          }
          else{
            sumUntrt.left = sumUntrt.left + y[i,1]/(1-y[i,3])
            sumUntrtWt.left = sumUntrtWt.left + 1/(1-y[i,3])
          }
          
        }
        else{
          if(y[i,2] >= 1){
            sumTrt.right = sumTrt.right + y[i,1]/y[i,3]
            sumTrtWt.right = sumTrtWt.right + 1/y[i,3]
          }
          else{
            sumUntrt.right = sumUntrt.right + y[i,1]/(1-y[i,3])
            sumUntrtWt.right = sumUntrtWt.right + 1/(1-y[i,3])
          }
        }
      }
      
      if(sumTrtWt.left != 0 & sumUntrtWt.left !=0 ){
        wmean.left =  sumTrt.left/sumTrtWt.left - sumUntrt.left/sumUntrtWt.left
      }
      if(sumTrtWt.left == 0 & sumUntrtWt.left !=0 ){
        wmean.left = - sumUntrt.left/sumUntrtWt.left
      }
      if(sumTrtWt.left != 0 & sumUntrtWt.left ==0 ){
        wmean.left = sumTrt.left/sumTrtWt.left
      }
      if(sumTrtWt.left == 0 & sumUntrtWt.left ==0 ){
        print("error!")
      }
      
      if(sumTrtWt.right != 0 & sumUntrtWt.right !=0 ){
        wmean.right =  -sumTrt.right/sumTrtWt.right + sumUntrt.right/sumUntrtWt.right
      }
      if(sumTrtWt.right == 0 & sumUntrtWt.right !=0 ){
        wmean.right =  sumUntrt.right/sumUntrtWt.right
      }
      if(sumTrtWt.right != 0 & sumUntrtWt.right ==0 ){
        wmean.right = -sumTrt.right/sumTrtWt.right
      }
      if(sumTrtWt.right == 0 & sumUntrtWt.right ==0 ){
        print("error!")
      }
      
      for(i in 1:nrow(y)){
        if(grep(x[i],ux) <= j){
          rss = rss + (y[i,4]-wmean.left)^2
        }
        else{
          rss = rss + (y[i,4]-wmean.right)^2
        }
      }
      goodness[j] = prss - rss 
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
  list(y =y, parms = 0, numresp = 1, numy = 4,
       summary = sfun)
}


alist <- list(eval=ctev, split=ctsplit, init=ctinit)
# y : outcome, treatment(W), "pscore", tansformaed outcome

fit1 <- rpart(cbind(outcome,Treatment,pscore,trans_outcome) ~ . -pscore -outcome-Treatment-Trt,
              db, control=rpart.control(minsplit=10, xval=0),
              method=alist)


prp(fit1)
print(fit1)
fit2 <- rpart(trans_outcome ~ . -pscore -outcome-Treatment-Trt,
              db, control=rpart.control(minsplit=10, xval=0),
              method='anova')
prp(fit2)
print(fit2)