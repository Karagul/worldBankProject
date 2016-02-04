library("rpart",lib.loc="/home/scratch/jzhao/wbproject/wb/R_libs/")
library("rpart.plot",lib.loc="/home/scratch/jzhao/wbproject/wb/R_libs/")
library("Rcpp",lib.loc="/home/scratch/jzhao/wbproject/wb/R_libs/")
Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
Sys.setenv("PKG_LIBS"="-fopenmp")
sourceCpp("/home/scratch/jzhao/wbproject/wb/splitc.cpp")
test <- read.csv("/home/scratch/jzhao/wbproject/wb/test.csv")
test = test[,2:304]
test = test[,-83]

db = test 
thresh = quantile(db$pscore,0.5)

# variables impact to the mean effect
coeff_mean = c(1:11)

# variable impact to treatment effect
coeff_trt = c(11)

# calculate outcome, set treatment
outcome = list(rep(0,nrow(db)))
Treatment = list(rep(0,nrow(db)))

for(i in 1:nrow(db)){
  if(db$pscore[i] > thresh){
    outcome[i] = coeff_trt %*% as.numeric(db[i,coeff_trt]) + coeff_mean  %*% as.numeric(db[i,coeff_mean])
    Treatment[i] = 1
  }
  else{
    outcome[i] = as.numeric(db[i,coeff_trt]) %*% coeff_trt
    Treatment[i] = 0
  }
}

df <- data.frame(matrix(unlist(outcome), byrow = T))
Trt = data.frame(matrix(unlist(Treatment),byrow = T))
db[,"outcome"] = df
db[,"treatment"] = Trt

trans_outcome = list(rep(0,nrow(db)))
for(i in 1:nrow(db)){
  if(db$treatment[i] == 1){
    trans_outcome[i] = db$outcome[i] / db$pscore[i]
  }
  else{
    trans_outcome[i] = -db$outcome[i] /(1- db$pscore[i])
  }
}
df <- data.frame(matrix(unlist(trans_outcome), byrow=T))
db = cbind(db,df)
names(db)[ncol(db)] = "trans_outcome"


# #number of variables 
# variables = 9
# 
# #coefficients for treatment 
# beta_11 = 100
# beta_21 = 0
# beta_31 = 0
# beta_41 = 0
# beta_51 = 0
# beta_61 = 0
# beta_71 = 0
# beta_81 = 0
# beta_91 = 0
# #coefficients for pscore and untreatment
# beta_0 = 0
# beta_1 = 1
# beta_2 = 1
# beta_3 = 1
# beta_4 = 1
# beta_5 = 1
# beta_6 = 1
# beta_7 = 1
# beta_8 = 1
# beta_9 = 1
# beta_outcome_trm = 1000
# #load world bank data set
# wb_pc1_data_all <- read.csv("~/Downloads/wb_pc1_data_all.csv")
# 
# # delete rows with NA
# row.has.na <- apply(wb_pc1_data_all, 1, function(x){any(is.na(x))})
# wb = wb_pc1_data_all[!row.has.na,]
# #wb = wb[1:1000,]
# # 9 variables example
# if(variables == 9){
#   beta = c(beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9)
#   betaTrt = c(beta_11,beta_21,beta_31,beta_41,beta_51,beta_61,beta_71,beta_81,beta_91)
#   c = grep("2000e",colnames(wb))
#   db = data.frame(wb[,c(c,269,245,301,302)])
#   names(db) = colnames(wb)[c(c,269,245,301,302)]
# }
# 
# # 4 variables example
# if(variables == 4){
#   beta = c(beta_1,beta_2,beta_3,beta_4)
#   betaTrt = c(beta_11,beta_21,beta_31,beta_41)
#   db = data.frame(cbind(wb$lnye_2000e,wb$sslp_e,wb$pc41_2000m,wb$at41_2000e))
#   names(db) = c("lnye_2000e", "sslp_e","pc41_2000m","at41_2000e")
# }
# 
# # 2 variables example
# if(variables == 2){
#   beta = c(beta_1,beta_2)
#   betaTrt = c(beta_11,beta_21)
#   db = data.frame(cbind(wb$lnye_2000e,wb$sslp_e))
#   names(db) = c("lnye_2000e", "sslp_e")
# }
# 
# # scale eahc column to range [0,1]
# 
# for(i in 1:ncol(db)){
#   if(is.numeric(db[,i])){
#     maxv = max(db[,i])
#     minv = min(db[,i])
#     db[,i] = (db[,i] -minv)/ (maxv-minv)
#   }
#   
# }
# # pscore caculation 
# db$pscore = data.matrix(db) %*% beta + beta_0
# db$Trt = data.matrix(db[,1:(ncol(db)-1)]) %*% betaTrt
# maxv = max(db$pscore)
# db$pscore =  db$pscore / maxv
# 
# # set pecentile for treatment and undtreament
# thresh = quantile(db$pscore,0.5)
# 
# # calculate outcome, set treatment
# outcome = list(rep(0,nrow(db)))
# Treatment = list(rep(0,nrow(db)))
# 
# for(i in 1:nrow(db)){
#   if(db$pscore[i] > thresh){
#     outcome[i] = db$Trt[i] + db$pscore[i] + beta_outcome_trm
#     Treatment[i] = 1
#   }
#   else{
#     outcome[i] = db$pscore[i]
#     Treatment[i] = 0
#   }
# }
# 
# df <- data.frame(matrix(unlist(outcome), byrow = T))
# Trt = data.frame(matrix(unlist(Treatment),byrow = T))
# db = cbind(db,df)
# names(db)[ncol(db)] = "outcome" 
# db = cbind(db,Trt)
# names(db)[ncol(db)] = "Treatment"
# db = db[which(db$pscore !=0 & db$pscore!=1),]
# # calculate transformed outcome 
# trans_outcome = list(rep(0,nrow(db)))
# for(i in 1:nrow(db)){
#   if(db$Treatment[i] == 1){
#     trans_outcome[i] = db$outcome[i] / db$pscore[i]
#   }
#   else{
#     trans_outcome[i] = -db$outcome[i] /(1- db$pscore[i])
#   }
# }
# df <- data.frame(matrix(unlist(trans_outcome), byrow=T))
# db = cbind(db,df)
# names(db)[ncol(db)] = "trans_outcome"
# 
# library("rpart")
# library("rpart.plot")
# library(Rcpp)
# Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
# Sys.setenv("PKG_LIBS"="-fopenmp")
# sourceCpp("~/Desktop/splitc.cpp")


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


fit1 <- rpart(cbind(outcome,treatment,pscore,trans_outcome) ~ . ,
              db, control=rpart.control(minsplit=20),
              method=alist)


prp(fit1)
print(fit1)