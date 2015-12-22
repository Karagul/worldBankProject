library("rpart")
library("rpart.plot")
library("MatchIt")
library(Rcpp)
sourceCpp("~/splitc.cpp")
#load world bank data set
wb_pc1_data_all <- read.csv("~/Downloads/wb_pc1_data_all.csv")

# delete rows with NA
row.has.na <- apply(wb_pc1_data_all, 1, function(x){any(is.na(x))})
wb = wb_pc1_data_all[!row.has.na,]
db = wb
# scale eahc column to range [0,1]
for(i in 1:ncol(db)){
  maxv = max(db[,i])
  minv = min(db[,i])
  db[,i] = (db[,i] -minv)/ (maxv-minv)
}

# pscore caculation 


psmModel <-  "TrtBin ~ gpw3_2000e + at41_1999e + pc41_1999e + alp4_1999e + lnye_1999e + 
am50_e + sslp_e + selv_e + dari_e + droa_e +
pre_avg_NDVI_max + pre_avg_temp_mean + pre_avg_precip_mean + post_avg_NDVI_max + post_avg_temp_mean + post_avg_precip_mean + 
pre_trend_temp_mean + pre_trend_precip_mean + post_trend_temp_mean + post_trend_precip_mean +
sector_group"
PSMfit <- glm(mdl, db, family="binomial", na.action=na.omit)


# set pecentile for treatment and undtreament
#thresh = quantile(db$pscore,0.5)

# calculate outcome, set treatment
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

db = db[which(db$pscore !=0 & db$pscore!=1),]

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
    ux <- sort(unique(x))
    n = length(ux)
    res = splitnc(y,x,ux)
    list(goodness=res[1:(n-1)], direction=res[n:(2*(n-1))])
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

fit1 <- rpart(cbind(outcome,TrtBin,pscore,trans_outcome) ~ . -pscore -outcome-Treatment-Trt,
              db,
              control=rpart.control(minsplit=2,cp = 0.004),
              method=alist)


prp(fit1)
print(fit1)