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
  if(is.numeric(db[,i])){
    maxv = max(db[,i])
    minv = min(db[,i])
    db[,i] = (db[,i] -minv)/ (maxv-minv)
  }
  
}

# pscore caculation 


psmModel =  "TrtBin ~ gpw3_2000e + at41_1999e + pc41_1999e + alp4_1999e + lnye_1999e + 
am50_e + sslp_e + selv_e + dari_e + droa_e +
pre_avg_NDVI_max + pre_avg_temp_mean + pre_avg_precip_mean + post_avg_NDVI_max + post_avg_temp_mean + post_avg_precip_mean + 
pre_trend_temp_mean + pre_trend_precip_mean + post_trend_temp_mean + post_trend_precip_mean"
PSMfit = glm(psmModel, db, family="binomial", na.action=na.omit)
pscore = predict(PSMfit, db, type="response")
db[,"pscore"] = pscore

# set pecentile for treatment and undtreament
#thresh = quantile(db$pscore,0.5)

# calculate outcome, set treatment
outcome = db$post_avg_NDVI_max - db$pre_avg_NDVI_max
db[,"outcome"] = outcome


# calculate transformed outcome 
trans_outcome = list(rep(0,nrow(db)))
for(i in 1:nrow(db)){
  if(db$TrtBin[i] == 1){
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
