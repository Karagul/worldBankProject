##world bank project

# load libraries
library("rpart")
library("rpart.plot")
library("MatchIt")

#load world bank data set
wb_pc1_data_all <- read.csv("~/Downloads/wb_pc1_data_all.csv")

# delete rows with NA
row.has.na <- apply(wb_pc1_data_all, 1, function(x){any(is.na(x))})
wb = wb_pc1_data_all[!row.has.na,]

# scale eahc column to range [0,1]

c = grep("2000",colnames(wb))
db = data.frame(wb[,c])
names(db) = colnames(wb)[c]

# db = data.frame(cbind(wb$lnye_2000e,wb$sslp_e,wb$pc41_2000m,wb$at41_2000e))
# names(db) = c("lnye_2000e", "sslp_e","pc41_2000m","at41_2000e")

# db = data.frame(cbind(wb$lnye_2000e,wb$sslp_e))
# names(db) = c("lnye_2000e", "sslp_e")
for(i in 1:ncol(db)){
  maxv = max(db[,i])
  db[,i] = db[,i] / maxv
}

#coefficients 
beta_0 = 1
beta_1 = 1
beta_2 = 1
beta_3 = 1
beta_4 = 1
beta_5 = 1
beta_6 = 1
beta_7 = 1
beta_8 = 1
beta_9 = 1
beta_outcome_trm = 100

# pscore caculation 
# pscore =  beta_0 + beta_1 * variable_1 + beta_2 * variable_2

# beta = c(beta_1,beta_2,beta_3,beta_4)
# beta = c(beta_1,beta_2)
beta = c(beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9)
db$pscore = data.matrix(db) %*% beta + beta_0
maxv = max(db$pscore)
db$pscore = db$pscore / maxv

# set pecentile for treatment and undtreament
thresh = quantile(db$pscore,0.5)

# calculate outcome, set treatment
# outcome = beta_0 + beta_1 * variable_1 + beta_2 * variable_2 + 
# Treatment * ( beta_1 * variable_1 + beta_2 * variable_2 + beta_outcome_trm)
outcome = list(rep(0,nrow(db)))
Treatment = list(rep(0,nrow(db)))

for(i in 1:nrow(db)){
  if(db$pscore[i] > thresh){
    outcome[i] = 2*db$pscore[i] + beta_outcome_trm
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

#regression tree model
fit <- rpart(trans_outcome ~ . -pscore -outcome ,
             db,method='anova',control=rpart.control(xval=10,cp=0.004))
prp(fit)
print(fit)

