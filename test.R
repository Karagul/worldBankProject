library("rpart")
library("rpart.plot")
library("MatchIt")
#number of variables 
variables = 4

#coefficients for treatment 
beta_11 = 10
beta_21 = 0
beta_31 = 0
beta_41 = 0
beta_51 = 0
beta_61 = 0
beta_71 = 0
beta_81 = 0
beta_91 = 0
#coefficients for pscore and untreatment
beta_0 = 0
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
#load world bank data set
#wb_pc1_data_all <- read.csv("~/Downloads/wb_pc1_data_all.csv")

# delete rows with NA
row.has.na <- apply(wb_pc1_data_all, 1, function(x){any(is.na(x))})
wb = wb_pc1_data_all[!row.has.na,]
wb = wb[1:15000,]
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
  minv = min(db[,i])
  db[,i] = (db[,i] -minv)/ (maxv-minv)
}

# pscore caculation 
db$pscore = data.matrix(db) %*% beta + beta_0
db$Trt = data.matrix(db[,1:(ncol(db)-1)]) %*% betaTrt
maxv = max(db$pscore)
minv = min(db$pscore)
db$pscore =  (db$pscore-minv) / (maxv - minv)

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



# m.out1 <- matchit(Treatment~ lnye_2000e+sslp_e+pc41_2000m + at41_2000e , data = db, method = "nearest", distance = "logit")
# m.data1 <- match.data(m.out1,distance ="pscore") # create ps matched data set from previous output
# print("pscore")
# print(dim(db))
# db = cbind(db,m.data1$pscore)
# names(db)[ncol(db)] = "pscore"


# calculate transformed outcome 
trans_outcome = list(rep(0,nrow(db)))
for(i in 1:nrow(db)){
  if(db$Treatment[i] == 1){
    trans_outcome[i] = db$outcome[i] / db$pscore[i]
    # trans_outcome[i] = db$outcome[i] - db$pscore[i]
  }
  else{
    trans_outcome[i] = -db$outcome[i] /(1- db$pscore[i])
    #trans_outcome[i] = db$outcome[i] - db$pscore[i]
  }
}
df <- data.frame(matrix(unlist(trans_outcome), byrow=T))
db = cbind(db,df)
names(db)[ncol(db)] = "trans_outcome"


db = db[which(db$pscore !=0 & db$pscore!=1),]



 fit2 <- rpart(trans_outcome ~ . -pscore-pscore -outcome-Treatment-Trt,
               db, control=rpart.control(minsplit=50, xval=0,cp = 0.004),
               method='anova')
 prp(fit2)
 print(fit2)