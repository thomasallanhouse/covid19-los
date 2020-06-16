####################################################
# Weibull Accellerated Failure Time model 
# Implementation in JAGS
# 
# Code for manuscript:
# A comparison of methods for analysing length ofstay data for COVID-19 patients
# Vekaria, Overton et al. 
#
####################################################
# RAMP COVID -19 Modelling
# Social Statistics Department
# University of Manchester
# 
####################################################
# Authors:
# Jihye Kim, Arkadiusz Wisniowski
# 
####################################################
# Last modified 09/06/2020
#
##################################################
# Run the Weibull model (right censored data) 
# using JAGS
# Requires:  tidyverse, readstata13, RJags, lattice, tictoc

rm(list = ls(all = TRUE))
library("tidyverse")
library("readstata13")
library("R2jags")
library("lattice")
library("tictoc")

##################################################
########## DATA from dta.file       ##############  
##################################################

# reading in data using dta (stata 14) file
#setwd("D:/RAMP")
data_CHESS <- read.dta13("CHESS_Case_Report_comparison paper_2020-05-26.dtanumeric_variablesadded.dta")

# removing missing observations
data_scenario_0 <- data_CHESS %>% 
        filter( !is.na(female) & 
                !is.na(ageyear4)&
                !is.na(wk12) & 
                !is.na(icu))



##################################################
##########DATA cleaning for Models ###############
##################################################

# Model 1. Hospital admission -> ICU entry, icu patients
# Model 2. ICU entry -> ICU exit,icu patients
# Model 3. Hospital admission -> finaloutcome, icu patients
# Model 4. Hospital admission -> finaloutcome, NON-icu patients


data_model_1 <- data_scenario_0 %>% 
    mutate(t=HOSPTOICU, x= recode(dummy_durationhospitalicu, `0` = "1", `1`="0"))

data_model_2 <- data_scenario_0 %>% 
    mutate(t=ICUduration, x= recode(dummy_outcome_icu, `0` = "1", `1`="0") )

data_model_3 <- data_scenario_0 %>% 
    mutate(t=HOSPtoOUTCOME, x= recode(dummy_hospitaloutcome, `0` = "1", `1`="0") )

data_model_4 <- data_scenario_0 %>% 
    mutate(t=HOSPtoOUTCOME, x= recode(dummy_hospitaloutcome, `0` = "1", `1`="0") )


##################################################
##########DATA cleaning for JAGS   ###############
##################################################

data_model_1_jags <- data_model_1 %>% filter(HOSPTOICU>0 & icu==1) %>% 
  dplyr::select(HOSPTOICU, dummy_durationhospitalicu, wk13, wk14, wk15, wk16, wk17, wk18, wk19_20, female, 
                age1_49, age50_64, age65_74, trustcode, x, t, ageyear4, wk15_20 ) %>% 
                mutate( t = replace( t, dummy_durationhospitalicu==0, NA) ) 
#N = 2983 Mean:0.66667

data_model_2_jags <- data_model_2 %>% filter(ICUduration>0 & icu==1) %>% 
  dplyr::select(ICUduration, dummy_outcome_icu, wk13, wk14, wk15, wk16, wk17, wk18, wk19_20, female, 
                age1_49, age50_64, age65_74, trustcode, x, t, ageyear4, wk15_20) %>% 
                mutate( t = replace( t, dummy_outcome_icu==0, NA) ) 
#N = 1809 Mean:11.57658

data_model_3_jags <- data_model_3 %>% filter(HOSPtoOUTCOME>0 & icu==1) %>% 
  dplyr::select(HOSPtoOUTCOME, dummy_hospitaloutcome, wk13, wk14, wk15, wk16, wk17, wk18, wk19_20, female, 
                age1_49, age50_64, age65_74, trustcode, x, t, ageyear4, wk15_20) %>% 
                mutate( t = replace( t, dummy_hospitaloutcome==0, NA) ) 
#N = 2555 Mean:16.02642 

data_model_4_jags <- data_model_4 %>% filter(HOSPtoOUTCOME>0 & icu==0) %>% 
  dplyr::select(HOSPtoOUTCOME, dummy_hospitaloutcome, wk13, wk14, wk15, wk16, wk17, wk18, wk19_20, female, 
                age1_49, age50_64, age65_74, trustcode, x, t, ageyear4, wk15_20) %>% 
                mutate( t = replace( t, dummy_hospitaloutcome==0, NA) ) 
# N=2805 Mean:8.36631

##################################################
############## AFT  Weibull Model 1 ##############
##################################################

# JAGS specification
# for help see http://www.stats.ox.ac.uk/~nicholls/MScMCMC15/jags_user_manual.pdf
model_1_weibull <- function () {
  
  for (i in 1:length(t)){
    
    c[i] <- HOSPTOICU[i]+ dummy_durationhospitalicu[i]	
    x[i] ~ dinterval(t[i], c[i])                       #x=1 if t is censored, 0 otherwise; if x=1, then t>c
    t[i] ~ dweib(b, lambda[i])                         #t is right-censored observation 
    lambda[i] <- exp(-mu[i]*b)                         #b is a shape parameter;lambda is a scale parameter
                                                       #lambda=pow(1/exp(mu), b)

    mu[i] <- beta[1] + beta[2]*wk13[i] + beta[3]*wk14[i] + beta[4]*wk15[i] +
      beta[5]*wk16[i] + beta[6]*wk17[i]+  beta[7]*wk18[i] + 
      beta[8]*wk19_20[i] + beta[9]*female[i] + beta[10]*age1_49[i]  +
      beta[11]*age50_64[i] + beta[12]*age65_74[i]
                        
   t.pred[i] ~ dweib(b, lambda[i])                     #prediction of length of day
   }

  beta[1:12]~dmnorm(m.a[1:12],t.a[1:12,1:12])          #multivariate normal prior for beta
  t.a[1,1]<-0.1
  t.a[2,2]<-0.1
  t.a[3,3]<-0.1
  t.a[4,4]<-0.1
  t.a[5,5]<-0.1
  t.a[6,6]<-0.1
  t.a[7,7]<-0.1
  t.a[8,8]<-0.1
  t.a[9,9]<-0.1
  t.a[10,10]<-0.1
  t.a[11,11]<-0.1
  t.a[12,12]<-0.1

  for (i in 1:11)  {
    for (j in (i+1):12){
      t.a[j,i]   <-0}       
    
    for (j in 1:i){
      t.a[j,i+1]  <-0}
  }
  
  for (i in 1:12)  {
  m.a[i] <- 0
  }
 
  b ~ dunif(0, 10)                                      #uniform prior for b                         
  mean.lambda <- mean(log(lambda[]))                    #calculate mean of log lambda

}
# end of the model

# setting parameters to save
 params <- c("beta", "b", "t.pred", "mean.lambda")

# running the model and generating posteriors
tic() # measures time
jags.fit.model.1.weibull <- jags.parallel(data=data_model_1_jags, inits=NULL, params, 
                            model.file=model_1_weibull,  n.chains = 3, 
                            n.thin=4, n.iter=50000, 
                            n.burnin=10000, DIC=TRUE, 
                            jags.module = c("glm", "dic"))
toc()

# saving results
capture.output(print(jags.fit.model.1.weibull),file="jags.fit.model.1.weibull.txt")
# plotting summaries of posteriors
plot(jags.fit.model.1.weibull)

##################################################
############## AFT  Weibull Model 2 ##############
##################################################

model_2_weibull <- function () {
  
  for (i in 1:length(t)){
    
    c[i] <- ICUduration[i]+ dummy_outcome_icu[i]	
    x[i] ~ dinterval(t[i], c[i])
    t[i] ~ dweib(b, lambda[i])
    lambda[i] <- exp(-mu[i] * b)

    mu[i] <- beta[1] + beta[2]*wk13[i] + beta[3]*wk14[i] + beta[4]*wk15[i] +
    beta[5]*wk16[i] + beta[6]*wk17[i]+  beta[7]*wk18[i] + 
      beta[8]*wk19_20[i] + beta[9]*female[i] + beta[10]*age1_49[i]  +
      beta[11]*age50_64[i] + beta[12]*age65_74[i]


   t.pred[i] ~   dweib(b, lambda[i])
   h[i] <- b*pow(t.pred[i], b-1)*lambda[i]    

  }


  beta[1:12]~dmnorm(m.a[1:12],t.a[1:12,1:12])
  t.a[1,1]<-0.1
  t.a[2,2]<-0.1
  t.a[3,3]<-0.1
  t.a[4,4]<-0.1
  t.a[5,5]<-0.1
  t.a[6,6]<-0.1
  t.a[7,7]<-0.1
  t.a[8,8]<-0.1
  t.a[9,9]<-0.1
  t.a[10,10]<-0.1
  t.a[11,11]<-0.1
  t.a[12,12]<-0.1

  for (i in 1:11)  {
    for (j in (i+1):12){
      t.a[j,i]   <-0}       
    
    for (j in 1:i){
      t.a[j,i+1]  <-0}
  }
  
  for (i in 1:12)  {
  m.a[i] <- 0
  }

  b ~ dunif(0, 10)
  mean.lambda <- mean(log(lambda[]))

}

 params <- c("beta", "b", "t.pred", "mean.lambda")

tic()
jags.fit.model.2.weibull <- jags.parallel(data=data_model_2_jags, inits=NULL, params, 
                            model.file=model_2_weibull,  n.chains = 3, 
                            n.thin=4, n.iter=50000, 
                            n.burnin=10000, DIC=TRUE, 
                            jags.module = c("glm", "dic"))
toc()

capture.output(print(jags.fit.model.2.weibull),file="jags.fit.model.2.weibull.txt")
plot(jags.fit.model.2.weibull)


##################################################
############## AFT  Weibull Model 3 ##############
##################################################

model_3_weibull <- function () {
  
  for (i in 1:length(t)){
    
    c[i] <- HOSPtoOUTCOME[i]+ dummy_hospitaloutcome[i]	
    x[i] ~ dinterval(t[i], c[i])
    t[i] ~ dweib(b, lambda[i])
    lambda[i] <- exp(-mu[i] * b)

    mu[i] <- beta[1] + beta[2]*wk13[i] + beta[3]*wk14[i] + beta[4]*wk15[i] +
      beta[5]*wk16[i] + beta[6]*wk17[i]+  beta[7]*wk18[i] + 
      beta[8]*wk19_20[i] + beta[9]*female[i] + beta[10]*age1_49[i]  +
      beta[11]*age50_64[i] + beta[12]*age65_74[i]

    t.pred[i] ~   dweib(b, lambda[i])
  }


  beta[1:12]~dmnorm(m.a[1:12],t.a[1:12,1:12])
  t.a[1,1]<-0.1
  t.a[2,2]<-0.1
  t.a[3,3]<-0.1
  t.a[4,4]<-0.1
  t.a[5,5]<-0.1
  t.a[6,6]<-0.1
  t.a[7,7]<-0.1
  t.a[8,8]<-0.1
  t.a[9,9]<-0.1
  t.a[10,10]<-0.1
  t.a[11,11]<-0.1
  t.a[12,12]<-0.1

  for (i in 1:11)  {
    for (j in (i+1):12){
      t.a[j,i]   <-0}       
    
    for (j in 1:i){
      t.a[j,i+1]  <-0}
  }
  
  for (i in 1:12)  {
  m.a[i] <- 0
  }

  b ~ dunif(0, 10)
  mean.lambda <- mean(log(lambda[]))
}

 params <- c("beta", "b", "t.pred", "mean.lambda")

tic()
jags.fit.model.3.weibull <- jags.parallel(data=data_model_3_jags, inits=NULL, params, 
                            model.file=model_3_weibull,  n.chains = 3, 
                            n.thin=4, n.iter=50000, 
                            n.burnin=10000, DIC=TRUE, 
                            jags.module = c("glm", "dic"))
toc()

capture.output(print(jags.fit.model.3.weibull),file="jags.fit.model.3.weibull.txt")
plot(jags.fit.model.3.weibull)



##################################################
############## AFT  Weibull Model 4 ##############
##################################################

model_4_weibull <- function () {
  
  for (i in 1:length(t)){
    
    c[i] <- HOSPtoOUTCOME[i]+ dummy_hospitaloutcome[i]	
    x[i] ~ dinterval(t[i], c[i])
    t[i] ~ dweib(b, lambda[i])
    lambda[i] <- exp(-mu[i] * b)

    mu[i] <- beta[1] + beta[2]*wk13[i] + beta[3]*wk14[i] + beta[4]*wk15[i] +
      beta[5]*wk16[i] + beta[6]*wk17[i]+  beta[7]*wk18[i] + 
      beta[8]*wk19_20[i] + beta[9]*female[i] + beta[10]*age1_49[i]  +
      beta[11]*age50_64[i] + beta[12]*age65_74[i]

   t.pred[i] ~   dweib(b, lambda[i])

  }


  beta[1:12]~dmnorm(m.a[1:12],t.a[1:12,1:12])
  t.a[1,1]<-0.1
  t.a[2,2]<-0.1
  t.a[3,3]<-0.1
  t.a[4,4]<-0.1
  t.a[5,5]<-0.1
  t.a[6,6]<-0.1
  t.a[7,7]<-0.1
  t.a[8,8]<-0.1
  t.a[9,9]<-0.1
  t.a[10,10]<-0.1
  t.a[11,11]<-0.1
  t.a[12,12]<-0.1

  for (i in 1:11)  {
    for (j in (i+1):12){
      t.a[j,i]   <-0}       
    
    for (j in 1:i){
      t.a[j,i+1]  <-0}
  }
  
  for (i in 1:12)  {
  m.a[i] <- 0
  }

  b ~ dunif(0, 10)
  mean.lambda <- mean(log(lambda[]))
}

 params <- c("beta", "b", "t.pred", "mean.lambda")

tic()
jags.fit.model.4.weibull <- jags.parallel(data=data_model_4_jags, inits=NULL, params, 
                            model.file=model_4_weibull,  n.chains = 3, 
                            n.thin=4, n.iter=50000, 
                            n.burnin=10000, DIC=TRUE, 
                            jags.module = c("glm", "dic"))
toc()

capture.output(print(jags.fit.model.4.weibull),file="jags.fit.model.4.weibull.txt")
plot(jags.fit.model.4.weibull)

##################################################
############## as.mcmc file   ####################
##################################################

model1.mcmc <- as.mcmc(jags.fit.model.1.weibull)
model1.mat <- as.matrix(model1.mcmc)

model2.mcmc <- as.mcmc(jags.fit.model.2.weibull)
model2.mat <- as.matrix(model2.mcmc)

model3.mcmc <- as.mcmc(jags.fit.model.3.weibull)
model3.mat <- as.matrix(model3.mcmc)

model4.mcmc <- as.mcmc(jags.fit.model.4.weibull)
model4.mat <- as.matrix(model4.mcmc)



##################################################
############## calculate mean,sd #################
##################################################
# posterior predictive distributions for LOS
out1=model1.mat
out1 = out1[,which(colnames(out1)=="t.pred[1]") : which(colnames(out1)=="t.pred[2983]")]
out2=model2.mat
out2 = out2[,which(colnames(out2)=="t.pred[1]") : which(colnames(out2)=="t.pred[1809]")]
out3=model3.mat
out3 = out3[,which(colnames(out3)=="t.pred[1]") : which(colnames(out3)=="t.pred[2555]")]
out4=model4.mat
out4 = out4[,which(colnames(out4)=="t.pred[1]") : which(colnames(out4)=="t.pred[2805]")]

out <- data.frame(out1)
out <- gather(out)
dim(out)

# table summarising posteriors
table0 <- out %>% 
  summarize(mean = mean(value), sd = sd(value), n = n()/30000, lower2.5 = quantile(value, probs = 0.025), lower25 = quantile(value, probs = 0.25), mid = quantile(value, probs = 0.5) , upper75 = quantile(value, probs = 0.75), upper97.5 = quantile(value, 0.975))
table0

##################################################
############## age*week table ####################
##################################################
out <- data.frame(out1)
dim(out)

week <- data_model_1_jags$wk15_20
wk <- matrix(rep(week, each=30000))
wk <- data.frame(wk)
dim(wk)

age4 <- data_model_1_jags$ageyear4
age <- matrix(rep(age4, each=30000))
age <- data.frame(age)
dim(age)

df <- data.frame(out, wk, age)

table1 <- df %>% 
  group_by(age, wk) %>% 
  summarize(mean = mean(value), sd = sd(value), n = n()/30000, lower2.5 = quantile(value, probs = 0.025), lower25 = quantile(value, probs = 0.25), mid = quantile(value, probs = 0.5) , upper75 = quantile(value, probs = 0.75), upper97.5 = quantile(value, 0.975))
table1