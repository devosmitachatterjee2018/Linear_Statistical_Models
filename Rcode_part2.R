### MSG500-MVE190 Linear Statistical Models 2018-19
### Project, part 2: US county demographic information
# Margareta Carlerös & Devosmita Chatterjee

setwd("D:/M.SC. prog. in Engineering Mathematics and Computational Science/Project_Statistics/Project")
getwd()

### import data
data=read.table("data18.txt")
colnames(data)=c("id","county","state","area","popul","pop1834",
                 "pop65plus","phys","beds","crimes","higrads",
                 "bachelors","poors","unemployed","percapitaincome",
                "totalincome","region")
dim(data)
names(data)
head(data)
data
###  Response variable
# crimes per 1000 people
crm_1000=1000*(data$crimes/data$popul)
hist(crm_1000)
summary(crm_1000)

data$crm_1000=crm_1000

summary(data$crm_1000)
# Highest number of crimes / 1000 population
data[crm_1000==max(data$crm_1000),]
# Kings,NY

### region and state
library(plyr)
data$region=factor(data$region)
data$region=relevel(data$region,ref="4") # set baseline to 4=West
count(data$region)
plot(data$region,data$crm_1000,main="crm_1000 vs region",xlab="Region",
    ylab="crm_1000")

summary(data$state)
length(summary(data$state))

### population density
pop_density=data$popul/data$area # population per square mile
data$pop_density=pop_density
par(mfrow=c(2,2))
plot(data$pop_density,data$crm_1000)
plot(data$area,data$crm_1000)

data[data$pop_density>25000,]
data[data$area>15000,]

### pop1834, pop65plus
par(mfrow=c(2,2))
plot(data$popul, data$crm_1000) # is this even a useful comparison? Density might be better.
plot(data$pop1834,data$crm_1000)
plot(data$pop65plus,data$crm_1000)

### phys, beds
par(mfrow=c(2,2))
plot(data$phys,data$crm_1000)
plot(data$beds,data$crm_1000)

# physicians per 1000 people
phys_1000=(data$phys/data$popul)*1000
data$phys_1000=phys_1000
plot(data$phys_1000,data$crm_1000)

# hospital beds per 1000 people
beds_1000=(data$beds/data$popul)*1000
data$beds_1000=beds_1000
plot(data$beds_1000,data$crm_1000)

data[data$phys>10000,]
data[data$phys_1000>10,]

data[data$beds>15000,]
data[data$beds_1000>10,]

### higrads, bachelors, poors, unemployed
par(mfrow=c(2,2))
plot(data$higrads,data$crm_1000)
plot(data$bachelors,data$crm_1000)
plot(data$poors,data$crm_1000)
plot(data$unemployed,data$crm_1000)

data[data$poors>25,]

### percapitaincome, totalincome
par(mfrow=c(2,2))
plot(data$percapitaincome,data$crm_1000)
plot(data$totalincome,data$crm_1000)

### Transformation of variables

# POPULATION DENSITY
par(mfrow=c(2,2))
# sqrt vs. log10 transformation of response
plot(log10(data$pop_density),log10(data$crm_1000),
     main="log(crm_1000) vs. log(pop_density)")
model_log=lm(log10(data$crm_1000)~log10(data$pop_density))
abline(model_log,col=2)
plot(log10(data$pop_density),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs log(pop_density)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$pop_density))
abline(model_sqrt,col=2)

# Diagnostic plots
# SQRT
model=model_sqrt

ind_val=seq(1,dim(data)[1])

par(mfrow=c(2,2))

# Residual plot
plot(log10(data$pop_density),model$res,cex=0.01) # ------- adapt
abline(h=0)

# Leverage plot
lmi=lm.influence(model)
plot(log10(data$pop_density),lmi$hat,ylab="leverage") # ------- adapt

# Impact on slope
plot(ind_val,lmi$coeff[,2],ylab="Impact on Slope",cex=0.01)
abline(h=0)

# Impact on Least Squares criterion
plot(ind_val,lmi$sig,ylab="Impact on Sum of Squares",cex=0.01)


# % population aged 18-34
par(mfrow=c(2,2))
# sqrt vs. log10 transformation of response
plot(log10(data$pop1834),log10(data$crm_1000),
    main="log(crm_1000) vs. log(pop1834)")
model_log=lm(log10(data$crm_1000)~log10(data$pop1834))
abline(model_log,col=2)
plot(log10(data$pop1834),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(pop1834)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$pop1834))
abline(model_sqrt,col=2)

# % population aged 65 or older
# sqrt vs. log10 transformation of response
plot(log10(data$pop65plus),log10(data$crm_1000),
    main="log(crm_1000) vs. log(pop65plus)")
model_log=lm(log10(data$crm_1000)~log10(data$pop65plus))
abline(model_log,col=2)
plot(log10(data$pop65plus),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(pop1834)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$pop65plus))
abline(model_sqrt,col=2)

# physicians/1000 population
par(mfrow=c(2,2))
# sqrt vs. log10 transformation of response
plot(log10(data$phys_1000),log10(data$crm_1000),
    main="log(crm_1000) vs. log(phys_1000)")
model_log=lm(log10(data$crm_1000)~log10(data$phys_1000))
abline(model_log,col=2)
plot(log10(data$phys_1000),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(phys_1000)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$phys_1000))
abline(model_sqrt,col=2)

# beds/1000 population
# sqrt vs. log10 transformation of response
plot(sqrt(data$beds_1000),log10(data$crm_1000),
    main="log(crm_1000) vs. sqrt(beds_1000)")
model_log=lm(log10(data$crm_1000)~sqrt(data$beds_1000))
abline(model_log,col=2)
plot(sqrt(data$beds_1000),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. sqrt(beds_1000)")
model_sqrt=lm(sqrt(data$crm_1000)~sqrt(data$beds_1000))
abline(model_sqrt,col=2)

# % completed highschool (aged 25 and older)
par(mfrow=c(2,2))
# sqrt vs. log10 transformation of response
plot(data$higrads,log10(data$crm_1000),
    main="log(crm_1000) vs. higrads")
model_log=lm(log10(data$crm_1000)~data$higrads)
abline(model_log,col=2)
plot(data$higrads,sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. higrads")
model_sqrt=lm(sqrt(data$crm_1000)~data$higrads)
abline(model_sqrt,col=2)

# % with bachelor's degree (aged 25 or older)
# sqrt vs. log10 transformation of response
plot(log10(data$bachelors),log10(data$crm_1000),
    main="log(crm_1000) vs. log(bachelors)")
model_log=lm(log10(data$crm_1000)~log10(data$bachelors))
abline(model_log,col=2)
plot(log10(data$bachelors),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(bachelors)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$bachelors))
abline(model_sqrt,col=2)

# % poors
par(mfrow=c(2,2))
# sqrt vs. log10 transformation of response
plot(log10(data$poors),log10(data$crm_1000),
    main="log(crm_1000) vs. log(poors)")
model_log=lm(log10(data$crm_1000)~log10(data$poors))
abline(model_log,col=2)
plot(log10(data$poors),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(poors)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$poors))
abline(model_sqrt,col=2)
summary(model_log)
summary(model_sqrt)

# % unemployed
# sqrt vs. log10 transformation of response
plot(log10(data$unemployed),log10(data$crm_1000),
    main="log(crm_1000) vs. log(unemployed)")
model_log=lm(log10(data$crm_1000)~log10(data$unemployed))
abline(model_log,col=2)
plot(log10(data$unemployed),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(unemployed)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$unemployed))
abline(model_sqrt,col=2)

# per capita income (dollars)
par(mfrow=c(2,2))
# sqrt vs. log10 transformation of response
plot(log10(data$percapitaincome),log10(data$crm_1000),
    main="log(crm_1000) vs. log(percapitaincome)")
model_log=lm(log10(data$crm_1000)~log10(data$percapitaincome))
abline(model_log,col=2)
plot(log10(data$percapitaincome),sqrt(data$crm_1000),
    main="sqrt(crm_1000) vs. log(percapitaincome)")
model_sqrt=lm(sqrt(data$crm_1000)~log10(data$percapitaincome))
abline(model_sqrt,col=2)

### Drop outlier in reponse
hist(data$crm_1000,breaks=c(0,25,50,75,100,125,150,175,200,225,250,275,300),
    xlab="crm_1000",main="Crimes per 1000 population",cex.lab=1.5,cex.main=1.5)
summary(data$crm_1000)

# Highest number of crimes / 1000 population
data[crm_1000==max(data$crm_1000),]
# Kings,NY

data[6,]

data=data[-6,] # drop oulier in response

dim(data)

### Final variable transformations
names(data)

df=data # copy

# COVARIATES
df$log10_pop1834=log10(data$pop1834)
df$log10_pop65plus=log10(data$pop65plus)
df$log10_phys_1000=log10(data$phys_1000)

df$sqrt_beds_1000=sqrt(data$beds_1000)
df$log10_bachelors=log10(data$bachelors)
df$log10_poors=log10(data$poors)

df$log10_unemployed=log10(data$unemployed)
df$log10_percapitaincome=log10(data$percapitaincome)
df$log10_pop_density=log10(data$pop_density)

# note: higrads untransformed & region categorical with 4=west set to baseline

# RESPONSE
df$sqrt_crm_1000=sqrt(data$crm_1000) # sqrt transformation
df$log10_crm_1000=log10(data$crm_1000) # log10 transformation

names(df)

# exclude covariates
df2=df[,-c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,18,19,20,21)]
names(df2)

# RESPONSE AND OFFSET FOR GLM
df2$crimes=data$crimes
df2$popul=data$popul

names(df2)

### Check multicollinearity
library(GGally)
ggpairs(data=df2,columns=c(1,3,4,5,6,7,8,9,10,11))

# Multicollinearity plot of most highly correlated covariates

ggpairs(data=df2,columns=c(1,7,8,10))

library(gplots)
distmat=1-cor(df2[,c(1,3,4,5,6,7,8,9,10,11)])
hh=heatmap.2(as.matrix(distmat),col=redgreen(75),cexROW=.5,key=TRUE,symkey=FALSE,
             density.info='none',trace='none',srtRow=45,srtCol=45,
             cexRow=1.2,cexCol=1.2, margins=c(10,10))

names(df2)

# SQRT RESPONSE

# Build full model - then check VIF
# exclude region to get VIF that's easier to interpret
full_excl_region=lm(formula = sqrt_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_bachelors + log10_poors +
              log10_unemployed + log10_percapitaincome + log10_pop_density, data = df2)

summary(full_excl_region)

library(car)
vif(full_excl_region)

# LOG RESPONSE

# Build full model - then check VIF
# exclude region to get VIF that's easier to interpret
full_excl_region=lm(formula = log10_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_bachelors + log10_poors +
              log10_unemployed + log10_percapitaincome + log10_pop_density, data = df2)

summary(full_excl_region)

library(car)
vif(full_excl_region)

names(df2)

# exclude bachelors and percapitaincome
df2=df2[,-c(7,10)]
names(df2)

### region:use original, exclude or recode
plot(df2$region,sqrt(df2$sqrt_crm_1000))
count(df2$region)
(152+77)/dim(df2)[1]

names(df2)
par(mfrow=c(2,2))
plot(df2$higrads,df2$sqrt_crm_1000,col=df2$region)
plot(df2$log10_pop1834,df2$sqrt_crm_1000,col=df2$region)
plot(df2$log10_pop65plus,df2$sqrt_crm_1000,col=df2$region)
plot(df2$log10_phys_1000,df2$sqrt_crm_1000,col=df2$region)
plot(df2$sqrt_beds_1000,df2$sqrt_crm_1000,col=df2$region)
plot(df2$log10_poors,df2$sqrt_crm_1000,col=df2$region)
plot(df2$log10_unemployed,df2$sqrt_crm_1000,col=df2$region)
plot(df2$log10_pop_density,df2$sqrt_crm_1000,col=df2$region)
# drop region
df3=df2[,-c(2)]
names(df3)
dim(df3)

# recode region
df4=df2
df4[df2$region==4,]$region=3
df4$region=factor(df4$region)
df4$region=relevel(df4$region,ref="3") # set baseline to 3=West+South
df4$region
count(df4$region)
head(df4)

### train/test
dim(df2)
names(df2)

dim(data)
names(data)

# 9 variables - including original region
seed=1
set.seed(seed)

num_obs=dim(df2)[1] # ALL
frac=0.8 # 80% for training

# --- TRAIN
ii<-sample(seq(1,dim(df2)[1]),round(num_obs*frac))
train2=df2[ii,] # note: includes all covariates + target
row.names(train2)<-seq(1,round(num_obs*frac))
# train2=train2[-common_outliers,] # --------------------- remove COMMON OUTLIERS in train
train2_y=train2[,10] # sqrt(crm_1000)
train2_y_log=train2[,11] # ------------ using log instead of sqrt
dim(train2)
head(train2)
train2_x=as.matrix(train2[-c(10,11)]) # excluding target ----- sqrt and log
dim(train2_x)
head(train2_x)

# --- TEST
test2=df2[-ii,]
row.names(test2)<-seq(round(num_obs*frac)+1,num_obs)
test2_y=test2[,10] # sqrt(crm_1000)
test2_y_log=test2[,11] # ------------ using log instead of sqrt
dim(test2) # 
head(test2)
test2_x=as.matrix(test2[-c(10,11)]) # excluding target ----- sqrt and log
dim(test2_x)
head(test2_x)

# 8 variables - excluding region

# --- TRAIN
train3=df3[ii,] # note: includes all covariates + target
row.names(train3)<-seq(1,round(num_obs*frac))
# train3=train3[-common_outliers,] # --------------------- remove COMMON OUTLIERS in train
train3_y=train3[,9] # sqrt(crm_1000), note: different index since region removed
train3_y_log=train3[,10] # ------------ using log instead of sqrt
dim(train3) #
train3_x=as.matrix(train3[,-c(9,10)]) # excluding target ----- sqrt and log
head(train3)

# --- TEST
test3=df3[-ii,]
row.names(test3)<-seq(round(num_obs*frac)+1,num_obs)
test3_y=test3[,9] # sqrt(crm_1000), note: different index since region removed
test3_y_log=test3[,10] # ------------ using log instead of sqrt
dim(test3) # 
test3_x=as.matrix(test3[,-c(9,10)]) # excluding target ----- sqrt and log
head(test3_x)

# 9 variables - including recoded region

# --- TRAIN
train4=df4[ii,] # note: includes all covariates + target
row.names(train4)<-seq(1,round(num_obs*frac))
# train4=train4[-common_outliers,] # --------------------- remove COMMON OUTLIERS in train
train4_y=train4[,10] # sqrt(crm_1000)
train4_y_log=train4[,11] # ------------ using log instead of sqrt
dim(train4)
head(train4)
train4_x=as.matrix(train4[-c(10,11)]) # excluding target ----- sqrt and log


# --- TEST
test4=df4[-ii,]
row.names(test4)<-seq(round(num_obs*frac)+1,num_obs)
test4_y=test4[,10] # sqrt(crm_1000)
test4_y_log=test4[,11] # ------------ using log instead of sqrt
dim(test4)
head(test4)
test4_x=as.matrix(test4[-c(10,11)]) # excluding target ----- sqrt and log

### MLR: model selection - REGION INCLUDED

# Without any variable selection: full model
# Use training data for model selection
full_model=lm(formula = sqrt_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train2)
summary(full_model)

# AIC
select_AIC=step(full_model,directions="backward",trace=T)
select_AIC

# BIC
select_BIC=step(full_model,directions="backward",trace=F,k=log(dim(train2)[1])) # note dim of train2
select_BIC

# Without any variable selection: full model
full_model=lm(formula = log10_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train2)
summary(full_model)

# AIC
select_AIC=step(full_model,directions="backward",trace=F)
select_AIC

# BIC
select_BIC=step(full_model,directions="backward",trace=F,k=log(dim(train2)[1])) # note dim of train2
select_BIC

### MLR: model selection & diagnostics - REGION RECODED
# Without any variable selection: full model
full_model=lm(formula = sqrt_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train4) # train4
summary(full_model)

# AIC
select_AIC=step(full_model,directions="backward",trace=F)
select_AIC

# BIC
select_BIC=step(full_model,directions="backward",trace=F,k=log(dim(train4)[1]))
select_BIC

# Without any variable selection: full model
full_model=lm(formula = log10_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train4) # train4
summary(full_model)

# AIC
select_AIC=step(full_model,directions="backward",trace=F)
select_AIC

# BIC
select_BIC=step(full_model,directions="backward",trace=F,k=log(dim(train4)[1]))
select_BIC

### MLR: model diagnostics - REGION INCLUDED

# Without any variable selection: full model
full_model=lm(formula = sqrt_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train2)
summary(full_model)

# AIC model
aic_reg_model=lm(formula = sqrt_crm_1000 ~ log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors + 
              log10_unemployed + log10_pop_density + region, data = train2)
summary(aic_reg_model)

# AIC model - diagnostics

model=aic_reg_model
n=dim(train2)[1]
p=10


# --- Leverage ---
v <- hatvalues(model)
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n and D=1")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))

plot(dfb[,8],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_7(i)",
     main="dfbeta_7 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,8]) > 2/sqrt(n),]))

plot(dfb[,9],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_8(i)",
     main="dfbeta_8 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,9]) > 2/sqrt(n),]))

plot(dfb[,10],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_9(i)",
     main="dfbeta_9 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,10]) > 2/sqrt(n),]))

sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 4,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)

# AIC model - sel_outliers > 1 removed
train2_out=train2[-outliers,]

aic_reg_model_out=lm(formula = sqrt_crm_1000 ~ log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors + 
              log10_unemployed + log10_pop_density + region, 
                 data = train2_out)
summary(aic_reg_model_out)

# compare to model with outliers
aic_reg_model=lm(formula = sqrt_crm_1000 ~ log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors + 
              log10_unemployed + log10_pop_density + region, 
                 data = train2)
summary(aic_reg_model)

# BIC model
bic_reg_model=lm(formula = sqrt_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train2)
summary(bic_reg_model)

# BIC model - diagnostics

model=bic_reg_model
n=dim(train2)[1]
p=7


# --- Leverage ---
v <- hatvalues(model)
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n and D=1")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))


sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)

# BIC model - sel_outliers > 1 removed
train2_out=train2[-outliers,]

bic_reg_model_out=lm(formula = sqrt_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train2_out)
summary(bic_reg_model_out)

# compare to model with outliers
bic_reg_model=lm(formula = sqrt_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train2)
summary(bic_reg_model)

# Without any variable selection: full model
full_model=lm(formula = log10_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train2)
summary(full_model)

# AIC model
aic_reg_model_log=lm(formula = log10_crm_1000 ~ log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train2)
summary(aic_reg_model)

# AIC model - diagnostics

model=aic_reg_model_log
n=dim(train2)[1]
p=10


# --- Leverage ---
v <- hatvalues(model)
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n and D=1")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))

plot(dfb[,8],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_7(i)",
     main="dfbeta_7 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,8]) > 2/sqrt(n),]))

plot(dfb[,9],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_8(i)",
     main="dfbeta_8 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,9]) > 2/sqrt(n),]))

plot(dfb[,10],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_9(i)",
     main="dfbeta_9 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,10]) > 2/sqrt(n),]))

sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)

# AIC model - sel_outliers > 1 removed
train2_out=train2[-outliers,]

aic_reg_model_out_log=lm(formula = log10_crm_1000 ~ log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors + 
              log10_unemployed + log10_pop_density + region, 
                 data = train2_out)
summary(aic_reg_model_out_log)

# compare to model with outliers
summary(aic_reg_model_log)

# BIC model
bic_reg_model_log=lm(formula = log10_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train2)
summary(bic_reg_model_log)

# BIC model - diagnostics

model=bic_reg_model_log
n=dim(train2)[1]
p=7


# --- Leverage ---
v <- hatvalues(model)
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n and D=1")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))


sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)

# BIC model - sel_outliers > 1 removed
train2_out=train2[-outliers,]

bic_reg_model_out_log=lm(formula = log10_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train2_out)
summary(bic_reg_model_out_log)

# compare to model with outliers
summary(bic_reg_model_log)

# Without any variable selection: full model
full_model=lm(formula = sqrt_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train4) # train4
summary(full_model)

# AIC model
aic_reg_recoded_model=lm(formula = sqrt_crm_1000 ~ log10_pop65plus  +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              +log10_unemployed + log10_pop_density + region, data = train4) # train4
summary(aic_reg_recoded_model)

# AIC - diagnostics

model=aic_reg_recoded_model
n=dim(train4)[1]-11
p=9

par(mfrow=c(2,2))
# --- Leverage ---
v <- hatvalues(model)
plot(v, main="Leverage", xlab="i", ylab="Leverage")
abline(h=2*(p/n))

# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))

plot(dfb[,8],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_7(i)",
     main="dfbeta_7 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,8]) > 2/sqrt(n),]))

plot(dfb[,9],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_8(i)",
     main="dfbeta_8 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,9]) > 2/sqrt(n),]))


sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)
outliers_aic=outliers

# AIC - sel_outliers > 1 removed

train4_out=train4[-outliers,]

aic_reg_recoded_model_out=lm(formula = sqrt_crm_1000 ~ log10_pop65plus  +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              +log10_unemployed + log10_pop_density + region, data = train4_out)
summary(aic_reg_recoded_model_out)
# compare to model with outliers
summary(aic_reg_recoded_model)

# BIC model
bic_reg_recoded_model=lm(formula = sqrt_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train4) # train4
summary(bic_reg_recoded_model)

# BIC - diagnostics

model=bic_reg_recoded_model
n=dim(train4)[1]
p=6

par(mfrow=c(2,2))
# --- Leverage ---
v <- hatvalues(model)
plot(v, main="Leverage", xlab="i", ylab="Leverage")
abline(h=2*(p/n))
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))


sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)
outliers_bic=outliers

# BIC - sel_outliers > 1 removed

train4_out=train4[-outliers,]

# BIC model
bic_reg_recoded_model_out=lm(formula = sqrt_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train4_out) # train4
summary(bic_reg_recoded_model_out)

# compare to model with outliers
summary(bic_reg_recoded_model)

# Without any variable selection: full model
full_model=lm(formula = log10_crm_1000 ~ higrads + log10_pop1834 + log10_pop65plus +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              log10_unemployed + log10_pop_density + region, data = train4) # train4
summary(full_model)

# AIC model
aic_reg_recoded_model_log=lm(formula = log10_crm_1000 ~ log10_pop65plus  +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              +log10_unemployed + log10_pop_density + region, data = train4) # train4
summary(aic_reg_recoded_model_log)

# AIC - diagnostics

model=aic_reg_recoded_model_log
n=dim(train4)[1]
p=9


# --- Leverage ---
v <- hatvalues(model)
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n and D=1")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))


sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)

# AIC - sel_outliers > 1 removed

train4_out=train4[-outliers,]

aic_reg_recoded_model_out_log=lm(formula = log10_crm_1000 ~ log10_pop65plus  +
              log10_phys_1000 + sqrt_beds_1000 + log10_poors +
              +log10_unemployed + log10_pop_density + region, data = train4_out) # train4
summary(aic_reg_recoded_model_out_log)

# compare to model with outliers
summary(aic_reg_recoded_model_log)

# BIC model
bic_reg_recoded_model_log=lm(formula = log10_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train4) # train4
summary(bic_reg_recoded_model_log)

# BIC - diagnostics

model=bic_reg_recoded_model_log
n=dim(train4)[1]
p=6


# --- Leverage ---
v <- hatvalues(model)
# identify suspected outliers
lev_ind=row.names(train2[v > 2*(p/n),])

# --- Studentized residuals ---
r_stud <- rstudent(model)
plot(r_stud,ylim=c(-7,7),xlab="i",ylab="r*_i",
       main="stud. residuals, +/- 2")
abline(h=0)
abline(h=2,col="red")
abline(h=-2,col="red")
# identify suspected outliers
sr_ind=row.names(train2[abs(r_stud) > 2,])

# --- Cook's distance ---
D<-cooks.distance(model)
plot(D,xlab="i",ylab="D_i", main="Cook's distance with D=4/n and D=1")
abline(h=4/n)
abline(h=1)
# identify suspected outliers
cd_ind=row.names(train2[D > 4/n,])

# DFBETAs
dfb=dfbetas(model)
dim(dfb)

par(mfrow=c(2,2))
dfb_ind=c()

plot(dfb[,1],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_0(i)",
     main="dfbeta_0 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,1]) > 2/sqrt(n),]))

plot(dfb[,2],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_1(i)",
     main="dfbeta_1 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,2]) > 2/sqrt(n),]))

plot(dfb[,3],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_2(i)",
     main="dfbeta_2 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,3]) > 2/sqrt(n),]))

plot(dfb[,4],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_3(i)",
     main="dfbeta_3 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,4]) > 2/sqrt(n),]))

plot(dfb[,5],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_4(i)",
     main="dfbeta_4 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,5]) > 2/sqrt(n),]))

plot(dfb[,6],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_5(i)",
     main="dfbeta_5 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,6]) > 2/sqrt(n),]))

plot(dfb[,7],ylim=c(-.5,.5),xlab="i",ylab="dfbeta_6(i)",
     main="dfbeta_6 +/- 2/sqrt(n)")
abline(h=2/sqrt(n),col="red")
abline(h=-2/sqrt(n),col="red")
dfb_ind=c(dfb_ind,row.names(train2[abs(dfb[,7]) > 2/sqrt(n),]))


sel_outliers=count(c(sr_ind,cd_ind,lev_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

all_outliers=count(c(sr_ind,cd_ind,lev_ind,dfb_ind))
all_outliers=all_outliers[order(all_outliers$freq),]
all_outliers[all_outliers$freq > 3,]

# outliers: sell outliers with freq > 1
train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)

# BIC - sel_outliers > 1 removed

train4_out=train4[-outliers,]

# BIC model
bic_reg_recoded_model_out_log=lm(formula = log10_crm_1000 ~ log10_phys_1000 + log10_poors +
              log10_pop_density + region, data = train4_out) # train4
summary(bic_reg_recoded_model_out_log)

# compare to model with outliers
summary(bic_reg_recoded_model_log)

### Cross-validation & "all subsets regression"

# use train4 - the recoded region - as the "full" dataset
# BUT create train5 based on this
# dummy coding levels of region by hand

names(train4)
train5=train4 # copy
train5$region_1=0
train5$region_2=0
names(train5)
head(train5)

train5$region_1[which(train5$region==1)]=1
train5$region_2[which(train5$region==2)]=1
head(train5)

yy=train5$sqrt_crm_1000
names(train5[,c(1,3,4,5,6,7,8,9,14,15)])
xx=train5[,c(1,3,4,5,6,7,8,9,14,15)]

library(leaps)
rleaps=regsubsets(xx,yy,int=T,nbest=250,nvmax=250,really.big=T,method=c("ex"))
# all subset models
cleaps=summary(rleaps,matrix=T)
Models=cleaps$which
Models=rbind(c(T,rep(F,dim(xx)[2])),Models)

# 10-fold cross-validation
K=10
set.seed(1)
ii=sample(seq(1,length(yy)),length(yy)) # scramble observations
foldsize=floor(length(yy)/K)
sizefold=rep(foldsize,K)
restdata=length(yy)-K*foldsize
if (restdata>0) {
    sizefold[1:restdata]=sizefold[1:restdata]+1
} # creates the size for each fold

Prederrors=matrix(0,dim(Models)[1],K) # matrix to store prediction errors
iused=0
Xmat=as.matrix(cbind(rep(1,dim(xx)[1]),xx))
for (k in (1:K)) {
    itest=ii[(iused+1):(iused+sizefold[k])] # k-fold test set
    itrain=ii[-c((iused+1):(iused+sizefold[k]))] # k-fold training set
    iused=iused+length(itest)
    for (mm in (1:dim(Models)[1])) {
        betahat<-solve(t(Xmat[itrain,Models[mm,]])%*%Xmat[itrain,
                                                       Models[mm,]])%*%t(Xmat[itrain,Models[mm,]])%*%yy[itrain]
     ypred<-Xmat[itest,Models[mm,]]%*%betahat ## predictions
     Prederrors[mm,k]<-sum((yy[itest]-ypred)^2) 
    } 
}
PE <- apply(Prederrors,1,sum)/length(yy)  ## final prediction errors, average across all folds.

jj<-sort.list(PE)[1:5]
print(as.matrix(Models[jj,]))

winmod<-Models[which.min(PE),]
winmod



#### POISSON REGRESSION
# just as for MLR - use only recoded version of baseline

# NOTE: Train2 and Test2 use recoded region variable !!!

Train2=train4
Test2=test4
Test2_y=test4_y
head(Train2)
head(Test2)
Train2$region

### Poisson regression - with offset
library("AER") # test for overdispersion/underdispersion

summary(p0 <- glm(crimes ~ log10_poors, data=Train2,family="poisson", offset=log(popul/1000)))
model=p0

# ---- Dispersion test
dispersiontest(model,alternative="two.sided") # Serious overdispersion

# additional checks of dispersion
model <- glm(crimes ~ log10_poors + region, data=Train2,family="poisson", offset=log(popul/1000))
dispersiontest(model,alternative="two.sided") # serious overdispersion
model <- glm(crimes ~ region, data=Train2,family="poisson", offset=log(popul/1000))
dispersiontest(model,alternative="two.sided") # serious overdispersion

# Conclusion: use negative binomial regression (with offset instead)

library(MASS) # needed for glm.nb()

summary(nb0 <- glm.nb(crimes ~ log10_poors + offset(log(popul/1000)), data=Train2))


summary(nb1 <- glm.nb(crimes ~ log10_poors + higrads + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb1)

summary(nb2 <- glm.nb(crimes ~ log10_poors + log10_pop1834 + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb2)

summary(nb3 <- glm.nb(crimes ~ log10_poors + log10_pop65plus + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb3)

summary(nb4 <- glm.nb(crimes ~ log10_poors + log10_phys_1000 + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb4)

summary(nb5 <- glm.nb(crimes ~ log10_poors + sqrt_beds_1000 + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb5)

summary(nb6 <- glm.nb(crimes ~ log10_poors + log10_unemployed + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb6)

summary(nb7 <- glm.nb(crimes ~ log10_poors + log10_pop_density + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb7)

summary(nb8 <- glm.nb(crimes ~ log10_poors + region + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb0,nb8)

summary(nb9 <- glm.nb(crimes ~ log10_poors + region + higrads +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb9)

summary(nb10 <- glm.nb(crimes ~ log10_poors + region + log10_pop1834 +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb10)

summary(nb11 <- glm.nb(crimes ~ log10_poors + region + log10_pop65plus +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb11)

summary(nb12 <- glm.nb(crimes ~ log10_poors + region + log10_phys_1000 +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb12)

summary(nb13 <- glm.nb(crimes ~ log10_poors + region + sqrt_beds_1000 +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb13)

summary(nb14 <- glm.nb(crimes ~ log10_poors + region + log10_unemployed +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb14)

summary(nb15 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb8,nb15)

summary(nb16 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + higrads +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb15,nb16)

summary(nb17 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_pop1834 +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb15,nb17)

summary(nb18 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_pop65plus +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb15,nb18)

summary(nb19 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb15,nb19)

# Interpretation of parameter estimates
exp(2.64408) # can't use this? Includes a bunch of other stuff...
exp(exp(0.77743))
exp(-0.52011)
exp(-0.26525)
exp(exp(0.29335))
exp(exp(0.32899))

summary(nb20 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + sqrt_beds_1000 +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb15,nb20)

summary(nb21 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_unemployed +
                      offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb15,nb21)

summary(nb22 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                       higrads + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb19,nb22)

summary(nb23 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                       log10_pop1834 + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb19,nb23)

summary(nb24 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                       log10_pop65plus + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb19,nb24)

summary(nb25 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                       sqrt_beds_1000 + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb19,nb25)

summary(nb26 <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                       log10_unemployed + offset(log(popul/1000)), data=Train2))

# ---- Likelihood ratio test --------------------------------------------
anova(nb19,nb26)

### GLM: diagnostics

summary(nb19)

# Diagnostics
model=nb19
n=dim(Train2)[1]

par(mfrow=c(2,2))

# Influence measures:
infl<-influence(model)
d<-cooks.distance(model)

# Pearsons residuals
r_pear <- infl$pear.res

## standardized Pearson residuals:
r_std<-r_pear/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),
     main="Standardised Pearson residuals, +/-2")
abline(h=0)
abline(h=-2,col="red")
abline(h=2,col="red")
# identify suspected outliers
spr_ind=row.names(Train2[abs(r_std) > 2,])

# Cook's distance:
plot(d,ylim=c(0,0.045),xlab="i",ylab="D",
     main="Cook's distance with D=4/n ")
abline(h=4/n)
# identify suspected outliers
cd_ind=row.names(Train2[d > 4/n,])
#r_std[d>0.03]

sel_outliers=count(c(spr_ind,cd_ind))
sel_outliers=sel_outliers[order(sel_outliers$freq),]
row.names(sel_outliers)=sel_outliers$x
sel_outliers[sel_outliers$freq > 1,]

# outliers: sell outliers with freq > 1
Train2[row.names(sel_outliers[sel_outliers$freq > 1,]),]
outliers=as.numeric(row.names(sel_outliers[sel_outliers$freq > 1,]))
length(outliers)
outliers_nb19=outliers

Train2_out=Train2[-outliers,]

summary(nb19_out <- glm.nb(crimes ~ log10_poors + region + log10_pop_density + log10_phys_1000 +
                      offset(log(popul/1000)), data=Train2_out)) # 9 outliers removed
# compare to model w/ outliers
summary(nb19)

### Comparing outliers

outliers_aic
outliers_bic
outliers_nb19

common_outliers=c(124,170,256,265,271,301,332,50,57,65,7) # 11 outliers in model 1 and 2

### Prediction

# Final MLR models

summary(aic_reg_model_out)
summary(aic_reg_model_out_log)
summary(bic_reg_model_out)
summary(bic_reg_model_out_log)
summary(aic_reg_recoded_model_out)
summary(aic_reg_recoded_model_out_log)
summary(bic_reg_recoded_model_out)
summary(bic_reg_recoded_model_out_log)

# pMSE

#print("pMSE: AIC model using original region")
#aic_reg_pred=predict(aic_reg_model_out, newdata=test2)
#sum((aic_reg_pred^2-test2$sqrt_crm_1000^2)^2)/length(test2_y) # square to get untransformed

#print("pMSE: AIC model using original region + LOG")
#aic_reg_pred_log=predict(aic_reg_model_out_log, newdata=test2)
#sum((10^aic_reg_pred_log-10^test2$log10_crm_1000)^2)/length(test2$log10_crm_1000) # 10^ to get untransformed

#print("pMSE: BIC model using original region")
#bic_reg_pred=predict(bic_reg_model_out, newdata=test2)
#sum((bic_reg_pred^2-test2$sqrt_crm_1000^2)^2)/length(test2_y)

#print("pMSE: BIC model using original region + LOG")
#bic_reg_pred_log=predict(bic_reg_model_out_log, newdata=test2)
#sum((10^bic_reg_pred_log-10^test2$log10_crm_1000)^2)/length(test2_y)

print("pMSE: AIC model using recoded region")
aic_reg_recoded_pred=predict(aic_reg_recoded_model_out, newdata=test4) # test4
sum((aic_reg_recoded_pred^2-test2$sqrt_crm_1000^2)^2)/length(test4_y)

#print("pMSE: AIC model using recoded region + LOG")
#aic_reg_recoded_pred_log=predict(aic_reg_recoded_model_out_log, newdata=test4) # test4
#sum((10^aic_reg_recoded_pred_log-10^test2$log10_crm_1000)^2)/length(test4_y)

print("pMSE: BIC model using recoded region")
bic_reg_recoded_pred=predict(bic_reg_recoded_model_out, newdata=test4) # test4
sum((bic_reg_recoded_pred^2-test2$sqrt_crm_1000^2)^2)/length(test4_y)

#print("pMSE: BIC model using recoded region + LOG")
#bic_reg_recoded_pred_log=predict(bic_reg_recoded_model_out_log, newdata=test4) # test4
#sum((10^bic_reg_recoded_pred_log-10^test2$log10_crm_1000)^2)/length(test4_y)


summary(nb19_out)

# plot MLR results
par(mfrow=c(2,2))

# --- AIC model using recoded region
aic_reg_recoded_pred=predict(aic_reg_recoded_model_out, newdata=test4) # test4
aic_reg_recoded_pred=(aic_reg_recoded_pred)^2 # rescaled
plot(1000*(test4$crimes/test4$popul),aic_reg_recoded_pred,
     ylim=c(0,140),xlim=c(0,140),
    main="Predicted by model 1 vs observed",
    ylab="Predicted by model 1",xlab="Observed")
abline(0,1,col="red",lty=2)

# --- BIC model using recoded region
bic_reg_recoded_pred=predict(bic_reg_recoded_model_out, newdata=test4) # test4
bic_reg_recoded_pred=(bic_reg_recoded_pred)^2 # rescaled
plot(1000*(test4$crimes/test4$popul),bic_reg_recoded_pred,
     ylim=c(0,140),xlim=c(0,140),
    main="Predicted by model 2 vs observed",
    ylab="Predicted by model 2",xlab="Observed") 
abline(0,1,col="red",lty=2)

# GLM: predict using Test data
# --- nb19_out
pred_nb19_out=predict(nb19_out,type="response",newdata=Test2) # to get predicted crimes
pred_nb19_out=(pred_nb19_out/Test2$popul)*1000 # to get predicted crm_1000

# Plots vs. observed
plot(1000*(Test2$crimes/Test2$popul),pred_nb19_out,
     ylim=c(0,140),xlim=c(0,140),
     main="Predicted by model 3 vs observed",
     xlab="Observed",ylab="Predicted by model 3")
abline(0,1,col="red",lty=2)

# pMSE

print("pMSE: AIC/ASR model using recoded region")
aic_reg_recoded_pred=predict(aic_reg_recoded_model_out, newdata=test4) # test4
sum((aic_reg_recoded_pred^2-test2$sqrt_crm_1000^2)^2)/length(test4_y) # rescaled

print("pMSE: BIC model using recoded region")
bic_reg_recoded_pred=predict(bic_reg_recoded_model_out, newdata=test4) # test4
sum((bic_reg_recoded_pred^2-test2$sqrt_crm_1000^2)^2)/length(test4_y) # rescaled

print("pMSE: pred_nb19_out")
sum((pred_nb19_out-1000*(Test2$crimes/Test2$popul))^2)/dim(Test2)[1]


### --- END --- ###
