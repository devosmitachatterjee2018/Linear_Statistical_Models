### MSG500-MVE190 Linear Statistical Models 2018-19
### Project, part 1: Summary of minianalyses
### Margareta Carlerös & Devosmita Chatterjee

#set current directory
setwd("D:/M.SC. prog. in Engineering Mathematics and Computational Science/Project_Statistics")
getwd() 

housedata=read.csv("kc_house_data.csv")
head(housedata)

# Randomly select subset of 500 observations

set.seed(1) # set the seed

ii<-sample(seq(1,dim(housedata)[1]),500)  # obtain a random sample of 500 indeces
train<-housedata[ii,]                    # select a random subset of your full dataset
row.names(train)<-seq(1,500)             # assign new IDs to each row
head(train)
dim(train)
names(train)

remainingdata<-housedata[-ii,] # let’s eliminate the observations indexed by ii
ii<-sample(seq(1,dim(remainingdata)[1]),500) # these are indeces for testing data
test<-remainingdata[ii,]
row.names(test)<-seq(501,1000)
head(test)
dim(test)
names(test)

head(train)

# extract year from date
mydate=train$date
yr_sold=as.numeric(format(as.Date(mydate,format="%Y%m%d"),"%Y"))

# age
age=yr_sold-train$yr_built
train$age=age
train[age<0,"age"]=0 # if house sold before built set age of house at selling date to 0. (it's a new house!)

# time_since_reno
train$yr_renovated[train$yr_renovated==0]=train$yr_built[train$yr_renovated==0]
train$time_since_reno=yr_sold-train$yr_renovated
train[age<0,"time_since_reno"]=0 # correct for cases where house sold before built

head(train)

# install.packages("geosphere")
library(geosphere)
p1=cbind(train$long,train$lat) # longitude,latitude
p2=matrix(c(-122.332069,47.606209),nrow=dim(train)[1],ncol=2,byrow=TRUE)
# distance in meters
dist_hav=distHaversine(p1, p2)

# dist_hav
train$dist_hav=dist_hav

head(train)
names(train)

# untransformed lat, transformed price

plot(train$lat,log10(train$price))
model=lm(log10(train$price)~train$lat)
abline(model,col=2)

# Diagnostic plots

pred_transformed=train$lat # ADAPT
ind_val=seq(1,dim(train)[1])

par(mfrow=c(2,2))

# Residual plot
plot(pred_transformed,model$res,cex=0.01)
abline(h=0)
# mark large residuals
#id=identify(x_transformed[induse],model$res,row.names(data)[induse],pos=T)

# Leverage plot
lmi=lm.influence(model)
plot(pred_transformed,lmi$hat,ylab="leverage")
#id=identify(x_transformed[induse],lmi$hat,row.names(data)[induse],pos=T)

# Impact on slope
plot(ind_val,lmi$coeff[,2],ylab="Impact on Slope",cex=0.01)
abline(h=0)
# id=identify(induse,lmi$coef[,2],label=row.names(data)[induse],pos=T)

# Impact on Least Squares criterion
plot(ind_val,lmi$sig,ylab="Impact on Sum of Squares",cex=0.01)
# id=identify(induse, lmi$sig,label=row.names(data)[induse],pos=T)

# untransformed long, transformed price - NOTE: negative values so can't use sqrt, log10

plot(train$long,log10(train$price))
model=lm(log10(train$price)~train$long)
abline(model,col=2)

# Diagnostic plots

pred_transformed=train$long # ADAPT
ind_val=seq(1,dim(train)[1])

par(mfrow=c(2,2))

# Residual plot
plot(pred_transformed,model$res,cex=0.01)
abline(h=0)
# mark large residuals
#id=identify(x_transformed[induse],model$res,row.names(data)[induse],pos=T)

# Leverage plot
lmi=lm.influence(model)
plot(pred_transformed,lmi$hat,ylab="leverage")
#id=identify(x_transformed[induse],lmi$hat,row.names(data)[induse],pos=T)

# Impact on slope
plot(ind_val,lmi$coeff[,2],ylab="Impact on Slope",cex=0.01)
abline(h=0)
# id=identify(induse,lmi$coef[,2],label=row.names(data)[induse],pos=T)

# Impact on Least Squares criterion
plot(ind_val,lmi$sig,ylab="Impact on Sum of Squares",cex=0.01)
# id=identify(induse, lmi$sig,label=row.names(data)[induse],pos=T)

plot(train$dist_hav,log10(train$price))

# sqrt transformation

par(mfrow=c(1,1))
plot(sqrt(train$dist_hav),log10(train$price))
model=lm(log10(train$price)~sqrt(train$dist_hav))
abline(model,col=2)


# Diagnostic plots

pred_transformed=sqrt(train$dist_hav) # ADAPT
ind_val=seq(1,dim(train)[1])

par(mfrow=c(2,2))

# Residual plot
plot(pred_transformed,model$res,cex=0.01)
abline(h=0)
# mark large residuals
#id=identify(x_transformed[induse],model$res,row.names(data)[induse],pos=T)

# Leverage plot
lmi=lm.influence(model)
plot(pred_transformed,lmi$hat,ylab="leverage")
#id=identify(x_transformed[induse],lmi$hat,row.names(data)[induse],pos=T)

# Impact on slope
plot(ind_val,lmi$coeff[,2],ylab="Impact on Slope",cex=0.01)
abline(h=0)
# id=identify(induse,lmi$coef[,2],label=row.names(data)[induse],pos=T)

# Impact on Least Squares criterion
plot(ind_val,lmi$sig,ylab="Impact on Sum of Squares",cex=0.01)
# id=identify(induse, lmi$sig,label=row.names(data)[induse],pos=T)


plot(train$time_since_reno,log10(train$price))

# sqrt transformation

par(mfrow=c(1,1))
plot(sqrt(train$time_since_reno),log10(train$price))
model=lm(log10(train$price)~sqrt(train$time_since_reno))
abline(model,col=2)


# Diagnostic plots

pred_transformed=sqrt(train$time_since_reno) # ADAPT
ind_val=seq(1,dim(mydata)[1])

par(mfrow=c(2,2))

# Residual plot
plot(pred_transformed,model$res,cex=0.01)
abline(h=0)
# mark large residuals
#id=identify(x_transformed[induse],model$res,row.names(data)[induse],pos=T)

# Leverage plot
lmi=lm.influence(model)
plot(pred_transformed,lmi$hat,ylab="leverage")
#id=identify(x_transformed[induse],lmi$hat,row.names(data)[induse],pos=T)

# Impact on slope
plot(ind_val,lmi$coeff[,2],ylab="Impact on Slope",cex=0.01)
abline(h=0)
# id=identify(induse,lmi$coef[,2],label=row.names(data)[induse],pos=T)

# Impact on Least Squares criterion
plot(ind_val,lmi$sig,ylab="Impact on Sum of Squares",cex=0.01)
# id=identify(induse, lmi$sig,label=row.names(data)[induse],pos=T)


# create data frame of transformed response and predictor variables
train2=data.frame(train$bedrooms,train$bathrooms,log10(train$sqft_living),log10(train$sqft_lot),
             train$floors,log10(train$sqft_above),sqrt(train$sqft_basement),log10(train$sqft_living15),
             log10(train$sqft_lot15),sqrt(train$age),sqrt(train$time_since_reno),train$lat,train$long,
             log10(train$dist_hav),log10(train$price))

df_names=c("bedrooms","bathrooms","log_sqft_living","log_sqft_lot","floors","log_sqft_above","sqrt_sqft_basement",
  "log_sqft_living15","log_sqft_lot15","sqrt_age","sqrt_time_since_reno","lat","long","log_dist_hav","log_price")
names(train2)=df_names
head(train2)
dim(train2)


# ------- Test - feature engineering

# extract year from date
mydate=test$date
yr_sold=as.numeric(format(as.Date(mydate,format="%Y%m%d"),"%Y"))
# age
age=yr_sold-test$yr_built
test$age=age
test[age==-1,"age"]=0 # if house sold before built set age of house at selling date to 0. (it's a new house!)
# time_since_reno
test$yr_renovated[test$yr_renovated==0]=test$yr_built[test$yr_renovated==0]
test$time_since_reno=yr_sold-test$yr_renovated
test[age==-1,"time_since_reno"]=0 # correct for cases where house sold before built

library(geosphere)
p1=cbind(test$long,test$lat) # longitude,latitude
p2=matrix(c(-122.332069,47.606209),nrow=dim(test)[1],ncol=2,byrow=TRUE)
# distance in meters
dist_hav=distHaversine(p1, p2)

# dist_hav
test$dist_hav=dist_hav

# --------- Test2 - transformed variables

test2=data.frame(test$bedrooms,test$bathrooms,log10(test$sqft_living),log10(test$sqft_lot),
             test$floors,log10(test$sqft_above),sqrt(test$sqft_basement),log10(test$sqft_living15),
             log10(test$sqft_lot15),sqrt(test$age),sqrt(test$time_since_reno),test$lat,test$long,
             log10(test$dist_hav),log10(test$price))

df_names=c("bedrooms","bathrooms","log_sqft_living","log_sqft_lot","floors","log_sqft_above","sqrt_sqft_basement",
  "log_sqft_living15","log_sqft_lot15","sqrt_age","sqrt_time_since_reno","lat","long","log_dist_hav","log_price")
names(test2)=df_names
head(test2)
dim(test2)


library(GGally)
ggpairs(data=train2,columns=1:14)

# a graphical summary of collinearity
library(gplots)
df2=train2[,1:14]
distmat=1-cor(df2)
hh=heatmap.2(as.matrix(distmat),col=redgreen(75),cexROW=.5,key=TRUE,symkey=FALSE,
             density.info='none',trace='none',srtRow=45,srtCol=45,
             cexRow=0.75,cexCol=0.75)

mm=lm(log_price~bedrooms+bathrooms+log_sqft_living+log_sqft_lot+floors+log_sqft_above+
      sqrt_sqft_basement+log_sqft_living15+log_sqft_lot15+sqrt_age+
      sqrt_time_since_reno+lat+long+log_dist_hav,data=train2) # use train2
summary(mm)
par(mfrow=c(2,2))
plot(mm) # diagnostic plots

library(car)
par(mfrow=c(2,2))
cooksd=cooks.distance(mm)
plot(cooksd,main="Cook's Distance",type="h")

library(car)
vif(mm)

# excluded time_since_reno, sqft_lot,sqft_living

mm2=lm(log_price~bedrooms+bathrooms+floors+log_sqft_above+
      sqrt_sqft_basement+log_sqft_living15+log_sqft_lot15+sqrt_age+
      lat+long+log_dist_hav,data=train2) # use train2
summary(mm2)
par(mfrow=c(2,2))
plot(mm2) # diagnostic plots

# Check collinearity again
vif(mm2)

names(train2)

library(gplots)
df2=train2[,c(1,2,5,6,7,8,9,10,12,13,14)]
distmat=1-cor(df2)
hh=heatmap.2(as.matrix(distmat),col=redgreen(75),cexROW=.5,key=TRUE,symkey=FALSE,
             density.info='none',trace='none',srtRow=45,srtCol=45,
             cexRow=0.75,cexCol=0.75)

library(GGally)
ggpairs(data=train2,columns=c(1,2,5,6,7,8,9,10,12,13,14))

ss=step(mm2,trace=0,k=500)
print(ss$anova)

RSSvec=ss$anova[,5] # resid. dev from above - note this includes the intercept to we have p parameters
deltaRSSvec=diff(RSSvec) # calculate the difference between successive residuals
MSEvec=RSSvec[2:length(RSSvec)]/ss$anova[2:length(RSSvec),4]
Fvec=deltaRSSvec/MSEvec
pvals=1-pf(Fvec,1,ss$anova[2:length(RSSvec),4])
out=cbind(ss$anova[2:length(RSSvec),1],round(Fvec,3),round(pvals,5))
colnames(out)=c("variable drop","F","p-value")
print(out)

# BEST MODEL (highly collinear variables removed)
mm_final=lm(formula = log_price ~ bedrooms + bathrooms + log_sqft_above + 
    sqrt_sqft_basement + log_sqft_living15 + log_sqft_lot15 + 
    log_dist_hav + lat + long, data = train2) # use train2
summary(mm_final)
par(mfrow=c(2,2))
plot(mm_final)
cooksd=cooks.distance(mm_final)
par(mfrow=c(2,2))
plot(cooksd,main="Cook's Distance",type="h")

selectstep=step(mm2,directions="backward",trace=F,k=log(500))
selectstep

# a graphical summary of collinearity
library(gplots)
df2=train2[,c(1,2,6,7,8,9,12,13,14)]
distmat=1-cor(df2)
hh=heatmap.2(as.matrix(distmat),col=redgreen(75),cexROW=.5,key=TRUE,symkey=FALSE,
             density.info='none',trace='none',srtRow=45,srtCol=45,
             cexRow=0.75,cexCol=0.75)

vif(mm_final)

predval=predict(selectstep,newdata=test2) # predict on transformed test data
prederror=sum((test2[,dim(test2)[2]]-predval)^2)/length(predval) # pMSE calculation: compare estimate to transformed response data
prederror # correct formula for pMSE?

# Plot observed vs predicted
plot(predval,test2[,dim(test2)[2]], main="Observed vs. predicted for model from mini 2",xlab="Predicted",ylab="Observed")

names(train2[,c(1,5,7,8,9,10,12,14)])

# 8 variables total
mm3=lm(formula = log_price ~ bedrooms + floors +
    sqrt_sqft_basement + log_sqft_living15 + log_sqft_lot15 + 
    sqrt_age+lat+log_dist_hav, data = train2) # use train2
summary(mm3)
par(mfrow=c(2,2))
plot(mm3)
vif(mm3)
# a graphical summary of collinearity
library(gplots)
df2=train2[,c(1,5,7,8,9,10,12,14)]
distmat=1-cor(df2)
hh=heatmap.2(as.matrix(distmat),col=redgreen(75),cexROW=.5,key=TRUE,symkey=FALSE,
             density.info='none',trace='none',srtRow=45,srtCol=45,
             cexRow=0.75,cexCol=0.75)
library(GGally)
ggpairs(data=train2,columns=c(1,5,7,8,9,10,12,14))

# --- Import data
housedata=read.csv("kc_house_data.csv")

# --- Feature engineering & data transformations on full data set
head(housedata)
housedata$sqft_living=log10(housedata$sqft_living)
housedata$sqft_lot=log10(housedata$sqft_lot)
housedata$sqft_above=log10(housedata$sqft_above)
housedata$sqft_basement=sqrt(housedata$sqft_basement)
housedata$sqft_living15=log10(housedata$sqft_living15)
housedata$sqft_lot15=log10(housedata$sqft_lot15)
housedata$price=log10(housedata$price) # transform response variable
# ---- age
mydate=housedata$date
yr_sold=as.numeric(format(as.Date(mydate,format="%Y%m%d"),"%Y")) # extract year from date
age=yr_sold-housedata$yr_built # NOTE: may be negative
housedata$age=age
housedata[age<0,"age"]=0 # if house sold before built set age of house at selling date to 0. (it's a new house!)
housedata$age=sqrt(housedata$age)
# -----dist_hav
library(geosphere)
p1=cbind(housedata$long,housedata$lat) # longitude,latitude
p2=matrix(c(-122.332069,47.606209),nrow=dim(housedata)[1],ncol=2,byrow=TRUE) # downtown Seattle coordinates
dist_hav=distHaversine(p1, p2) # distance in meters 'as the crow flies'
housedata$dist_hav=dist_hav
housedata$dist_hav=log10(housedata$dist_hav)

housedata=housedata[,-c(1,2,15,16)] # drop id,date,yr_built,yr_renovated
df_names=c("log_price","bedrooms","bathrooms","log_sqft_living","log_sqft_lot","floors",
           "waterfront","view","condition","grade","log_sqft_above","sqrt_sqft_basement",
           "zipcode", "lat","long","log_sqft_living15","log_sqft_lot15","sqrt_age",
           "log_dist_hav")
names(housedata)=df_names
head(housedata)
dim(housedata)

frac=0.7
seed_no=1
# --- 1st preselection of variables: 11 covariates
#covars=c('bedrooms','bathrooms','floors','log_sqft_above','sqrt_sqft_basement','log_sqft_living15',
#             'log_sqft_lot15','sqrt_age','lat','long','log_dist_hav') # pre-selected covariates, ADD CATEGORICAL?

# --- 2nd preselection of variables: 8 covariates
covars=c('bedrooms','floors','sqrt_sqft_basement','log_sqft_living15',
             'log_sqft_lot15','sqrt_age','lat','log_dist_hav') # pre-selected covariates, ADD CATEGORICAL?

set.seed(seed_no) # set the seed
# --- TRAIN
ii<-sample(seq(1,dim(housedata)[1]),round(1000*frac)) # for selecting training data, 1000 obs for train+test
# Q: use all data and split into train/test or use 1000 observations for train/test and vary the fraction?
# Ans: YES, since we want to explore effect of selecting other subsets on model selection!!!
# original: round(dim(housedata)[1]*frac)
train=housedata[ii,]
row.names(train)<-seq(1,round(1000*frac))
train_x=as.matrix(train[,covars]) # include only pre-selected covariates
train_y=train[,1] # response variable

# --- TEST
remainingdata<-housedata[-ii,] # eliminate the observations indexed by ii
ii<-sample(seq(1,dim(remainingdata)[1]),round(1000*(1-frac))) # these are indeces for testing data
test<-remainingdata[ii,]
row.names(test)<-seq(round(1000*frac)+1,1000)
test_x=as.matrix(test[,covars]) # include only pre-selected covariates
test_y=test[,1] # response variable

#dim(train)
#dim(train_x)
#length(train_y)
#dim(test)
#dim(test_x)
#length(test_y)
#head(train_x)

# --- Model selection using backward selection

# --- 1st preselection of variables: 11 covariates
#mymodel=lm(log_price~bedrooms+bathrooms+floors+log_sqft_above+
#           sqrt_sqft_basement+log_sqft_living15+log_sqft_lot15+sqrt_age+
#           lat+long+log_dist_hav,data=train) # pre-selected covariates, ADD CATEGORICAL
# --- 2nd preselection of variables: 8 covariates
mymodel=lm(log_price~bedrooms+floors+sqrt_sqft_basement+log_sqft_living15+
           log_sqft_lot15+sqrt_age+lat+log_dist_hav,data=train) # pre-selected covariates, ADD CATEGORICAL

selectstep=step(mymodel,directions="backward",trace=F)
size_model=length(names(selectstep$coefficients)) # including intercept
params=names(selectstep$coefficients)[-1]
predval=predict(selectstep,newdata=test[,-1])
prederror=sum((test_y-predval)^2)/length(predval)
print("Backward selection pMSE:")
prederror
plot(predval,test_y, main="Observed vs. predicted for model from mini 2",xlab="Predicted",ylab="Observed")

# --- Model selection using pMSE
library(leaps)
# all subsets regression
rleaps=regsubsets(train_x,train_y,int=T,nbest=1000,nvmax=dim(housedata)[2],
                   really.big=T,method=c("ex"))
cleaps=summary(rleaps,matrix=T)
# cleaps$which
# dim(cleaps$which) # 1 less since intercept only model not included
# 2^11 # 11 pre-selected covariates

# pMSE for intercept only model
nullpmse<-sum((test_y-mean(train_y))^2)/length(test_y)
# nullpmse

# Calculate the remaining pMSEs
pmses=rep(0,dim(cleaps$which)[1])
for (jj in (1:dim(cleaps$which)[1])) {
    mmr=lm(train_y~train_x[,cleaps$which[jj,-1]==T]) # fit model using training data
    design = test_x[,cleaps$which[jj,-1]==T]
    design = cbind(rep(1,dim(test_x)[1]),design) # add a column of ones for the intercept
    PEcp<-sum((test_y-design%*%mmr$coef)^2)/length(test_y) # pMSE for specific model
    pmses[jj]<-PEcp
}

pmses=c(nullpmse,pmses) # add pMSE for intercept only model

# Lowest pMSE per model size
tt=apply(cleaps$which,1,sum) # size of each model in the matrix (includes the intercept)
tt=c(1,tt) # add intercept-only model
tmin=min(tt) # smallest model tried
tmax=max(tt) # largest model tried
tsec=seq(tmin,tmax)
pmsevec=rep(0,length(tsec))
for(tk in 1:length(tsec)){
    pmsevec[tk]=min(pmses[tt==tsec[tk]]) # winning model for each size
}
plot(tsec,pmsevec,xlab="number of parameters",ylab="pMSE",main="prediction MSE",
    type="b",lwd=2,col=2)

# Model with smallest pMSE
print("min(pMSE):")
min(pmses) # smallest pMSE value
ptmin=which.min(pmses) # note: pmses includes intercept-only model
# Matrix of all models (including intercept)
ModMat<-rbind(c("TRUE",rep("FALSE",dim(cleaps$which)[2]-1)),cleaps$which) # add intercept-only model
pmod<-ModMat[ptmin,] # best model for prediction
winsize=sum(pmod==T)# size of winning model
winsize
# Winning model
params=names(pmod[pmod==T])[-1] # note: -1 removes intercept

mmr=lm(train_y~train_x[,params]) # fit model using training data
design = test_x[,params]
design = cbind(rep(1,dim(test_x)[1]),design) # add a column of ones for the intercept
predval=design%*%mmr$coef # by pmse model selction
plot(predval,test_y)

##------- RUN BEFORE RUNNING model_sel

# --- Import data
housedata=read.csv("kc_house_data.csv")

# --- Feature engineering & data transformations on full data set
head(housedata)
housedata$sqft_living=log10(housedata$sqft_living)
housedata$sqft_lot=log10(housedata$sqft_lot)
housedata$sqft_above=log10(housedata$sqft_above)
housedata$sqft_basement=sqrt(housedata$sqft_basement)
housedata$sqft_living15=log10(housedata$sqft_living15)
housedata$sqft_lot15=log10(housedata$sqft_lot15)
housedata$price=log10(housedata$price) # transform response variable
# ---- age
mydate=housedata$date
yr_sold=as.numeric(format(as.Date(mydate,format="%Y%m%d"),"%Y")) # extract year from date
age=yr_sold-housedata$yr_built # NOTE: may be negative
housedata$age=age
housedata[age<0,"age"]=0 # if house sold before built set age of house at selling date to 0. (it's a new house!)
housedata$age=sqrt(housedata$age)
# -----dist_hav
library(geosphere)
p1=cbind(housedata$long,housedata$lat) # longitude,latitude
p2=matrix(c(-122.332069,47.606209),nrow=dim(housedata)[1],ncol=2,byrow=TRUE) # downtown Seattle coordinates
dist_hav=distHaversine(p1, p2) # distance in meters 'as the crow flies'
housedata$dist_hav=dist_hav
housedata$dist_hav=log10(housedata$dist_hav)

housedata=housedata[,-c(1,2,15,16)] # drop id,date,yr_built,yr_renovated
df_names=c("log_price","bedrooms","bathrooms","log_sqft_living","log_sqft_lot","floors",
           "waterfront","view","condition","grade","log_sqft_above","sqrt_sqft_basement",
           "zipcode", "lat","long","log_sqft_living15","log_sqft_lot15","sqrt_age",
           "log_dist_hav")
names(housedata)=df_names
head(housedata)
dim(housedata)

model_sel=function(frac,seeds){
    # ----------------
    # frac = proportion of TRAINING data
    # seeds = number of seeds

    # --- 1st preselection of variables: 11 covariates
     covars=c('bedrooms','bathrooms','floors','log_sqft_above','sqrt_sqft_basement','log_sqft_living15',
             'log_sqft_lot15','sqrt_age','lat','long','log_dist_hav') # 11 pre-selected covariates

    # --- 2nd preselection of variables: 8 covariates
    #covars=c('bedrooms','floors','sqrt_sqft_basement','log_sqft_living15',
    #             'log_sqft_lot15','sqrt_age','lat','log_dist_hav') # 8 pre-selected covariates
    
    # -------VECTORS OF RESULTS
    
    num_obs=1000 #---------- Number of observations in train+test
    # test_y vectors
    test_y_vecs=matrix(rep(0,round(num_obs*(1-frac)))*seeds,nrow=round(num_obs*(1-frac)),ncol=seeds)
    # predicted values from backward selection
    predvals_bwd=matrix(rep(0,round(num_obs*(1-frac)))*seeds,nrow=round(num_obs*(1-frac)),ncol=seeds)
    # pMSE from backward selection
    pMSEs_bwd=rep(0,seeds)
    # size of model by backward selection
    size_bwd=rep(0,seeds)
    # params selected by backward selection
    params_bwd=c()
    # predicted values using pmse model selection
    predvals_pmse=matrix(rep(0,round(num_obs*(1-frac)))*seeds,nrow=round(num_obs*(1-frac)),ncol=seeds)
    # min pMSE using pmse model selection
    pMSEs_mins=rep(0,seeds)
    # size of model using pmse model selection
    size_pmse=rep(0,seeds)
    # params selected using pmse model selection
    params_pmse=c()
    
    # ------------
    for(i in 1:seeds){
        set.seed(i) # set the seed
        # --- TRAIN
        ii<-sample(seq(1,dim(housedata)[1]),round(num_obs*frac)) # for selecting training data, 1000 obs for train+test
        # Q: use all data and split into train/test or use 1000 observations for train/test and vary the fraction?
        # Ans: YES, since we want to explore effect of selecting other subsets on model selection!!!
        # original: round(dim(housedata)[1]*frac)
        train=housedata[ii,]
        row.names(train)<-seq(1,round(num_obs*frac))
        train_x=as.matrix(train[,covars]) # include only pre-selected covariates
        train_y=train[,1] # response variable

        # --- TEST
        remainingdata<-housedata[-ii,] # eliminate the observations indexed by ii
        ii<-sample(seq(1,dim(remainingdata)[1]),round(num_obs*(1-frac))) # these are indeces for testing data
        test<-remainingdata[ii,]
        row.names(test)<-seq(round(num_obs*frac)+1,num_obs)
        test_x=as.matrix(test[,covars]) # include only pre-selected covariates
        test_y=test[,1] # response variable
        test_y_vecs[,i]=test_y # store test_y vector
        
        # --- Model selection using backward selection--------------------------------

        # --- 1st preselection of variables: 11 covariates
        mymodel=lm(log_price~bedrooms+bathrooms+floors+log_sqft_above+
                   sqrt_sqft_basement+log_sqft_living15+log_sqft_lot15+sqrt_age+
                   lat+long+log_dist_hav,data=train) # pre-selected covariates, ADD CATEGORICAL
        # --- 2nd preselection of variables: 8 covariates
        #mymodel=lm(log_price~bedrooms+floors+sqrt_sqft_basement+log_sqft_living15+
        #           log_sqft_lot15+sqrt_age+lat+log_dist_hav,data=train) # pre-selected covariates, ADD CATEGORICAL
        
        selectstep=step(mymodel,directions="backward",trace=F) # -------- AIC or BIC: k=log(num_obs*frac)
        size_model=length(names(selectstep$coefficients)) # including intercept
        params=names(selectstep$coefficients)[-1] # remove intercept
        params_bwd=c(params_bwd,params)
        predval=predict(selectstep,newdata=test[,-1])
        prederror=sum((test_y-predval)^2)/length(predval)
        
        #print("Backward selection pMSE:")
        #prederror
        #plot(predval,test_y, main="Observed vs. predicted for model from mini 2",xlab="Predicted",ylab="Observed")
        predvals_bwd[,i]=predval
        pMSEs_bwd[i]=prederror
        size_bwd[i]=size_model
        
        # --- Model selection using pMSE----------------------------------------------
        library(leaps)
        # all subsets regression
        rleaps=regsubsets(train_x,train_y,int=T,nbest=1000,nvmax=dim(housedata)[2],
                           really.big=T,method=c("ex"))
        cleaps=summary(rleaps,matrix=T)
        # pMSE for intercept only model
        nullpmse<-sum((test_y-mean(train_y))^2)/length(test_y)
        # Calculate the remaining pMSEs
        pmses=rep(0,dim(cleaps$which)[1])
        for (jj in (1:dim(cleaps$which)[1])) {
            mmr=lm(train_y~train_x[,cleaps$which[jj,-1]==T]) # fit model using training data
            design = test_x[,cleaps$which[jj,-1]==T]
            design = cbind(rep(1,dim(test_x)[1]),design) # add a column of ones for the intercept
            PEcp<-sum((test_y-design%*%mmr$coef)^2)/length(test_y) # pMSE for specific model
            pmses[jj]<-PEcp
        }
        pmses=c(nullpmse,pmses) # add pMSE for intercept only model
        # pMSEs_mins[i]=min(pmses) # smallest pMSE value ---------------------------- REMOVED
        ptmin=which.min(pmses) # note: pmses includes intercept-only model
        # Matrix of all models (including intercept)
        ModMat<-rbind(c("TRUE",rep("FALSE",dim(cleaps$which)[2]-1)),cleaps$which) # add intercept-only model
        pmod<-ModMat[ptmin,] # best model for prediction
        winsize=sum(pmod==T)# size of winning model
        size_pmse[i]=winsize
        # Winning model
        params=names(pmod[pmod==T])[-1] # note: -1 removes intercept
        params_pmse=c(params_pmse,params) # store selected variables
        # Predict using winning model
        mmr=lm(train_y~train_x[,params]) # fit model using training data
        design = test_x[,params]
        design = cbind(rep(1,dim(test_x)[1]),design) # add a column of ones for the intercept
        predval=design%*%mmr$coef # by pmse model selction
        prederror=sum((test_y-predval)^2)/length(predval) # ---------------------------- ADDED
        predvals_pmse[,i]=predval
        pMSEs_mins[i]=prederror # --------------------------------- ADDED
        
    }
    
    output<-list(test_y_vecs,predvals_bwd,pMSEs_bwd,size_bwd,as.data.frame(table(sort(params_bwd))),
                 predvals_pmse,pMSEs_mins,size_pmse,as.data.frame(table(sort(params_pmse))))
    return(output)
}

frac=0.5 # proportion of TRAINING data
seeds=20
res_5=model_sel(frac,seeds)

frac=0.6 # proportion of TRAINING data
seeds=20
res_6=model_sel(frac,seeds)

frac=0.7 # proportion of TRAINING data
seeds=20
res_7=model_sel(frac,seeds)

frac=0.8 # proportion of TRAINING data
seeds=20
res_8=model_sel(frac,seeds)

frac=0.9 # proportion of TRAINING data
seeds=20
res_9=model_sel(frac,seeds)

# -----pMSE

par(mfrow=c(2,3),mar=c(3,4.5,4,2))

# frac=0.5
pmse_bwd=unlist(res_5[3])
min_pmse=unlist(res_5[7])
pmses=cbind(pmse_bwd,min_pmse)
boxplot(pmses,names=c("bwd search","pMSE sel"),main="frac=0.5",ylim=c(0.0075,0.025),ylab="pMSE",
        cex.main=1.5,cex.lab=1.5,cex.axis=1.1) # ADAPT

# frac=0.6
pmse_bwd=unlist(res_6[3])
min_pmse=unlist(res_6[7])
pmses=cbind(pmse_bwd,min_pmse)
boxplot(pmses,names=c("bwd search","pMSE sel"),main="frac=0.6",ylim=c(0.0075,0.025),
        cex.main=1.5,cex.axis=1.1) # ADAPT

# frac=0.7
pmse_bwd=unlist(res_7[3])
min_pmse=unlist(res_7[7])
pmses=cbind(pmse_bwd,min_pmse)
boxplot(pmses,names=c("bwd search","pMSE sel"),main="frac=0.7",ylim=c(0.0075,0.025),
        cex.main=1.5,cex.axis=1.1) # ADAPT

# frac=0.8
pmse_bwd=unlist(res_8[3])
min_pmse=unlist(res_8[7])
pmses=cbind(pmse_bwd,min_pmse)
boxplot(pmses,names=c("bwd search","pMSE sel"),main="frac=0.8",ylim=c(0.0075,0.025),
        cex.main=1.5,ylab="pMSE",cex.lab=1.5,cex.axis=1.1) # ADAPT

# frac=0.9
pmse_bwd=unlist(res_9[3])
min_pmse=unlist(res_9[7])
pmses=cbind(pmse_bwd,min_pmse)
boxplot(pmses,names=c("bwd search","pMSE sel"),main="frac=0.9",ylim=c(0.0075,0.025),
        cex.main=1.5,cex.axis=1.1) # ADAPT

# -----Model sizes
# NOTE: ALSO INCLUDES INTERCEPT

par(mfrow=c(2,3),mar=c(3,4.5,4,2))

# frac=0.5
size_bwd=unlist(res_5[4])
size_pmse=unlist(res_5[8])
mod_sizes=cbind(size_bwd,size_pmse)
boxplot(mod_sizes,names=c("bwd search","pMSE sel"),main="frac=0.5",ylim=c(0,12),ylab="Model size",
        cex.main=1.5,cex.lab=1.5,cex.axis=1.1) # ADAPT

# frac=0.6
size_bwd=unlist(res_6[4])
size_pmse=unlist(res_6[8])
mod_sizes=cbind(size_bwd,size_pmse)
boxplot(mod_sizes,names=c("bwd search","pMSE sel"),main="frac=0.6",ylim=c(0,12),
        cex.main=1.5,cex.axis=1.1) # ADAPT

# frac=0.7
size_bwd=unlist(res_7[4])
size_pmse=unlist(res_7[8])
mod_sizes=cbind(size_bwd,size_pmse)
boxplot(mod_sizes,names=c("bwd search","pMSE sel"),main="frac=0.7",ylim=c(0,12),
        cex.main=1.5,cex.axis=1.1) # ADAPT

# frac=0.8
size_bwd=unlist(res_8[4])
size_pmse=unlist(res_8[8])
mod_sizes=cbind(size_bwd,size_pmse)
boxplot(mod_sizes,names=c("bwd search","pMSE sel"),main="frac=0.8",ylim=c(0,12),ylab="Model size",
        cex.main=1.5,cex.lab=1.5,cex.axis=1.1) # ADAPT

# frac=0.9
size_bwd=unlist(res_9[4])
size_pmse=unlist(res_9[8])
mod_sizes=cbind(size_bwd,size_pmse)
boxplot(mod_sizes,names=c("bwd search","pMSE sel"),main="frac=0.9",ylim=c(0,12),
        cex.main=1.5,cex.axis=1.1) # ADAPT


# --- 1st preselection of variables: 11 covariates
# covars=c('bedrooms','bathrooms','floors','log_sqft_above','sqrt_sqft_basement','log_sqft_living15',
#         'log_sqft_lot15','sqrt_age','lat','long','log_dist_hav') # 11 pre-selected covariates

# --- 2nd preselection of variables: 8 covariates
covars=c('bedrooms','floors','sqrt_sqft_basement','log_sqft_living15',
         'log_sqft_lot15','sqrt_age','lat','log_dist_hav') # 8 pre-selected covariates
# -----Variables selected
par(mfrow=c(3,2))
# frac=0.5
varsel_bwd=as.data.frame(res_5[5])
names(varsel_bwd)=c("var","freq_bwd")
covar_names=as.data.frame(sort(covars))
names(covar_names)="var"
df=merge(covar_names, varsel_bwd, all=TRUE)
varsel_pmse=as.data.frame(res_5[9])
names(varsel_pmse)=c("var","freq_pmse")
df=merge(df,varsel_pmse,by="var",all=TRUE)
df[is.na(df)] = 0

par(mar=c(2,9,2,1))
barplot(df$freq_bwd,names.arg=df$var,las=2,cex.names=1,horiz=TRUE,
        main="backward search \nfrac=0.5",xlab="Count",xlim=c(0,seeds),cex.lab=1.2)
barplot(df$freq_pmse,las=2,horiz=TRUE,
        main="pMSE sel\nfrac=0.5")#,xlab="Count",xlim=c(0,seeds),cex.lab=1.2)

# frac=0.6
varsel_bwd=as.data.frame(res_6[5])
names(varsel_bwd)=c("var","freq_bwd")
covar_names=as.data.frame(sort(covars))
names(covar_names)="var"
df=merge(covar_names, varsel_bwd, all=TRUE)
varsel_pmse=as.data.frame(res_6[9])
names(varsel_pmse)=c("var","freq_pmse")
df=merge(df,varsel_pmse,by="var",all=TRUE)
df[is.na(df)] = 0
barplot(df$freq_bwd,names.arg=df$var,las=2,cex.names=1,horiz=TRUE,
        main="frac=0.6",xlab="Count",xlim=c(0,seeds),cex.lab=1.2)
barplot(df$freq_pmse,las=2,horiz=TRUE,
        main="frac=0.6")#,xlab="Count",xlim=c(0,seeds),cex.lab=1.2)


# frac=0.7
varsel_bwd=as.data.frame(res_7[5])
names(varsel_bwd)=c("var","freq_bwd")
covar_names=as.data.frame(sort(covars))
names(covar_names)="var"
df=merge(covar_names, varsel_bwd, all=TRUE)
varsel_pmse=as.data.frame(res_7[9])
names(varsel_pmse)=c("var","freq_pmse")
df=merge(df,varsel_pmse,by="var",all=TRUE)
df[is.na(df)] = 0

barplot(df$freq_bwd,names.arg=df$var,las=2,cex.names=1,horiz=TRUE,
        main="frac=0.7",xlab="Count",xlim=c(0,seeds))
barplot(df$freq_pmse,las=2,horiz=TRUE,
        main="frac=0.7",xlab="Count",xlim=c(0,seeds),cex.lab=1.2)

par(mfrow=c(2,2))
# frac=0.8
varsel_bwd=as.data.frame(res_8[5])
names(varsel_bwd)=c("var","freq_bwd")
covar_names=as.data.frame(sort(covars))
names(covar_names)="var"
df=merge(covar_names, varsel_bwd, all=TRUE)
varsel_pmse=as.data.frame(res_8[9])
names(varsel_pmse)=c("var","freq_pmse")
df=merge(df,varsel_pmse,by="var",all=TRUE)
df[is.na(df)] = 0
par(mar=c(5, 9, 2, 2))
barplot(df$freq_bwd,names.arg=df$var,las=2,cex.names=1,horiz=TRUE,
        main="frac=0.8")#,xlab="Count",xlim=c(0,seeds),cex.lab=1.2)
barplot(df$freq_pmse,las=2,,horiz=TRUE,
        main="frac=0.8")#,xlab="Count",xlim=c(0,seeds),cex.lab=1.2)


# frac=0.9
varsel_bwd=as.data.frame(res_9[5])
names(varsel_bwd)=c("var","freq_bwd")
covar_names=as.data.frame(sort(covars))
names(covar_names)="var"
df=merge(covar_names, varsel_bwd, all=TRUE)
varsel_pmse=as.data.frame(res_9[9])
names(varsel_pmse)=c("var","freq_pmse")
df=merge(df,varsel_pmse,by="var",all=TRUE)
df[is.na(df)] = 0
barplot(df$freq_bwd,names.arg=df$var,las=2,cex.names=1,horiz=TRUE,
        main="frac=0.9",xlab="Count",xlim=c(0,seeds),cex.lab=1.2)
barplot(df$freq_pmse,las=2,horiz=TRUE,
        main="frac=0.9",xlab="Count",xlim=c(0,seeds),cex.lab=1.2)
