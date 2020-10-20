### MSG500-MVE190 Linear Statistical Models 2018-19
### Project, part 1: Summary of minianalyses
### Margareta Carlerös & Devosmita Chatterjee

#set current directory
setwd("D:/M.SC. prog. in Engineering Mathematics and Computational Science/Project_Statistics")
getwd() 

housedata=read.csv("kc_house_data.csv")
dim(housedata) #      21613 21
head(housedata)

library(plyr)
bed_counts=count(housedata[,'bedrooms'])
bed_counts

# Only show data up to 10 bedrooms

null_vals=as.data.frame(cbind(seq(0,10),rep(0,max(11))))
names(null_vals)=c("x","freq_null")
freqs=merge(bed_counts[-c(12,13),], null_vals, all=TRUE)
freqs[is.na(freqs)] = 0
p=barplot(freqs[,2],names.arg=freqs[,1], xlab="Number of bedrooms",ylab="Frequency")
text(p, freqs[,2], labels=freqs[,2], xpd=TRUE,pos=3) 

# remove observations with >10 bedrooms
df=housedata[housedata[,"bedrooms"]<=10,]
dim(df) #      21611 21, 2 observations removed
head(df)
names(df)

# ---- age
mydate=df$date
yr_sold=as.numeric(format(as.Date(mydate,format="%Y%m%d"),"%Y")) # extract year from date
age=yr_sold-df$yr_built # NOTE: may be negative
df$age=age
df[age<0,"age"]=0 # if house sold before built set age of house at selling date to 0. (it's a new house!)

# -----dist_hav
library(geosphere)
p1=cbind(df$long,df$lat) # longitude,latitude
p2=matrix(c(-122.332069,47.606209),nrow=dim(df)[1],ncol=2,byrow=TRUE) # downtown Seattle coordinates
dist_hav=distHaversine(p1, p2) # distance in meters 'as the crow flies'
df$dist_hav=dist_hav

names(df)
dim(df) # 21611 23 

df$grade_cat=df$grade # make copy of grade
df$grade_cat[df$grade<=3]="poor"
df$grade_cat[(df$grade>3) & (df$grade<=6)]="lower"
df$grade_cat[df$grade==7]="average"
df$grade_cat[(df$grade>7) & (df$grade<=9)]="better"
df$grade_cat[df$grade>9]="higher"

df$grade_cat=factor(df$grade_cat,labels=c("average","better","higher","lower","poor")) # in alphabetical order
count(df$grade_cat)

# original variable as factor
df$grade=factor(df$grade)
count(df$grade) # with 1 as baseline

df$waterfront=factor(df$waterfront)
count(df$waterfront) # with no waterfront as baseline

# recoded
df$view_cat=df$view
count(df$view_cat)
df[df$view==4,]$view_cat="amazing"
df[(df$view==2)|(df$view==3),]$view_cat="ok"
df[(df$view==0)|(df$view==1),]$view_cat="poor"
count(df$view_cat)

# as factor
df$view_cat=factor(df$view_cat,labels=c("amazing","ok","poor")) # in alphabetical order!
df$view_cat=relevel(df$view_cat,ref="ok") # set baseline category
count(df$view_cat)


# original variable as factor
df$view=factor(df$view)
count(df$view) # with 0 as baseline

# recoded
df$condition_cat=df$condition
count(df$condition_cat)
df[(df$condition==5)|(df$condition==4),]$condition_cat="good"
df[(df$condition==3),]$condition_cat="ok"
df[(df$condition==1)|(df$condition==2),]$condition_cat="poor"
count(df$condition_cat)

# as factor
df$condition_cat=factor(df$condition_cat,labels=c("good","ok","poor")) # in alphabetical order!
df$condition_cat=relevel(df$condition_cat,ref="ok") # set baseline category
count(df$condition_cat)

# original variable as factor
df$condition=factor(df$condition)
count(df$condition) # with 1 as baseline

# lat
lat_bounds=quantile(df$lat,probs=c(1/3,2/3))
df$lat_cat=df$lat # make copy of lat
df[df$lat<=lat_bounds[1],]$lat_cat="low"
df[(df$lat>lat_bounds[1]) & (df$lat<=lat_bounds[2]),]$lat_cat="medium"
df[df$lat>lat_bounds[2],]$lat_cat="high"
count(df$lat_cat)

# long
long_bounds=quantile(df$long,probs=c(1/3,2/3))
df$long_cat=df$long # make copy of lat
df[df$long<=long_bounds[1],]$long_cat="low"
df[(df$long>long_bounds[1]) & (df$long<=long_bounds[2]),]$long_cat="medium"
df[df$long>long_bounds[2],]$long_cat="high"
count(df$long_cat)

# dist_hav
dist_bounds=quantile(df$dist_hav,probs=c(1/3,2/3))
df$dist_cat=df$long # make copy of lat
df[df$dist_hav<=dist_bounds[1],]$dist_cat="low"
df[(df$dist_hav>dist_bounds[1]) & (df$dist_hav<=dist_bounds[2]),]$dist_cat="medium"
df[df$dist_hav>dist_bounds[2],]$dist_cat="high"
count(df$dist_cat)

min(df$lat)
lat_bounds
max(df$lat)

min(df$long)
long_bounds
max(df$long)

min(df$dist_hav)
dist_bounds
max(df$dist_hav)

# as factor and relevel
# as factor

# lat_cat
df$lat_cat=factor(df$lat_cat,labels=c("high","low","medium")) # in alphabetical order!
df$lat_cat=relevel(df$lat_cat,ref="medium") # set baseline category
count(df$lat_cat)

# long_cat
df$long_cat=factor(df$long_cat,labels=c("high","low","medium")) # in alphabetical order!
df$long_cat=relevel(df$long_cat,ref="medium") # set baseline category
count(df$long_cat)

# dist_cat
df$dist_cat=factor(df$dist_cat,labels=c("high","low","medium")) # in alphabetical order!
df$dist_cat=relevel(df$dist_cat,ref="medium") # set baseline category
count(df$dist_cat)

# Group by neighborhood
# https://www.bestplaces.net/find/zip.aspx?county=53033&st=WA

df$neighborhood=df$zipcode
df$neighborhood[df$zipcode== 98178]="Bryn Mawr-Skyway"
df$neighborhood[df$zipcode== 98125]="Seattle"
df$neighborhood[df$zipcode== 98028]="Kenmore"
df$neighborhood[df$zipcode== 98136]="Seattle"
df$neighborhood[df$zipcode== 98074]="Sammamish"
df$neighborhood[df$zipcode== 98053]="Union Hill-Novelty Hill"

df$neighborhood[df$zipcode== 98003]="Federal Way"
df$neighborhood[df$zipcode== 98198]="Des Moines"
df$neighborhood[df$zipcode== 98146]="White Center"
df$neighborhood[df$zipcode== 98038]="Maple Valley"
df$neighborhood[df$zipcode== 98007]="Bellevue"
df$neighborhood[df$zipcode== 98115]="Seattle"

df$neighborhood[df$zipcode== 98107]="Seattle"
df$neighborhood[df$zipcode== 98126]="Seattle"
df$neighborhood[df$zipcode== 98019]="Duvall"
df$neighborhood[df$zipcode== 98103]="Seattle"
df$neighborhood[df$zipcode== 98002]="Auburn"
df$neighborhood[df$zipcode== 98133]="Seattle"

df$neighborhood[df$zipcode== 98040]="Mercer Island"
df$neighborhood[df$zipcode== 98092]="Auburn"
df$neighborhood[df$zipcode== 98030]="Kent"
df$neighborhood[df$zipcode== 98119]="Seattle"
df$neighborhood[df$zipcode== 98112]="Seattle"
df$neighborhood[df$zipcode== 98052]="Redmond"

df$neighborhood[df$zipcode== 98027]="Issaquah"
df$neighborhood[df$zipcode== 98117]="Seattle"
df$neighborhood[df$zipcode== 98058]="Fairwood"
df$neighborhood[df$zipcode== 98001]="Lakeland North"
df$neighborhood[df$zipcode== 98056]="Renton"
df$neighborhood[df$zipcode== 98166]="Burien"

df$neighborhood[df$zipcode== 98023]="Federal Way"
df$neighborhood[df$zipcode== 98070]="Vashon"
df$neighborhood[df$zipcode== 98148]="Burien"
df$neighborhood[df$zipcode== 98105]="Seattle"
df$neighborhood[df$zipcode== 98042]="Covington"
df$neighborhood[df$zipcode== 98008]="Bellevue"

df$neighborhood[df$zipcode== 98059]="Renton"
df$neighborhood[df$zipcode== 98122]="Seattle"
df$neighborhood[df$zipcode== 98144]="Seattle"
df$neighborhood[df$zipcode== 98004]="Bellevue"
df$neighborhood[df$zipcode== 98005]="Bellevue"
df$neighborhood[df$zipcode== 98034]="Kirkland"

df$neighborhood[df$zipcode== 98075]="Sammamish"
df$neighborhood[df$zipcode== 98116]="Seattle"
df$neighborhood[df$zipcode== 98010]="Black Diamond"
df$neighborhood[df$zipcode== 98118]="Seattle"
df$neighborhood[df$zipcode== 98199]="Seattle"
df$neighborhood[df$zipcode== 98032]="Kent"

df$neighborhood[df$zipcode== 98045]="North Bend"
df$neighborhood[df$zipcode== 98102]="Seattle"
df$neighborhood[df$zipcode== 98077]="Cottage Lake"
df$neighborhood[df$zipcode== 98108]="Seattle"
df$neighborhood[df$zipcode== 98168]="Tukwila"
df$neighborhood[df$zipcode== 98177]="Shoreline"

df$neighborhood[df$zipcode== 98065]="Snoqualmie"
df$neighborhood[df$zipcode== 98029]="Issaquah"
df$neighborhood[df$zipcode== 98006]="Bellevue"
df$neighborhood[df$zipcode== 98109]="Seattle"
df$neighborhood[df$zipcode== 98022]="Enumclaw"
df$neighborhood[df$zipcode== 98033]="Kirkland"

df$neighborhood[df$zipcode== 98155]="Shoreline"
df$neighborhood[df$zipcode== 98024]="Fall City"
df$neighborhood[df$zipcode== 98011]="Bothell"
df$neighborhood[df$zipcode== 98031]="East Hill-Meridian"
df$neighborhood[df$zipcode== 98106]="Seattle"
df$neighborhood[df$zipcode== 98072]="Woodinville"

df$neighborhood[df$zipcode== 98188]="SeaTac"
df$neighborhood[df$zipcode== 98014]="Carnation"
df$neighborhood[df$zipcode== 98055]="Renton"
df$neighborhood[df$zipcode== 98039]="Medina"
count(df$neighborhood)

# as factor
df$neighborhood=factor(df$neighborhood) # in alphabetical order
df$neighborhood=relevel(df$neighborhood,ref="Seattle") # set Seattle as baseline
count(df$neighborhood)
dim(count(df$neighborhood))[1] # 37 categories

dim(df) # 21611 30 

seed=1
set.seed(seed)

num_obs=dim(df)[1] # ALL
frac=0.7 # ADAPT: for now just consider training set

# --- TRAIN
ii<-sample(seq(1,dim(df)[1]),round(num_obs*frac))
train=df[ii,] # note: includes all variables (+ bedrooms)
row.names(train)<-seq(1,round(num_obs*frac))
train_y=train[,4] # bedrooms
dim(train) #      15128 30 

# --- TEST
test=df[-ii,]
row.names(test)<-seq(round(num_obs*frac)+1,num_obs)
test_y=test[,4] # bedrooms
dim(test) #     6483 30 
test_x=test[,-4]

head(train)
names(train)
dim(train) #      15128 30 

par(mfrow=c(2,3))

plot(train$sqft_living,train$bedrooms,main="bedrooms vs sqft_living")
plot(train$sqft_above,train$bedrooms,main="bedrooms vs sqft_above")
plot(train$sqft_basement,train$bedrooms,main="bedrooms vs sqft_basement")
plot(train$sqft_lot,train$bedrooms,main="bedrooms vs sqft_lot")
plot(train$sqft_living15,train$bedrooms,main="bedrooms vs sqft_living15")
plot(train$sqft_lot15,train$bedrooms,main="bedrooms vs sqft_lot15")

plot(train$view,train$bedrooms,main="bedrooms vs view")
plot(train$view_cat,train$bedrooms,main="bedrooms vs view_cat")
plot(train$condition,train$bedrooms,main="bedrooms vs condition")
plot(train$condition_cat,train$bedrooms,main="bedrooms vs condition_cat")
plot(train$grade,train$bedrooms,main="bedrooms vs grade")
plot(train$grade_cat,train$bedrooms,main="bedrooms vs grade_cat")

plot(train$dist_hav,train$bedrooms,main="bedrooms vs dist_hav")
plot(train$dist_cat,train$bedrooms,main="bedrooms vs dist_cat")
plot(train$lat,train$bedrooms,main="bedrooms vs lat")
plot(train$lat_cat,train$bedrooms,main="bedrooms vs lat_cat")
plot(train$long,train$bedrooms,main="bedrooms vs long")
plot(train$long_cat,train$bedrooms,main="bedrooms vs long_cat")

plot(train$price,train$bedrooms,main="bedrooms vs. price")
plot(train$bathrooms,train$bedrooms,main="bedrooms vs bathrooms")
plot(factor(train$floors),train$bedrooms,main="bedrooms vs floors")
plot(train$waterfront,train$bedrooms,main="bedrooms vs waterfront")
plot(train$age,train$bedrooms,main="bedrooms vs age")

# log of response
par(mfrow=c(2,3))

plot(train$sqft_living,log(train$bedrooms),main="log(bedrooms) vs sqft_living")
plot(train$sqft_above,log(train$bedrooms),main="log(bedrooms) vs sqft_above")
plot(train$sqft_basement,log(train$bedrooms),main="log(bedrooms) vs sqft_basement")
plot(train$sqft_lot,log(train$bedrooms),main="log(bedrooms) vs sqft_lot")
plot(train$sqft_living15,log(train$bedrooms),main="log(bedrooms) vs sqft_living15")
plot(train$sqft_lot15,log(train$bedrooms),main="log(bedrooms) vs sqft_lot15")

plot(train$view,log(train$bedrooms),main="log(bedrooms) vs view")
plot(train$view_cat,log(train$bedrooms),main="log(bedrooms) vs view_cat")
plot(train$condition,log(train$bedrooms),main="log(bedrooms) vs condition")
plot(train$condition_cat,log(train$bedrooms),main="log(bedrooms) vs condition_cat")
plot(train$grade,log(train$bedrooms),main="log(bedrooms) vs grade")
plot(train$grade_cat,log(train$bedrooms),main="log(bedrooms) vs grade_cat")

plot(train$dist_hav,log(train$bedrooms),main="log(bedrooms) vs dist_hav")
plot(train$dist_cat,log(train$bedrooms),main="log(bedrooms) vs dist_cat")
plot(train$lat,log(train$bedrooms),main="log(bedrooms) vs lat")
plot(train$lat_cat,log(train$bedrooms),main="log(bedrooms) vs lat_cat")
plot(train$long,log(train$bedrooms),main="log(bedrooms) vs long")
plot(train$long_cat,log(train$bedrooms),main="log(bedrooms) vs long_cat")

plot(train$price,log(train$bedrooms),main="log(bedrooms) vs. price")
plot(train$bathrooms,log(train$bedrooms),main="log(bedrooms) vs bathrooms")
plot(factor(train$floors),log(train$bedrooms),main="log(bedrooms) vs floors")
plot(train$waterfront,log(train$bedrooms),main="log(bedrooms) vs waterfront")
plot(train$age,log(train$bedrooms),main="log(bedrooms) vs age")

sum(train$sqft_basement==0)/dim(train)[1]

names(train)
covars1=c("sqft_living","sqft_above","condition","condition_cat","grade_cat","bathrooms")

# 9 continuous variables
covars_cat=c("sqft_above","bathrooms","sqft_living","price","floors","sqft_living15",
             "sqft_basement")
library(GGally)
par(mar=c(4,1,9,1), mgp = c(5, 1, 0))
ggpairs(data=train,columns=covars_cat)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






covars_cat2=c("sqft_above","bathrooms","price","floors")

ggpairs(data=train,columns=covars_cat2)

# comparison of categorical variables - chi2-test of indep.
# H0: indep, H1: some correlation
# https://datascience.stackexchange.com/questions/893/how-to-get-correlation-between-two-categorical-variable-and-a-categorical-variab

# categorical variables to test: grade_cat, condition_cat, view_cat

# grade_cat vs. condition_cat
table(df$grade_cat,df$condition_cat)
chi2 = chisq.test(x=df$grade_cat,y=df$condition_cat)
c(chi2$statistic, chi2$p.value)
# p=4e-255 --> Reject H0, s there is some correlation between at least 1 pair

# grade_cat vs. view_cat
table(df$grade_cat,df$view_cat)
chi2 = chisq.test(x=df$grade_cat,y=df$view_cat)
c(chi2$statistic, chi2$p.value)
# p=4e-269 --> Reject H0, s there is some correlation between at least 1 pair

# condition_cat vs. view_cat
table(df$condition_cat,df$view_cat)
chi2 = chisq.test(x=df$condition_cat,y=df$view_cat)
c(chi2$statistic, chi2$p.value)
# p=2e-7 --> Reject H0, s there is some correlation between at least 1 pair

# comparison of categorical vs. numerical variables using ANOVA

# --- SQFT_ABOVE---

aov1 = aov(df$sqft_above ~ df$grade_cat)
summary(aov1) # p<2e-16 --> so there is correlation between sqft_above and grade_cat

aov1 = aov(df$sqft_above ~ df$condition_cat)
summary(aov1) # p<2e-16 --> so there is correlation between sqft_above and condition_cat

aov1 = aov(df$sqft_above ~ df$view_cat)
summary(aov1) # p<2e-16 --> so there is correlation between sqft_above and view_cat

# --- PRICE---

aov1 = aov(df$price ~ df$grade_cat)
summary(aov1) # p<2e-16 --> so there is correlation between price and grade_cat

aov1 = aov(df$price ~ df$condition_cat)
summary(aov1) # p<1.78e-15 --> so there is correlation between price and condition_cat

aov1 = aov(df$price ~ df$view_cat)
summary(aov1) # p<2e-16 --> so there is correlation between price and view_cat

# --- FLOORS---

aov1 = aov(df$floors ~ df$grade_cat)
summary(aov1) # p<2e-16 --> so there is correlation between floors and grade_cat

aov1 = aov(df$floors ~ df$condition_cat)
summary(aov1) # p<2e-16 --> so there is correlation between floors and condition_cat

aov1 = aov(df$floors ~ df$view_cat)
summary(aov1) # p<9.26e-06 --> so there is correlation between floors and view_cat

# --- BATHROOMS---

aov1 = aov(df$bathrooms ~ df$grade_cat)
summary(aov1) # p<2e-16 --> so there is correlation between bathrooms and grade_cat

aov1 = aov(df$bathrooms ~ df$condition_cat)
summary(aov1) # p<2e-16 --> so there is correlation between bathrooms and condition_cat

aov1 = aov(df$bathrooms ~ df$view_cat)
summary(aov1) # p<2e-16 --> so there is correlation between bathrooms and view_cat

library(car) # vif
library("AER") # test for overdispersion/underdispersion

summary(p0 <- glm(bedrooms ~ grade_cat , data=train,family="poisson")) # ADAPT
model=p0 # ADAPT

# ---- Dispersion test
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test
# average is baseline
exp(0.09)
exp(0.20)
exp(-0.21)
exp(-1.47)
# Relative to average:
# - 9% more bedrooms if grade better
# - 22% more bedrooms if grade higher
# - 19% fewer bedrooms if grade lower
# - 78% fewer bedrooms if grade poor
# All are significant

# --- VIF
# vif(model) # only 1 covariate so can't perform this here

###-----------------------
summary(pq0 <- glm(bedrooms ~ grade_cat , data=train,family="quasipoisson"))

exp(0.095839)
exp(0.208641)
exp(-0.198097)
exp(-1.584391)

summary(p1 <- glm(bedrooms~grade_cat+sqft_above,data=train,family="poisson")) # ADAPT

model=p1 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test-------------------------------------------------------

# all significant except grade==better (relative to grade==average)
exp(-0.01)
exp(-0.11)
exp(-0.15)
exp(-1.32)
exp(0.0002)
# Relative to average grade:
# - 1% fewer bedrooms if grade better (NOT SIGNIFICANT)
# - 10% fewer bedrooms if grade higher
# - 14% fewer bedrooms if grade lower
# - 73% fewer bedrooms if grade poor

# - 0.02% more bedrooms for each 1 sqft increase in sqft_above

# ---- VIF -------------------------------------------------------------
vif(model) # OK?

# ---- Likelihood ratio test --------------------------------------------
# should we add sqft_above to model containing grade_cat?
anova(p0,p1) # Diff. in deviance: 650.1892
qchisq(0.95,1) # 3.84
# D_diff > 3.84, reject H0=reduced model. Need sqft_above covariate.

# ------Quasi----------
summary(pq1 <- glm(bedrooms~grade_cat+sqft_above,data=train,family="quasipoisson"))
anova(pq0,pq1)

summary(p2 <- glm(bedrooms~grade_cat+condition_cat,data=train,family="poisson")) # ADAPT

model=p2 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------
# all significant except condition==poor (relative to grade==average & condition==ok)
exp(0.10)
exp(0.21)
exp(-0.21)
exp(-1.44)
exp(0.04)
exp(-0.05)
# Relative to average grade & ok condition:
# - 11% more bedrooms if grade better
# - 23% more bedrooms if grade higher
# - 19% fewer bedrooms if grade lower
# - 76% fewer bedrooms if grade poor
# - 4% more bedrooms if condition good
# - 5% fewer bedrooms if condition poor

# ---- VIF -------------------------------------------------------------
vif(model) # OK?

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p0,p2) # Diff. in deviance: 24.95023
qchisq(0.95,2) # 3.84
# D_diff > 3.84. reject H0=reduced model. Need condition covariate.

# ------Quasi----------
summary(pq2 <- glm(bedrooms~grade_cat+condition_cat,data=train,family="quasipoisson"))
anova(pq0,pq2)

summary(p3 <- glm(bedrooms~grade_cat+bathrooms,data=train,family="poisson")) # ADAPT

model=p3 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p0,p3) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

# https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif

summary(p4 <- glm(bedrooms~sqft_above,data=train,family="poisson")) # ADAPT

model=p4 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
# vif(model) # can't be calculated here

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p4,p1) # Diff. in deviance: ...
qchisq(0.95,4) # 3.84
# D_diff ... 3.84

summary(p5 <- glm(bedrooms~condition_cat,data=train,family="poisson")) # ADAPT

model=p5 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
# vif(model) # can't compute this here

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p5,p2) # Diff. in deviance: ...
qchisq(0.95,4) # 3.84
# D_diff ... 3.84

summary(p6 <- glm(bedrooms~bathrooms,data=train,family="poisson")) # ADAPT

model=p6 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
#vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p6,p3) # Diff. in deviance: ...
qchisq(0.95,4) # 3.84
# D_diff ... 3.84

summary(p7 <- glm(bedrooms~sqft_above+bathrooms,data=train,family="poisson")) # ADAPT

model=p7 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p4,p7) # Diff. in deviance: ...
anova(p6,p7) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p8 <- glm(bedrooms~grade_cat+sqft_above+bathrooms,data=train,family="poisson")) # ADAPT

model=p8 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p7,p8) # Diff. in deviance: ...
anova(p1,p8)
anova(p3,p8)
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

sqft_above+condition_cat+
  grade_cat+bathrooms+view_cat+sqft_living+
  sqft_basement+price+floors

summary(p9 <- glm(bedrooms~sqft_above++sqft_living+condition_cat+
                    grade_cat+bathrooms+view_cat+
                    price+floors+sqft_lot+sqft_living15+sqft_lot15,data=train,family="poisson")) # ADAPT

model=p9 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p1,p9) # Diff. in deviance: ...
qchisq(0.95,11) # note: k=10
# D_diff ... 3.84

step(p9,trace=FALSE) # wierd
step(p9,trace=FALSE,direction='forward') # wierd

summary(p10 <- glm(bedrooms~grade_cat+sqft_above+bathrooms+price,data=train,family="poisson")) # ADAPT

model=p10 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p8,p10) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p11 <- glm(bedrooms~grade_cat+sqft_above+bathrooms+floors,data=train,family="poisson")) # ADAPT

model=p11 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p8,p11) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p12 <- glm(bedrooms~grade_cat+price,data=train,family="poisson")) # ADAPT

model=p12 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p0,p12) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p13 <- glm(bedrooms~grade_cat+floors,data=train,family="poisson")) # ADAPT

model=p13 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p0,p13) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p14 <- glm(bedrooms~grade_cat+view_cat,data=train,family="poisson")) # ADAPT

model=p14 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p0,p14) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

summary(p15 <- glm(bedrooms~grade_cat+bathrooms+sqft_above,data=train,family="poisson")) # ADAPT

model=p15 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p3,p15) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

###------ Quasi -----
summary(pq15 <- glm(bedrooms~grade_cat+bathrooms+sqft_above,data=train,family="quasipoisson"))

summary(p16 <- glm(bedrooms~grade_cat+bathrooms+price,data=train,family="poisson")) # ADAPT

model=p16 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p3,p16) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p17 <- glm(bedrooms~grade_cat+bathrooms+condition_cat,data=train,family="poisson")) # ADAPT

model=p17 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p3,p17) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

summary(p18 <- glm(bedrooms~grade_cat+bathrooms+floors,data=train,family="poisson")) # ADAPT

model=p18 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p3,p18) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p19 <- glm(bedrooms~grade_cat+bathrooms+view_cat,data=train,family="poisson")) # ADAPT

model=p19 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p3,p19) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p20 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+condition_cat,data=train,family="poisson")) # ADAPT

model=p20 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p15,p20) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

summary(p21 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors,data=train,family="poisson")) # ADAPT

model=p21 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p15,p21) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

#### ----- Quasi ----
summary(pq21 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors,data=train,family="quasipoisson"))
anova(pq15,pq21)

summary(p22 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+price,data=train,family="poisson")) # ADAPT

model=p22 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p15,p22) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p23 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+view_cat,data=train,family="poisson")) # ADAPT

model=p23 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p15,p23) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

summary(p24 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat,data=train,family="poisson")) # ADAPT

model=p24 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p21,p24) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84


#####------Quasi-----
summary(pq24 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat,data=train,family="quasipoisson"))

summary(p25 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+view_cat,data=train,family="poisson")) # ADAPT

model=p25 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p21,p25) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

summary(p26 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+price,data=train,family="poisson")) # ADAPT

model=p26 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p21,p26) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

### ---- quasi ----
summary(pq26 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+price,data=train,family="quasipoisson"))
anova(pq21,pq26)

### --- quasi + interactions ----
summary(pqi26 <- glm(bedrooms~grade_cat*bathrooms*sqft_above*floors*price,data=train,family="quasipoisson"))

summary(p27 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat+view_cat,data=train,family="poisson")) # ADAPT

model=p27 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p24,p27) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

summary(p28 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat+price,data=train,family="poisson")) # ADAPT

model=p28 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p24,p28) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

### ----- Quasi -----
summary(pq28 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat+price,data=train,family="quasipoisson"))
anova(pq24,pq28)

summary(p29 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat+price+view_cat,data=train,family="poisson")) # ADAPT

model=p29 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p28,p29) # Diff. in deviance: ...
qchisq(0.95,2) # 3.84
# D_diff ... 3.84

#----Quasi---
summary(pq29 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat+price+
                      view_cat,data=train,family="quasipoisson"))
anova(pq28,pq29)

summary(p30 <- glm(bedrooms~bathrooms+sqft_above,data=train,family="poisson")) # ADAPT

model=p30 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p6,p30) # Diff. in deviance: ... # compared to model with only bathrooms
anova(p4,p30) # compared to model with only sqft_above
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p31 <- glm(bedrooms~bathrooms+price,data=train,family="poisson")) # ADAPT

model=p31 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p6,p31) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p32 <- glm(bedrooms~bathrooms+floors,data=train,family="poisson")) # ADAPT

model=p32 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p6,p32) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p33 <- glm(bedrooms~bathrooms+condition_cat,data=train,family="poisson")) # ADAPT

model=p33 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p6,p33) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p34 <- glm(bedrooms~bathrooms+view_cat,data=train,family="poisson")) # ADAPT

model=p34 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p6,p34) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p35 <- glm(bedrooms~bathrooms+sqft_above+grade_cat,data=train,family="poisson")) # ADAPT

model=p35 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p30,p35) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p36 <- glm(bedrooms~bathrooms+sqft_above+condition_cat,data=train,family="poisson")) # ADAPT

model=p36 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p30,p36) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p37 <- glm(bedrooms~bathrooms+sqft_above+floors,data=train,family="poisson")) # ADAPT

model=p37 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p30,p37) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p38 <- glm(bedrooms~bathrooms+sqft_above+view_cat,data=train,family="poisson")) # ADAPT

model=p38 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p30,p38) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p39 <- glm(bedrooms~bathrooms+sqft_above+price,data=train,family="poisson")) # ADAPT

model=p39 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p30,p39) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p40 <- glm(bedrooms~sqft_above+floors,data=train,family="poisson")) # ADAPT

model=p40 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p4,p40) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p41 <- glm(bedrooms~sqft_above+price,data=train,family="poisson")) # ADAPT

model=p41 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p4,p41) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p42 <- glm(bedrooms~sqft_above+condition_cat,data=train,family="poisson")) # ADAPT

model=p42 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p4,p42) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p43 <- glm(bedrooms~sqft_above+view_cat,data=train,family="poisson")) # ADAPT

model=p43 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p4,p43) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p44 <- glm(bedrooms~floors,data=train,family="poisson")) # ADAPT

model=p44 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model)

anova(p44,p40) # include sqft_above?
anova(p44,p32) # include bathrooms?
anova(p44,p13) # include grade_cat?

qchisq(0.95,1) # 3.84

summary(p45 <- glm(bedrooms~floors+price,data=train,family="poisson")) # ADAPT

model=p45 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p44,p45) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p46 <- glm(bedrooms~floors+condition_cat,data=train,family="poisson")) # ADAPT

model=p46 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p44,p46) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p47 <- glm(bedrooms~floors+view_cat,data=train,family="poisson")) # ADAPT

model=p47 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p44,p47) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p48 <- glm(bedrooms~floors+bathrooms+sqft_above,data=train,family="poisson")) # ADAPT

model=p48 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p32,p48) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p32,p18) # include grade_cat?
qchisq(0.95,1) # 3.84

summary(p49 <- glm(bedrooms~floors+bathrooms+price,data=train,family="poisson")) # ADAPT

model=p49 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p32,p49) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p50 <- glm(bedrooms~floors+bathrooms+condition_cat,data=train,family="poisson")) # ADAPT

model=p50 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p32,p50) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p51 <- glm(bedrooms~floors+bathrooms+view_cat,data=train,family="poisson")) # ADAPT

model=p51 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p32,p51) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p48,p21) # include grade_cat?
qchisq(0.95,1) # 3.84

summary(p52 <- glm(bedrooms~floors+bathrooms+sqft_above+price,data=train,family="poisson")) # ADAPT

model=p52 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p48,p52) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p53 <- glm(bedrooms~floors+bathrooms+sqft_above+condition_cat,data=train,family="poisson")) # ADAPT

model=p53 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p48,p53) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p54 <- glm(bedrooms~floors+bathrooms+sqft_above+view_cat,data=train,family="poisson")) # ADAPT

model=p54 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p48,p54) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p5,p33) # bathrooms
anova(p5,p42) # sqft_above
anova(p5,p46) # floors
anova(p5,p2) # grade_cat

summary(p55 <- glm(bedrooms~condition_cat+price,data=train,family="poisson")) # ADAPT

model=p55 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p5,p55) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p56 <- glm(bedrooms~condition_cat+view_cat,data=train,family="poisson")) # ADAPT

model=p56 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p5,p56) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p33,p36) # include sqft_above?
anova(p33,p17) # include grade_cat?

summary(p57 <- glm(bedrooms~condition_cat+bathrooms+floors,data=train,family="poisson")) # ADAPT

model=p57 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p33,p57) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p58 <- glm(bedrooms~condition_cat+bathrooms+price,data=train,family="poisson")) # ADAPT

model=p58 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p33,p58) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p59 <- glm(bedrooms~condition_cat+bathrooms+view_cat,data=train,family="poisson")) # ADAPT

model=p59 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p33,p59) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p36,p20) # include grade_cat?

summary(p60 <- glm(bedrooms~condition_cat+bathrooms+sqft_above+floors,data=train,family="poisson")) # ADAPT

model=p60 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p36,p60) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p61 <- glm(bedrooms~condition_cat+bathrooms+sqft_above+view_cat,data=train,family="poisson")) # ADAPT

model=p61 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p36,p61) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p62 <- glm(bedrooms~condition_cat+bathrooms+sqft_above+price,data=train,family="poisson")) # ADAPT

model=p62 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p36,p62) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p20,p24) # include floors?

summary(p63 <- glm(bedrooms~condition_cat+bathrooms+sqft_above+grade_cat+view_cat,data=train,family="poisson")) # ADAPT

model=p63 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p20,p63) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84


summary(p64 <- glm(bedrooms~condition_cat+bathrooms+sqft_above+grade_cat+price,data=train,family="poisson")) # ADAPT

model=p64 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p20,p64) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p65 <- glm(bedrooms~price,data=train,family="poisson")) # ADAPT

model=p65 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model)

anova(p65,p31) # include bathrooms?
anova(p65,p41) # include sqft_above?
anova(p65,p45) # include floors?
anova(p65,p12) # include grade_cat?
anova(p65,p55) # include condition_cat?


summary(p66 <- glm(bedrooms~price+view_cat,data=train,family="poisson")) # ADAPT

model=p66 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p65,p66) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p31,p39) # include sqft_above?
anova(p31,p49) # include floors?
anova(p31,p16) # include grade_cat?

summary(p67 <- glm(bedrooms~price+bathrooms+condition_cat,data=train,family="poisson")) # ADAPT

model=p67 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p31,p67) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p68 <- glm(bedrooms~price+bathrooms+view_cat,data=train,family="poisson")) # ADAPT

model=p68 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p31,p68) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p39,p22) # include grade_cat?
anova(p39,p52) # include floors?
anova(p39,p62) # include condition_cat?

summary(p69 <- glm(bedrooms~price+bathrooms+sqft_above+view_cat,data=train,family="poisson")) # ADAPT

model=p69 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p39,p69) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p22,p26) # include floors?
anova(p22,p64) # include condition_cat?


summary(p70 <- glm(bedrooms~price+bathrooms+sqft_above+grade_cat+view_cat,data=train,family="poisson")) # ADAPT

model=p70 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p22,p70) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

anova(p26,p28) # include condition_cat?

summary(p71 <- glm(bedrooms~price+bathrooms+sqft_above+grade_cat+floors+view_cat,data=train,family="poisson")) # ADAPT

model=p71 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model) # No overdispersion

# ---- Wald test-------------------------------------------------------

# ---- VIF -------------------------------------------------------------
vif(model) # 

# ---- Likelihood ratio test --------------------------------------------
# should we add ... to model containing ...?
anova(p26,p71) # Diff. in deviance: ...
qchisq(0.95,1) # 3.84
# D_diff ... 3.84

summary(p72 <- glm(bedrooms~view_cat,data=train,family="poisson")) # ADAPT

model=p72 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model)

anova(p72,p34) # include bathrooms?
anova(p72,p43) # include sqft_above?
anova(p72,p47) # include floors?
anova(p72,p14) # include grade_cat?
anova(p72,p56) # include condition_cat?
anova(p72,p66) # include price?


anova(p34,p38) # include sqft_above?
anova(p34,p51) # include floors?
anova(p34,p19) # include grade_cat?
anova(p34,p59) # include condition_cat?
anova(p34,p68) # include price?

anova(p38,p54) # include floors?
anova(p38,p23) # include grade_cat?
anova(p38,p61) # include condition_cat?
anova(p38,p69) # include price?
qchisq(0.95,2)
qchisq(0.95,4)

anova(p23,p25) # include floors?
anova(p23,p63) # include condition_cat?
anova(p23,p70) # include price?



anova(p25,p27) # include condition_cat?
anova(p25,p71) # include price?

anova(p27,p29)

summary(pq15)
exp(-0.046)
exp(-0.16)
exp(-0.09)
exp(-1.30)
exp(0.13)
exp(0.00009)

# Wald test, interpretation
# Relative to average grade:
# - 4.5% fewer bedrooms if grade better
# - 15% fewer bedrooms if grade higher
# - 9% fewer bedrooms if grade lower
# - 73% fewer bedrooms if grade poor

# - 14% more bedrooms for each extra bathroom
# - same number of bedrooms if increase sqft_above 1 sqft

# Influence measures:

model=pq15 # ADAPT

infl<-influence(model)
d<-cooks.distance(model)

par(mfrow=c(1,2))
## standardized residuals:
r_std<-infl$pear.res/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),main="Standardised residuals")
z<-qnorm(0.05/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")

# Cook's distance:
plot(d,ylim=c(0,1.2),xlab="i",ylab="D",main="Cook's distance")
abline(h=1,col="red")

# 4/dim(train)[1]

y_hat=exp(predict(pq15,newdata=test_x))
plot(test_y,y_hat,ylim=c(0,9),xlim=c(0,9))
abline()
length(y_hat)
length(test_y)
pmse=sum((y_hat-test_y)^2)/length(test_y)
pmse

summary(pq21)
exp(-2.963e-02)
exp(-1.608e-01)
exp(-9.167e-02)
exp(-1.287e+00)
exp(1.386e-01)
exp(1.092e-04)
exp(-8.442e-02)

# Wald test, interpretation
# Relative to average grade:
# - 3% fewer bedrooms if grade better
# - 15% fewer bedrooms if grade higher
# - 9% fewer bedrooms if grade lower
# - 72% fewer bedrooms if grade poor

# - 15% more bedrooms for each extra bathroom added
# - same number of bedrooms if increase sqft_above by 1 sqft
# - 8% fewer bedrooms for each extra floor added

# Influence measures:

model=pq21 # ADAPT

infl<-influence(model)
d<-cooks.distance(model)

par(mfrow=c(1,2))
## standardized residuals:
r_std<-infl$pear.res/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),main="Standardised residuals")
z<-qnorm(0.05/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")

# Cook's distance:
plot(d,ylim=c(0,1.2),xlab="i",ylab="D",main="Cook's distance")
abline(h=1,col="red")

# 4/dim(train)[1]

y_hat=exp(predict(pq21,newdata=test_x))
plot(test_y,y_hat,ylim=c(0,9),xlim=c(0,9))
abline()
length(y_hat)
length(test_y)
pmse=sum((y_hat-test_y)^2)/length(test_y)
pmse

# Build model
summary(pq26 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+price,data=train,family="quasipoisson"))
exp(-2.743e-02)
exp(-1.476e-01)
exp(-9.167e-02)
exp(-1.286e+00)
exp(1.417e-01)
exp(1.136e-04)
exp(-8.767e-02)
exp(-2.707e-08)
# Wald test, interpretation
# Relative to average grade:
# - 3% fewer bedrooms if grade better
# - 14% fewer bedrooms if grade higher
# - 9% fewer bedrooms if grade lower
# - 72% fewer bedrooms if grade poor

# - 15% more bedrooms for each extra bathroom added
# - same number of bedrooms if increase sqft_above by 1 sqft
# - 8% fewer bedrooms for each extra floor added
# - same number of bedrooms if increase price by $1!

# Influence measures:

model=pq26 # ADAPT

infl<-influence(model)
d<-cooks.distance(model)

par(mfrow=c(1,2))
## standardized residuals:
r_std<-infl$pear.res/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),main="Standardised residuals")
z<-qnorm(0.05/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")

# Cook's distance:
#plot(d,ylim=c(0,0.002),xlab="i",ylab="D",main="Cook's distance")
plot(d,ylim=c(0,0.03),xlab="i",ylab="D",main="Cook's distance")
#abline(h=4/dim(train)[1],col="red")
abline(h=0.005,col="red")


# Outliers from standardized residuals
num_sr=length(r_std[r_std>-z])

# Outliers from Cook's distance plot
num_cd=length(d[d>0.005])

(num_sr+num_cd)/dim(train)[1]

y_hat=exp(predict(pq26,newdata=test_x))
plot(test_y,y_hat,ylim=c(0,9),xlim=c(0,9), main="Predicted vs. observed number of bedrooms",
     xlab="Observed number of bedrooms", ylab="Predicted number of bedrooms")
abline(0,1,col="red",lty=2)
length(y_hat)
length(test_y)
pmse=sum((y_hat-test_y)^2)/length(test_y)
pmse

######### p26

y_hat=exp(predict(p26,newdata=test_x))
plot(test_y,y_hat,ylim=c(0,9),xlim=c(0,9))
abline(0,1,col="red",lty=2)
length(y_hat)
length(test_y)
pmse=sum((y_hat-test_y)^2)/length(test_y)
pmse

### CONCLUSION: same as quasipoisson

# Training data with outliers removed
dim(train)
train2=train[(r_std<=-z)&(d<=0.005),]
dim(train)[1]-dim(train2)[1]
dim(train2)

# Build model
summary(pq26 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+price,data=train2,family="quasipoisson")) # train2

# Remove poor grade from test data
names(test_x)
dim(test)
library(dplyr)
test2 <- filter(test, grade_cat!="poor")
dim(test2)

test2_x=test2[,-4]
names(test2_x)

test2_y=test2[,4]
length(test2_y)

y_hat=exp(predict(pq26,newdata=test2_x))
plot(test2_y,y_hat,ylim=c(0,9),xlim=c(0,9))
abline(0,1,col="red",lty=2)
length(y_hat)
length(test_y)
pmse=sum((y_hat-test2_y)^2)/length(test2_y)
pmse

summary(pq28)
exp(-2.498e-02)
exp(-1.380e-01)
exp(-9.099e-02)
exp(-1.269e+00)
exp(1.432e-01)
exp(1.158e-04)
exp(-7.547e-02)
exp(5.314e-02)
exp(-2.967e-02)
exp(-3.923e-08)

# Wald test, interpretation
# Relative to average grade & ok condition:
# - 2% fewer bedrooms if grade better
# - 13% fewer bedrooms if grade higher
# - 9% fewer bedrooms if grade lower
# - 72% fewer bedrooms if grade poor
# - 5% more bedrooms if condition good 
# - 3% fewer bedrooms if condition poor # NOT SIGNIFICANT

# - 15% more bedrooms for each extra bathroom added
# - same number of bedrooms if increase sqft_above by 1 sqft
# - 7% fewer bedrooms for each extra floor added
# - same number of bedrooms if increase price by $1!

# Influence measures:

model=pq28 # ADAPT

infl<-influence(model)
d<-cooks.distance(model)

par(mfrow=c(1,2))
## standardized residuals:
r_std<-infl$pear.res/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),main="Standardised residuals")
z<-qnorm(0.05/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")

# Cook's distance:
plot(d,ylim=c(0,1.2),xlab="i",ylab="D",main="Cook's distance")
abline(h=1,col="red")

# 4/dim(train)[1]

# plot(model) # WARNING: don't do this

y_hat=exp(predict(pq28,newdata=test_x))
plot(test_y,y_hat,ylim=c(0,9),xlim=c(0,9))
abline()
length(y_hat)
length(test_y)
pmse=sum((y_hat-test_y)^2)/length(test_y)
pmse

# Build model
summary(pq26 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+price+condition_cat,
                    data=train2,family="quasipoisson")) # train2

summary(pq29)
exp(-2.489e-02)
exp(-1.382e-01)
exp(-9.051e-02)
exp(-1.268e+00)
exp(1.434e-01)
exp(1.150e-04)
exp(-7.578e-02)
exp(5.313e-02)
exp(-2.935e-02)
exp(-3.361e-08)
exp(-3.773e-02)
exp(4.110e-03)

# Wald test, interpretation
# Relative to average grade & ok condition & ok view:
# - 2% fewer bedrooms if grade better
# - 13% fewer bedrooms if grade higher
# - 9% fewer bedrooms if grade lower
# - 72% fewer bedrooms if grade poor
# - 5% more bedrooms if condition good 
# - 3% fewer bedrooms if condition poor # NOT SIGNIFICANT
# - 4% fewer bedrooms if view amazing
# - same number of bedrooms if view is poor # NOT SIGNIFICANT

# - 15% more bedrooms for each extra bathroom added
# - same number of bedrooms if increase sqft_above by 1 sqft
# - 7% fewer bedrooms for each extra floor added
# - same number of bedrooms if increase price by $1!

# Influence measures:

model=pq29 # ADAPT

infl<-influence(model)
d<-cooks.distance(model)

par(mfrow=c(1,2))
## standardized residuals:
r_std<-infl$pear.res/sqrt(1-infl$hat)
plot(r_std,xlab="i",ylab="r_i",ylim=c(-4,4),main="Standardised residuals")
z<-qnorm(0.05/2)
abline(h=0)
abline(h=-z,col="red")
abline(h=z,col="red")

# Cook's distance:
plot(d,ylim=c(0,1.2),xlab="i",ylab="D",main="Cook's distance")
abline(h=1,col="red")

# 4/dim(train)[1]
par(mfrow=c(1,1))
y_hat=exp(predict(pq29,newdata=test_x))
plot(test_y,y_hat,ylim=c(0,9),xlim=c(0,9))
abline()
length(y_hat)
length(test_y)
pmse=sum((y_hat-test_y)^2)/length(test_y)
pmse
par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
boxplot(y_hat~test_y,main="Predicted vs. observed number of bedrooms")
title(xlab="Observed number of bedrooms",ylab = "Predicted number of bedrooms", cex.lab = 1,
      line = 2.5)

summary(p29 <- glm(bedrooms~grade_cat+bathrooms+sqft_above+floors+condition_cat+price+view_cat,data=train,family="poisson")) # ADAPT

model=p29 # ADAPT

# ---- Dispersion test-------------------------------------------------
dispersiontest(model,alternative="two.sided") # No overdispersion
