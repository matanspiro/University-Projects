datasetTotal<-read.csv(file.choose(),header = T) #continuous variables without countries
colnames(datasetTotal)[1] <- "AgeAtFirstBirth"

dataset <- datasetTotal[,1:length(datasetTotal)-1] #remove country name


colnames(dataset)[1] <- "AgeAtFirstBirth"

dataWithoutCat <- subset(dataset,select=c("AgeAtFirstBirth",	"Days.at.home.for.labor",	"Rate.of.happiness",
                              "Yrs.of.education..woman",	"Divorce.rates",	"Life.expectancy..women",
                              "Hrs.of.work",	"Avg.marriage.age..woman",	
                              "Avg.num.of.kids",	"Wage"))

#------------------------------------------Q3-------------------------------------------
cov(dataWithoutCat) 
x <- cor(dataWithoutCat)

plot(dataWithoutCat,col=rgb(0,100,0,50,maxColorValue=255), pch=16)
summary(dataWithoutCat) 

#------------------------------------------Q4-------------------------------------------

library(e1071) 


answer <- function(vector,string){ #to display data comfortably
  MEAN <- mean(vector)
  MEDIAN <- median(vector)
  SD <- sd(vector)
  QUANTILE <- quantile(vector,probs=c(0.25,0.5,0.75))
  SKEWNESS <- skewness(vector)
  print(string)
  paste("mean=",MEAN," median=",MEDIAN," sd=",SD," quantile=", QUANTILE," skewness=",SKEWNESS)
  
}

print(answer(dataWithoutCat$AgeAtFirstBirth,"AgeAtFirstBirth")) 
print(answer(dataWithoutCat$Days.at.home.for.labor,"Days.at.home.for.labor")) 
print(answer(dataWithoutCat$Rate.of.happiness,"Rate.of.happiness")) 
print(answer(dataWithoutCat$Yrs.of.education..woman,"Yrs.of.education..woman")) 
print(answer(dataWithoutCat$Divorce.rates,"Divorce.rates") )
print(answer(dataWithoutCat$Life.expectancy..women,"expectancy..women")) 
print(answer(dataWithoutCat$Hrs.of.work,"Hrs.of.work") )
print(answer(dataWithoutCat$Avg.marriage.age..woman,"Avg.marriage.age..woman")) 
print(answer(dataWithoutCat$Avg.num.of.kids,"Avg.num.of.kids"))
print(answer(dataWithoutCat$Wage,"wage"))


categorialDataset <- subset(dataset,select=c("AgeAtFirstBirth","regime"))

yAtRegime1 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime=='Democracy'")
print(answer(yAtRegime1$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime2 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Dominant Party'")
print(answer(yAtRegime2$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime3 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Foreign/Occupied'")
print(answer(yAtRegime3$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime4 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Military'")
print(answer(yAtRegime4$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime5 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Monarchy'")
print(answer(yAtRegime5$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime6 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Party-Personal'")
print(answer(yAtRegime6$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime7 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Personal Dictatorship'")
print(answer(yAtRegime7$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtRegime8 <- sqldf("select AgeAtFirstBirth from categorialDataset where regime='Provisional - Civilian'")
print(answer(yAtRegime8$AgeAtFirstBirth,"AgeAtFirstBirth")) 


categorialDataset2<-subset(dataset,select=c("AgeAtFirstBirth","Religion"))

yAtReligion1 <- sqldf("select AgeAtFirstBirth from categorialDataset2 where Religion='Buddhist'")
print(answer(yAtReligion1$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtReligion2 <- sqldf("select AgeAtFirstBirth from categorialDataset2 where Religion='Christian'")
print(answer(yAtReligion2$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtReligion3 <- sqldf("select AgeAtFirstBirth from categorialDataset2 where Religion='Hindu'")
print(answer(yAtReligion3$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtReligion4 <- sqldf("select AgeAtFirstBirth from categorialDataset2 where Religion='IrReligion'")
print(answer(yAtReligion1$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtReligion5 <- sqldf("select AgeAtFirstBirth from categorialDataset2 where Religion='Jewish'")
print(answer(yAtReligion5$AgeAtFirstBirth,"AgeAtFirstBirth")) 

yAtReligion6 <- sqldf("select AgeAtFirstBirth from categorialDataset2 where Religion='Muslim'")
print(answer(yAtReligion6$AgeAtFirstBirth,"AgeAtFirstBirth")) 



#--------------------------------------------Q5------------------------------------------


bp<-boxplot(dataset$Days.at.home.for.labor , main='Days.at.home.for.labor')
bp<-boxplot(dataset$Rate.of.happiness , main='Rate.of.happiness')
bp<-boxplot(dataset$Yrs.of.education , main='Yrs.of.education')
bp<-boxplot(dataset$Divorce.rates , main='Divorce.rates')
bp<-boxplot(dataset$Life.expectancy , main='Life.expectancy')
bp<-boxplot(dataset$Hrs.of.work , main='Hrs.of.work')
bp<-boxplot(dataset$Avg.marriage.age , main='Avg.marriage.age')
bp<-boxplot(dataset$Avg.num.of.kids , main='Avg.num.of.kids')
bp<-boxplot(dataset$AgeAtFirstBirth , main='AgeAtFirstBirth')
bp<-boxplot(dataset$Wage , main='Wage')


#--------------------------------------- Q6----------------------------------------
#------------daysAtHome--------------------------------------------------

hist(dataset$Days.at.home.for.labor,prob=TRUE, main='Days at home for labor',xlab = 'Days at home for labor')
lines(density(dataset$Days.at.home.for.labor),col="blue",lwd=2)

plot(ecdf(dataset[,"Days.at.home.for.labor"]),main='Days at home for labor', xlab = 'Days at home for labor')


#-----------AvNumOfKids-------------------------------

hist(dataset$Avg.num.of.kids,prob=TRUE, main='Avg hum of kids',xlab = 'Avg hum of kids')
lines(density(dataset$Avg.num.of.kids),col="blue",lwd=2)

plot(ecdf(dataset[,"Avg.num.of.kids"]),main='Avg.num.of.kids',xlab = 'Avg.num.of.kids')

#---------LifeExpectancy------------------------------

hist(dataset$Life.expectancy,prob=TRUE, main='Life expectancy',xlab = 'Life expectancy')
lines(density(dataset$Life.expectancy),col="blue",lwd=2)

plot(ecdf(dataset[,"Life.expectancy..women"]),main='Life expectancy',xlab = 'Life expectancy')


# -------------------------------------Q7----------------------------------------

plot(x=dataset$Rate.of.happiness,y=dataset$Life.expectancy,
     ylab="Life expectancy",frame = FALSE,col = 'blue' ,pch = 20,xlab="Rate of happiness")
plot(x=as.numeric(factor(dataset$Religion)),y=dataset$Wage,
     ylab="Wage",frame = FALSE,col = 'blue',xlab="Religion")
plot(x=dataset$Yrs.of.education,y=dataset$Avg.num.of.kids ,
     ylab="Avg num of kids",frame = FALSE,col = 'blue' ,pch = 20,xlab="Years of education")
plot(x=dataset$Avg.marriage.age,y=dataset$Divorce.rates,
     ylab="Divorce rates",frame = FALSE,col = 'blue' ,pch = 20,xlab="Avg marriage age")
plot(x=dataset$Yrs.of.education,y=dataset$Wage,
     ylab="Wage",frame = FALSE,col = 'blue' ,pch = 20,xlab="Years of education")


#-----------------------------Q8------------------------------------------
binFreqTable <- function(x, bins) {
  freq = hist(x, breaks=bins, include.lowest=TRUE, plot=FALSE)
  ranges = paste(head(freq$breaks,-1), freq$breaks[-1], sep=" - ")
  return(data.frame(range = ranges, frequency = freq$counts))
}


#1D tables
x2.9 <- binFreqTable(dataset$Rate.of.happiness,seq(2,8,by=1))
hist(dataset$Rate.of.happiness); axis(1,at=seq(2,8,1))


xx2.9 <- binFreqTable(dataset$Avg.num.of.kids,seq(0,7,by=1))
hist(dataset$Avg.num.of.kids); axis(1,at=seq(0,7,1))


#2D tables

  
#2D table- wage&hrs of work

ANS <- matrix( 0,nrow = 7, ncol = 7)

x=20
y=0
for(n in 1:7){
  for(k in 1:7){
    for(i in 1:128){
      if((dataWithoutCat[i,7]>=x && dataWithoutCat[i,7]<(5+x)) && (dataWithoutCat[i,10]>=y && dataWithoutCat[i,10]<(10000+y))){
        ANS[n,k]=ANS[n,k]+1
      }
    }
    y=y+10000
  }
  y=0
  x=x+5
}


#2D table- avg age of marriage $avg num of kids

ANS2 <- matrix( 0,nrow = 7, ncol = 7)

x=14
y=0
for(n in 1:7){
  for(k in 1:7){
    for(i in 1:128){
      if((dataWithoutCat[i,8]>=x && dataWithoutCat[i,8]<(3+x)) && (dataWithoutCat[i,9]>=y && dataWithoutCat[i,9]<(1+y))){
        ANS2[n,k]=ANS2[n,k]+1
      }
    }
    y=y+1
  }
  y=0
  x=x+3
}

#------------------------------------Part B-------------------------------------------------------------------

#------------------------------------Q 2.1---------------------------------------

#scatterplot for suspicious varabiles

plot(y=dataset$AgeAtFirstBirth,x=dataset$Days.at.home.for.labor,
     xlab="Days.at.home.for.labor",frame = TRUE,col = 'blue' ,pch = 20,ylab="AgeAtFirstBirth")
abline(lm(dataset$AgeAtFirstBirth~dataset$Days.at.home.for.labor), col="red")


plot(y=dataset$AgeAtFirstBirth,x=dataset$Divorce.rates,
     xlab="Divorce.rates",frame = TRUE,col = 'blue' ,pch = 20,ylab="AgeAtFirstBirth")
abline(lm(dataset$AgeAtFirstBirth~dataset$Divorce.rates), col="red")

plot(y=dataset$AgeAtFirstBirth,x=dataset$Hrs.of.work,
     xlab="Hrs.of.work",frame = TRUE,col = 'blue' ,pch = 20,ylab="AgeAtFirstBirth")
abline(lm(dataset$AgeAtFirstBirth~dataset$Hrs.of.work), col="red")



model.1<-lm(AgeAtFirstBirth ~ factor(Religion), data=dataset,x=TRUE,y=TRUE)
summary(model.1)

model.2<-lm(AgeAtFirstBirth ~ factor(regime), data=dataset,x=TRUE,y=TRUE)
summary(model.2)

model.3<-lm(AgeAtFirstBirth ~ Hrs.of.work, data=dataset,x=TRUE,y=TRUE)
summary(model.3)

model.4<-lm(AgeAtFirstBirth ~ Days.at.home.for.labor, data=dataset,x=TRUE,y=TRUE)
summary(model.4)

model.5<-lm(AgeAtFirstBirth ~ Divorce.rates, data=dataset,x=TRUE,y=TRUE)
summary(model.5)


datasetNew <- subset(dataset,select=c("AgeAtFirstBirth",	"Days.at.home.for.labor",	"Rate.of.happiness",
                                      "Yrs.of.education..woman","regime",	"Life.expectancy..women",
                                      "Avg.marriage.age..woman",	"Avg.num.of.kids",	"Wage"))

#------------------------------Q2.2---------------------------------

datasetNew<-datasetNew[!(datasetNew$regime=="Provisional - Civilian"),]
datasetNew<-datasetNew[!(datasetNew$regime=="Foreign/Occupied"),]

unic <- unique(datasetNew$regime)
unic2 <- as.numeric(factor(unic))

datasetNew$regime <- as.numeric(factor(datasetNew$regime))
#1:democracy 2:dominant 3:millatery 4:monarchy 5:party-personal 6:dictator

#union categories
datasetNew$regime[datasetNew$regime==2] <- 1 #democracy and dominant
datasetNew$regime[datasetNew$regime==6] <- 2 #dictator
datasetNew$regime[datasetNew$regime==3] <- 3 #other
datasetNew$regime[datasetNew$regime==4] <- 3 
datasetNew$regime[datasetNew$regime==5] <- 3 

#discretion:

aveDays <- mean(datasetNew$Rate.of.happiness)
datasetNew$Rate.of.happiness<-ifelse(datasetNew$Rate.of.happiness>aveDays,c(0),c(1)) 

catRateHappiness<-lm(AgeAtFirstBirth ~ Rate.of.happiness, data=datasetNew,x=TRUE,y=TRUE)
summary(catRateHappiness)

#----------------------------Q2.4------------------------------------

regimeFactor <- relevel(factor(datasetNew$regime),ref = c(1)) %>% print()

model.f<-lm(AgeAtFirstBirth ~ Avg.marriage.age..woman * regimeFactor, data = datasetNew)
plot(x=datasetNew$Avg.marriage.age..woman,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Avg.marriage.age..woman[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Avg.marriage.age..woman[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Avg.marriage.age..woman[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

model.f<-lm(AgeAtFirstBirth ~ Yrs.of.education..woman * regimeFactor, data = datasetNew)
plot(x=datasetNew$Yrs.of.education..woman,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Yrs.of.education..woman[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Yrs.of.education..woman[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Yrs.of.education..woman[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

model.f<-lm(AgeAtFirstBirth ~ Life.expectancy..women * regimeFactor, data = datasetNew)
plot(x=datasetNew$Life.expectancy..women,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Life.expectancy..women[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Life.expectancy..women[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Life.expectancy..women[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

model.f<-lm(AgeAtFirstBirth ~ Rate.of.happiness * regimeFactor, data = datasetNew)
plot(x=datasetNew$Rate.of.happiness,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Rate.of.happiness[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Rate.of.happiness[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Rate.of.happiness[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

model.f<-lm(AgeAtFirstBirth ~ Wage * regimeFactor, data = datasetNew)
plot(x=datasetNew$Wage,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Wage[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Wage[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Wage[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

model.f<-lm(AgeAtFirstBirth ~ Days.at.home.for.labor * regimeFactor, data = datasetNew)
plot(x=datasetNew$Days.at.home.for.labor,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Days.at.home.for.labor[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Days.at.home.for.labor[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Days.at.home.for.labor[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

model.f<-lm(AgeAtFirstBirth ~ Avg.num.of.kids * regimeFactor, data = datasetNew)
plot(x=datasetNew$Avg.num.of.kids,y=datasetNew$AgeAtFirstBirth,col=regimeFactor)
lines(datasetNew$Avg.num.of.kids[regimeFactor==1],predict(model.f)[regimeFactor==1],col=1)
lines(datasetNew$Avg.num.of.kids[regimeFactor==2],predict(model.f)[regimeFactor==2],col=2)
lines(datasetNew$Avg.num.of.kids[regimeFactor==3],predict(model.f)[regimeFactor==3],col=3)
legend("topleft",legend=c("Democracy and Domimant","Dictatory","Else"),col=c(1,2,3),lty=c(1,1,1),bty="n",pt.bg=factor(regimeFactor))
summary(model.f)

#-----------------------------------Q3.1-------------------------------------

regimeFactor
fullModel <- lm(AgeAtFirstBirth~Days.at.home.for.labor+Days.at.home.for.labor*regimeFactor+Rate.of.happiness+Yrs.of.education..woman
+Life.expectancy..women+Avg.marriage.age..woman+Avg.marriage.age..woman*regimeFactor+Avg.num.of.kids
+Wage+Wage*regimeFactor,datasetNew)

summary(fullModel)

emptyModel <- lm(AgeAtFirstBirth~1,datasetNew)

#regration by steps:

bw.model <- step(fullModel,direction='backward',scope=~1)
summary(bw.model)

fwd.model <- step(emptyModel,direction='forward',scope=formula(fullModel))
summary(fwd.model)

stepwise <- step(fullModel, direction="both")
summary(stepwise)

#-------------------------------------------Q3.2---------------------------------------------
#delete Years Of Education:
datasetAfterStep <- datasetNew
datasetAfterStep$Rate.of.happiness <- NULL
datasetAfterStep$Yrs.of.education..woman <- NULL
datasetAfterStep$Avg.num.of.kids <- NULL
datasetAfterStep$Wage <- NULL

#Linear Model
ModelAfterStep <- lm(AgeAtFirstBirth~Days.at.home.for.labor
                     +Life.expectancy..women+Avg.marriage.age..woman*regimeFactor
                     ,datasetAfterStep)
suma <- summary(ModelAfterStep)

#Fixed errors
datasetAfterStep$predicted <- fitted(ModelAfterStep)#predicted values
datasetAfterStep$residuals <- residuals(ModelAfterStep)
se <- sqrt(var(datasetAfterStep$residuals))
datasetAfterStep$residualsFix <- (residuals(ModelAfterStep)/se)#fixed errors
plot(datasetAfterStep$residualsFix,xlab = "predictedValues",ylab = "FixedErrors")
abline(h=0)

#QQ plot:

qqnorm(datasetAfterStep$residualsFix)
abline(a=0,b=1)

#Histogram plot:
hist(datasetAfterStep$residualsFix)

#--------KS  
ks.test(x=datasetAfterStep$residualsFix,y="pnorm",alternative = "two.sided",exact=NULL)


#F Test for Equal Variances

A <- datasetAfterStep[,1]

A<-sort(A)

third <- round(length(A)/3)
twothird <- round(length(A)*2/3)

thirdData <- A[1:third]
twothirdData <- A[twothird:length(A)]

var.test(x= thirdData, y= twothirdData, ratio = 1, alternative = c("two.sided"), conf.level = 0.95)

#-------------------------------Q4----------------------------------
#checkPizor:

boxcox(ModelAfterStep, lambda = seq(-7,5,0.1))
lambda <- 0
ModelAfterCox <- lm(log(AgeAtFirstBirth)~Days.at.home.for.labor
                     +Life.expectancy..women+Avg.marriage.age..woman*regimeFactor
                     ,datasetAfterStep)
sama <- summary(ModelAfterCox)

#Fixed errors
datasetAfterStep$predicted <- fitted(ModelAfterCox)#predicted values
datasetAfterStep$predicted <- exp(datasetAfterStep$predicted)
datasetAfterStep$residuals <- datasetAfterStep$AgeAtFirstBirth-datasetAfterStep$predicted
se <- sqrt(var(datasetAfterStep$residuals))
datasetAfterStep$residualsFix <- (datasetAfterStep$residuals/se)#fixed errors
plot(datasetAfterStep$residualsFix,xlab = "predictedValues",ylab = "FixedErrors")
abline(h=0)


#findRajusted:
SSE <- sum((datasetAfterStep$residuals)^2)
RAdj <- 1-(SSE/(125-8))/var(datasetAfterStep$AgeAtFirstBirth)
RAdj

#QQ plot:

qqnorm(datasetAfterStep$residualsFix);axis(2,seq(1,4,1))
abline(a=0,b=1)


#Histogram plot:
hist(datasetAfterStep$residualsFix)


#--------KS  
ks.test(x=datasetAfterStep$residualsFix,y="pnorm",alternative = "two.sided",exact=NULL)

#-------------------------------------------------------------------

#try another transforms(0.5):


ModelAfterCox <- lm(((AgeAtFirstBirth)^0.5)~Days.at.home.for.labor
                    +Life.expectancy..women+Avg.marriage.age..woman*regimeFactor
                    ,datasetAfterStep)
sama <- summary(ModelAfterCox)

#Fixed errors
datasetAfterStep$predicted <- fitted(ModelAfterCox)#predicted values
datasetAfterStep$predicted <- (datasetAfterStep$predicted)^(2)
datasetAfterStep$residuals <- datasetAfterStep$AgeAtFirstBirth-datasetAfterStep$predicted
se <- sqrt(var(datasetAfterStep$residuals))
datasetAfterStep$residualsFix <- (datasetAfterStep$residuals/se)#fixed errors
plot(datasetAfterStep$residualsFix,xlab = "predictedValues",ylab = "FixedErrors")
abline(h=0)



#findRajusted:
SSE <- sum((datasetAfterStep$residuals)^2)
RAdj <- 1-(SSE/(125-8))/var(datasetAfterStep$AgeAtFirstBirth)
RAdj

#QQ plot:

qqnorm(datasetAfterStep$residualsFix);axis(2,seq(1,4,1))
abline(a=0,b=1)


#Histogram plot:
hist(datasetAfterStep$residualsFix)

#Shapiro test
shapiro.test(datasetAfterStep$residualsFix)


#--------KS  
ks.test(x=datasetAfterStep$residualsFix,y="pnorm",alternative = "two.sided",exact=NULL)

#-----------------------------------------------------------------------

#try another transforms(-0.5):


ModelAfterCox <- lm(((AgeAtFirstBirth)^-0.5)~Days.at.home.for.labor
                    +Life.expectancy..women+Avg.marriage.age..woman*regimeFactor
                    ,datasetAfterStep)
sama <- summary(ModelAfterCox)

#Fixed errors
datasetAfterStep$predicted <- fitted(ModelAfterCox)#predicted values
datasetAfterStep$predicted <- (datasetAfterStep$predicted)^(-2)
datasetAfterStep$residuals <- datasetAfterStep$AgeAtFirstBirth-datasetAfterStep$predicted
se <- sqrt(var(datasetAfterStep$residuals))
datasetAfterStep$residualsFix <- (datasetAfterStep$residuals/se)#fixed errors
plot(datasetAfterStep$residualsFix,xlab = "predictedValues",ylab = "FixedErrors")
abline(h=0)


#findRajusted:
SSE <- sum((datasetAfterStep$residuals)^2)
RAdj <- 1-(SSE/(125-8))/var(datasetAfterStep$AgeAtFirstBirth)
RAdj

#QQ plot:

qqnorm(datasetAfterStep$residualsFix);axis(2,seq(1,4,1))
abline(a=0,b=1)


#Histogram plot:
hist(datasetAfterStep$residualsFix)

#--------KS  
ks.test(x=datasetAfterStep$residualsFix,y="pnorm",alternative = "two.sided",exact=NULL)


#F Test for Equal Variances

A <- datasetAfterStep[,1]

A<-sort(A)

third <- round(length(A)/3)
twothird <- round(length(A)*2/3)

thirdData <- A[1:third]
twothirdData <- A[twothird:length(A)]

var.test(x= thirdData, y= twothirdData, ratio = 1, alternative = c("two.sided"), conf.level = 0.95)

