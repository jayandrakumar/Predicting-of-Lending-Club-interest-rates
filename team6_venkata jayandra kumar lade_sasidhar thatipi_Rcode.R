#reading data set
d1=read.csv("LoanStats3c_updated.csv",header=TRUE)

#converting all the null spaces into NA
d1[d1==""]<-NA
d1<-na.omit(d1)

#removing months in term
s1 <- sapply(strsplit(as.character(d1[,"term"])," "), function(x) (x[2]))
s1 <- as.integer(s1)
s1<-data.frame(s1)
d1[,"term"]<-s1
d1=as.data.frame(d1)

#preliminary analysis
#selecting numerical variables from the data set
for(i in 1:ncol(d1))
{
  if(i==1)
    tf<-is.numeric(d1[,i])
  else
    tf[i]<-is.numeric(d1[,i])
}
colnum_data<- which(tf)
d4=d1[,colnum_data]
m=cor(d4)
library(corrplot)
corrplot(m,method = "square")

#individual Histograms
attach(d4)
hist(loan_amnt)
hist(term)
hist(int_rate)
hist(annual_inc)
hist(dti)
hist(delinq_2yrs)
hist(earliest_cr_line)
hist(inq_last_6mths)
hist(mths_since_last_delinq)
hist(mths_since_last_record)
hist(open_acc)
hist(pub_rec)
hist(revol_bal)
hist(revol_util)
hist(total_acc)
hist(out_prncp)
hist(total_pymnt)
hist(total_rec_prncp)
hist(total_rec_int)
hist(total_rec_late_fee)
hist(recoveries)
hist(collection_recovery_fee)
hist(collections_12_mths_ex_med)
hist(mths_since_last_major_derog)
detach(d4)

#splitting the data set into training and validation
set.seed(2016)
traindata<-sample(n,n*0.6,replace = FALSE)
training<-d1traindata,]
validation<-d1[-traindata,]

#fitting a model for the data through regression
#backward regression
trainfit_temp<-lm(int_rate~.,data=training)
backward<-step(trainfit_temp,direction = 'backward')
#forward regression
null<-lm(int_rate~1,data=training)
forward<-step(null, scope=list(upper=trainfit_temp), direction='forward')
#both direction regression
both<-step(trainfit_temp, direction='both')

#AIC and BIC values for all model-model selection
AIC(backward,forward,both)
BIC(backward,forward,both)

#Performing VIF to check multi collinearity
library(HH)
vif(forward)

#Diagnostic plot
par(mfrow=c(2,2))
plot(forward)

#linearity
dev.off()
plot(fitted(forward),residuals(forward))
abline(0,0)

#Anderson-Darling test
library(nortest)
ad.test(forward$residuals)

#Analysis of variance
a=anova(forward)
a

#regression sum of squares
sum(a$`Mean Sq`)

#residual sum of squares
a$`Sum Sq`[10]

#total sum of squares
(sum(a$`Mean Sq`)+a$`Sum Sq`[10])

#r-squared
sum(a$`Mean Sq`)/(sum(a$`Mean Sq`)+a$`Sum Sq`[10])

#predicting int_rates for entire vildation set
pred=predict(forward,validation)

#root mean square for error caluclation
rms=sqrt(mean((pred-validation$int_rate)^2))
rms

#predicting for specific rows
pred[1]

validation[1,]$int_rate

pred[59]

validation[59,]$int_rate
