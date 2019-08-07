library(neuralnet)
library(quantmod)
library(nnet)
#Grab Apple Stock Data
AAPL<-as.data.frame(getSymbols("AAPL",auto.assign = FALSE))

#Compute the Simple Moving Average
sma50<-SMA(AAPL$AAPL.Close,50)
sma200<-SMA(AAPL$AAPL.Close,200)
sma50<-sma50[200:length(sma50)]
sma200<-sma200[200:length(sma200)]
AAPL<-AAPL[200:nrow(AAPL),]

#Append the MAs to the Data Frame
AAPL<-data.frame(AAPL,sma50,sma200)

#Split the Data Frame into "Before" and "Current" data (i.e., "lag" the series)
AAPL_c<-AAPL[-1,]
AAPL_b<-AAPL[-nrow(AAPL),]

#Compute percentage difference of the close from the previous open, close, high, low:
cc<-(AAPL_c$AAPL.Close - AAPL_b$AAPL.Close)/AAPL_b$AAPL.Close
plot(ts(cc))

#Compute percentage of prev day's O,H,L,C from 50 and 200 ma:
c50<-(AAPL_b$AAPL.Close-AAPL_b$sma50)/AAPL_b$sma50
o50<-(AAPL_b$AAPL.Open-AAPL_b$sma50)/AAPL_b$sma50
l50<-(AAPL_b$AAPL.Low-AAPL_b$sma50)/AAPL_b$sma50
h50<-(AAPL_b$AAPL.High-AAPL_b$sma50)/AAPL_b$sma50
c200<-(AAPL_b$AAPL.Close-AAPL_b$sma200)/AAPL_b$sma200
o200<-(AAPL_b$AAPL.Open-AAPL_b$sma200)/AAPL_b$sma200
l200<-(AAPL_b$AAPL.Low-AAPL_b$sma200)/AAPL_b$sma200
h200<-(AAPL_b$AAPL.High-AAPL_b$sma200)/AAPL_b$sma200

aapl_train<-data.frame(cc,c50,o50,h50,l50,c200,o200,h200,l200)
#Train Neural Network
n <- names(aapl_train)
f<-as.formula(paste("cc~", 
                    paste(n[!n %in% c("cc")], collapse = " + ")))

myFunction=function(x){
  sin(x)
}

nn<-neuralnet(f,data=aapl_train,act.fct=myFunction,hidden=c(20,10,5),linear.output=T)
nn2<-neuralnet(f,data=aapl_train,act.fct=tanh,hidden=c(8,8,7,7,6,6,5,5,4,4,3,3,2,2),linear.output=T)
#nn<-neuralnet(f,data=aapl_train,hidden=c(5,3),linear.output=T,act.fct = af)


pred<-nn2$net.result[[1]]
actual<-cc
sum((pred-actual)^2)


#Run a classification on the data.
#If the close was a negative percentage, = "Neg"
#If it was no change from yesterday's close, = "NC"
#If the percentage change was positive, = "Pos"
cc2<-cc
cc2[which(cc<0)]="Neg"
cc2[which(cc>0)]="Pos"
cc2[which(cc==0)]="NC"

#Convert the new cc2 categorical variable into 
#dummy variables:
appl_train<-cbind(aapl_train,class.ind(as.factor(cc2)))

#Now train the model for classification into the one of
#three categorical variables:
n <- names(appl_train)
f<-as.formula(paste("Neg+Pos+NC~", 
                    paste(n[!n %in% c("cc","Neg","Pos","NC")], collapse = " + ")))
nn<-neuralnet(f,data=appl_train,hidden=c(5,3),linear.output=F)


  
  