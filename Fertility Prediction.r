Fertility <- read.csv("FertilityData.csv")


#Q1
#Relationship between Age and LowAFC
plot(Fertility$Age,Fertility$LowAFC, main="Relationship between Age and Smallest Antral Follicle Count", xlab = "Age (Years)", ylab = "Smallest Antral Follicle Count")
#plot(LowAFC~Age,data=Fertility)  // Double Check previous
lm_model <- lm(LowAFC~Age,data=Fertility)
abline(lm_model, col = "red")
summary.lm(lm_model)

#Assumption Checking
par(mfrow=c(1,2))
plot(lm_model)
par(mfrow=c(1,1))

plot((Fertility$Age)^2,Fertility$LowAFC)

simplefit <- lm(LowAFC~.^2,data=Fertility)
summary(simplefit)


#Q2
#Best Single Predictor of LowAFC
#Age
AgeAFC<- lm(LowAFC~Age,data=Fertility)
summary(AgeAFC)

#FSH
FSHAFC <- lm(LowAFC~FSH,data=Fertility)
summary(FSHAFC)

#E2
E2AFC <- lm(LowAFC~E2,data=Fertility)
summary(E2AFC)

#MaxE2
MaxAFC <- lm(LowAFC~MaxE2,data=Fertility)
summary(MaxAFC)

#TotalGn
GnAFC <- lm(LowAFC~TotalGn,data=Fertility)
summary(GnAFC)
abline(GnAFC, col = "red")

#Embryos
EmbryoAFC <- lm(LowAFC~Embryos,data=Fertility)
summary(EmbryoAFC)

plot(Fertility$TotalGn,Fertility$LowAFC)
plot(Fertility$Age,Fertility$LowAFC)


##Best Explainer for LowAFC is TotalGN
#TotalGn
GnAFC <- lm(LowAFC~TotalGn,data=Fertility)
summary(GnAFC)
abline(GnAFC, col = "red")
plot(LowAFC~TotalGn,data=Fertility, main = "Relationship between Total Gonadotropin Level and Smallest Antral Follicle Count", xlab = "Total Gonadotropin Level", ylab = "Smallest Antral Follicle Count")
#Assumption Checking
par(mfrow=c(1,2))
plot(GnAFC)

par(mfrow=c(1,1))


#Q3
#Normal
simplefit <- lm(LowAFC~.,data=Fertility)
summary(simplefit)
stepfit <- step(simplefit,direction="backward",trace=0)
summary(stepfit)
plot(stepfit)


#^2 - The model found
complexfit <- lm(LowAFC~.^2,data=Fertility)
summary(complexfit)
mystepfit <- step(complexfit,direction="backward",trace=0)
summary(mystepfit)
plot(mystepfit)

#^2 without high p-values - Good
adjustedfit <- lm(LowAFC~FSH^2 + MaxE2^2+E2^2+TotalGn^2+MaxE2:Embryos,data=Fertility)
summary(adjustedfit)
plot(adjustedfit)

#^2 with an additional removal of p-value - THE ONE
adjustedfit <- lm(LowAFC~FSH^2+TotalGn^2+MaxE2:Embryos,data=Fertility)
summary(adjustedfit)
plot(adjustedfit)




#Plotting Predictor against Response
plot(Fertility$Age,Fertility$LowAFC)
plot(Fertility$FSH,Fertility$LowAFC)
plot(Fertility$E2,Fertility$LowAFC)
plot(Fertility$MaxE2,Fertility$LowAFC)
plot(Fertility$TotalGn,Fertility$LowAFC)
plot(Fertility$Embryos,Fertility$LowAFC)

#^2 without non-linear variables - without FSH - Bad
NonLinFit <- lm(LowAFC~ E2^2 + MaxE2^2 + TotalGn^2+Embryos^2+MaxE2:Embryos,data=Fertility)
summary(NonLinFit)
par(mfrow=c(1,2))
plot(NonLinFit)
par(mfrow=c(1,1))


#^2 without non-linear variables - without E2 - Bad
NonLinFit2 <- lm(LowAFC~FSH^2+MaxE2^2+TotalGn^2+Embryos^2+MaxE2:Embryos,data=Fertility)
summary(NonLinFit2)
par(mfrow=c(2,2))
plot(NonLinFit2)
par(mfrow=c(1,1))





#^2 with an additional removal of p-value 
foundfit <- lm(LowAFC~FSH^2+TotalGn^2+MaxE2:Embryos,data=Fertility)
summary(foundfit)
plot(foundfit)


#Prediction Interval
predict(foundfit,newdata=data.frame(FSH=42,TotalGn=150,MaxE2=3,Embryos=7),interval='prediction')


predict(foundfit,newdata=data.frame(FSH=0.5,TotalGn=1388,MaxE2=2527,Embryos=7),interval='prediction')


predict(foundfit,newdata=data.frame(FSH=0.5,TotalGn=1388,MaxE2=2527,Embryos=7))
