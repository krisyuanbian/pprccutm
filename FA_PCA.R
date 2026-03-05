library(tidyverse)
library(readxl)
dat_360<- read_excel("D:/Downloads/Case study data/physician 360-degree evaluation data.xlsx")
colnames(dat_360)<-paste0("Q",1:25)
dat_360$Q25<-factor(dat_360$Q25)
dat_360$Q24<-factor(dat_360$Q24)
dat_me<-dat_360%>%
  group_by(Q24)%>%
  summarise(Q1=median(Q1,na.rm = TRUE),
            Q2=median(Q2,na.rm = TRUE),
            Q3=median(Q3,na.rm = TRUE),
            Q4=median(Q4,na.rm = TRUE),
            Q5=median(Q5,na.rm = TRUE),
            Q6=median(Q6,na.rm = TRUE),
            Q7=median(Q7,na.rm = TRUE),
            Q8=median(Q8,na.rm = TRUE),
            Q9=median(Q9,na.rm = TRUE),
            Q10=median(Q10,na.rm = TRUE),
            Q11=median(Q11,na.rm = TRUE),
            Q12=median(Q12,na.rm = TRUE),
            Q13=median(Q13,na.rm = TRUE),
            Q14=median(Q14,na.rm = TRUE),
            Q15=median(Q15,na.rm = TRUE),
            Q16=median(Q16,na.rm = TRUE),
            Q17=median(Q17,na.rm = TRUE),
            Q18=median(Q18,na.rm = TRUE),
            Q19=median(Q19,na.rm = TRUE),
            Q20=median(Q20,na.rm = TRUE),
            Q21=median(Q21,na.rm = TRUE),
            Q22=median(Q22,na.rm = TRUE),
            Q23=median(Q23,na.rm = TRUE))

#non-linear-factor analysis
#library(homals)
library(psych)

Fdat<-dat_me[,2:24]
poly_cor = polychoric(Fdat)
cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)

#For Medical expert
Fdat1<-dat_me[,2:4]

poly_cor = polychoric(Fdat1)
rho = poly_cor$rho
poly_cor$tau### Thresholds/Scaling results

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
# Polychoric factor analysis
poly_model = fa(Fdat1, nfactor=1, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
Weights<-poly_model$weights
colnames(poly_model$loadings) <- c("Medical_Fac")
fa.diagram(poly_model)

F1<-as.matrix(Fdat1)%*%as.matrix(Weights)

#For Professionalism
Fdat1<-dat_me[,7:10]

poly_cor = polychoric(Fdat1)
rho = poly_cor$rho
poly_cor$tau### Thresholds/Scaling results

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
# Polychoric factor analysis
poly_model = fa(Fdat1, nfactor=2, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
Weights<-poly_model$weights
colnames(poly_model$loadings) <- c("Prof_Fac1","Prof_Fac2")
fa.diagram(poly_model)

F2<-as.matrix(Fdat1)%*%as.matrix(Weights)


#For Communication
Fdat1<-dat_me[,11:15]

poly_cor = polychoric(Fdat1)
rho = poly_cor$rho
poly_cor$tau### Thresholds/Scaling results

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)
#fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
# Polychoric factor analysis
poly_model = fa(Fdat1, nfactor=3, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
Weights<-poly_model$weights
colnames(poly_model$loadings) <- c("Comm_Fac1","Comm_Fac2","Comm_Fac3")
fa.diagram(poly_model)

F3<-as.matrix(Fdat1)%*%as.matrix(Weights)

#For Collaboration
Fdat1<-dat_me[,16:19]

poly_cor = polychoric(Fdat1)
rho = poly_cor$rho
poly_cor$tau### Thresholds/Scaling results

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
# Polychoric factor analysis
poly_model = fa(Fdat1, nfactor=2, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
Weights<-poly_model$weights
colnames(poly_model$loadings) <- c("Col_Fac1","Col_Fac2")
fa.diagram(poly_model)

F4<-as.matrix(Fdat1)%*%as.matrix(Weights)

#For Management
Fdat1<-dat_me[,20:23]

poly_cor = polychoric(Fdat1)
rho = poly_cor$rho
poly_cor$tau### Thresholds/Scaling results

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")
# Polychoric factor analysis
poly_model = fa(Fdat1, nfactor=2, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
Weights<-poly_model$weights
colnames(poly_model$loadings) <- c("Mana_Fac1","Mana_Fac2")
fa.diagram(poly_model)


F5<-as.matrix(Fdat1)%*%as.matrix(Weights)

Fac_data<-cbind(F1,F2,F3,F4,F5,dat_me[,c(5,6,24,1)])
colnames(Fac_data)<-c("MedicalFac","ProfesFac1","ProfesFac2","CommuFac1","CommuFac2","CommuFac3","CollabFac1","CollabFac2","ManagFac1","ManagFac2","Advocacy","SciKno","OverallSc","DocId")

write.table(Fac_data,"360f.csv",row.names=FALSE,col.names=TRUE,sep=",")


#PCA for constructing INDEX
#data
library(readr)
library(tidyverse)
PCA_Dat<- read_csv("D:/Downloads/Case study data/predict.csv")


com1 <- prcomp(PCA_Dat[,-c(1:3)], center = TRUE,scale. = TRUE)
summary(com1)

#loadings
load<-t(t(com1$rotation)*com1$sdev)

Comp1<-as.matrix(PCA_Dat[,-c(1:3)])%*%load[,1]
Comp2<-as.matrix(PCA_Dat[,-c(1:3)])%*%load[,2]

INDEX<-(Comp1*0.6109+Comp2*0.3333)/0.9442
ID<-1:25
DDAT<-cbind(ID,INDEX)
TOP<-DDAT[order(DDAT[,2],decreasing = TRUE),]

TOP10<-as.data.frame(TOP[1:10,])
TOP10<-TOP10[order(TOP10[,2]),]
TOP10$Doc<-c("Doc")
TOP10$ID<-str_c(TOP10$Doc,TOP10$ID)
#Name<-paste0("TOP",c(10,9,8,7,6,5,4,3,2,1))
cols <- heat.colors(10) 
b <- barplot(as.numeric(TOP10$V2),names.arg =TOP10$ID, horiz=TRUE,col = cols, space= 0.1, xlim=c(0,28),axes = F) 
