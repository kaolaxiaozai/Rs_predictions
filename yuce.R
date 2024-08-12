
###load packages
library(ggplot2)
library(cowplot)
library(randomForest)
#library(reprtree)
library(repr)
library(tree)
library(party)
library(ggpubr)
library(Metrics)
library(readr)
library(tibble)
library(readxl)
library(missForest)
library(missRanger)
library(rfPermute)
library(A3)



RsForRandomForest<- read_csv("E:/data/14098967/Table 1 MGRsD.csv",locale=locale(encoding="GBK"))
str(RsForRandomForest)
RsForRandomForest$Measure_Year <- as.factor(RsForRandomForest$Measure_Year)
RsForRandomForest$Measure_Month <- as.factor(RsForRandomForest$Measure_Month)
RsForRandomForest$Climate_Koeppon <- as.factor(RsForRandomForest$Climate_Koeppon)
RsForRandomForest$TopClimatetype <- as.factor(RsForRandomForest$TopClimatetype)
RsForRandomForest$IGBP <- as.factor(RsForRandomForest$IGBP)
RsForRandomForest$Country <- as.factor(RsForRandomForest$Country)
RsForRandomForest$SiteID <- as.factor(RsForRandomForest$SiteID)
RsForRandomForest$MiddleClimate <- as.factor(RsForRandomForest$MiddleClimate)
RsForRandomForest$IGBP_FromPaper <- as.factor(RsForRandomForest$IGBP_FromPaper)
RsForRandomForest$Meas_Method <- as.factor(RsForRandomForest$Meas_Method)
RsForRandomForest$absLat <- abs(RsForRandomForest$Latitude)
RsForRandomForest$absLog <- abs(RsForRandomForest$Longitude)


names(RsForRandomForest)[33] <- "Clay"
names(RsForRandomForest)[34] <- "Sand"
names(RsForRandomForest)[35] <- "Silt"
names(RsForRandomForest)[30] <- "CN"



RsForRandomForest <- subset(RsForRandomForest, select= -IGBP_FromPaper)
RsForRandomForest <- subset(RsForRandomForest, select= -Meas_Method)
RsForRandomForest <- subset(RsForRandomForest, select= -Country)
RsForRandomForest <- subset(RsForRandomForest, select= -SiteID)
RsForRandomForest_na <- subset(RsForRandomForest, !is.na(RsForRandomForest$N))
RsForRandomForest_naOM <- subset(RsForRandomForest_na, !is.na(RsForRandomForest_na$Elevation))
#RsForRandomForest_naOM <- subset(RsForRandomForest_naO, !is.na(RsForRandomForest_naO$Meas_Method))
# 打印新的数据框
print(RsForRandomForest_naOM)

#填加缺失值
set.seed (502)
RsForRandomForest_naOMIT <- missRanger(RsForRandomForest_naOM)

print(RsForRandomForest_naOMIT)

#write.csv(RsForRandomForest_naOMIT, 'RsForRandomForest_naOMIT.csv')
#plot(RsForRandomForest_naOMIT$TS,RsForRandomForest_naOMIT$Tm)
plot(RsForRandomForest_naOMIT$Tm,RsForRandomForest_naOMIT$TS, #main = '训练集',
     xlab = 'Tm', ylab = 'Ts',xlim=c(-30,50), ylim=c(-30,50),col = "#00000030",pch=19)
abline(0, 1,lty = "dashed")
z<-line(RsForRandomForest_naOMIT$TS,RsForRandomForest_naOMIT$Tm)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(-18, 40, c("R=0.91,p<2.2e-16"),cex=1,font=2,col="#000000")


RsForRandomForest_naOMIT %>% 
  mutate(IBGP1 = case_when(
    IGBP %in% c("CRO", "CVM") ~ "Cro",
    IGBP %in% c("BSV", "OSH") ~ "Shr",
    IGBP %in% c("DBF", "EBF", "ENF","MF") ~ "For",
    #ClimateTypes %in% c() ~ "Cs",
    #ClimateTypes %in% c("Cwa", "Cwb", "Cwc") ~ "Cw",
    IGBP %in% c("GRA", "SAV", "WSA") ~ "Gra",
    #ClimateTypes %in% c("Dsa", "Dsb", "Dsc", "Dwa", "Dwb", "Dwc", "Dwd") ~ "Dsw",
    IGBP %in% c("SNO", "URB","WAT") ~ "Oth",
    TRUE ~ "Oth")) -> RsForRandomForest_naOMIT

#Randomly split the overall data into 70% for training, 30% for validating
set.seed (502)
train <- sample(2, nrow(RsForRandomForest_naOMIT), replace = TRUE, prob = c(0.7,0.3))
TrainSet <- RsForRandomForest_naOMIT[train==1,] #To train the RF model
ValidSet <- RsForRandomForest_naOMIT[train==2,] #To validate the RF model
#summary(TrainSet)
#summary(ValidSet)

#Split data into chambers from convergent areas 
TrainSetIA <- subset(TrainSet, IBGP1 == "Cro" )
TrainSetIB <- subset(TrainSet, IBGP1 == "Shr" )
TrainSetIC <- subset(TrainSet, IBGP1 == "For" )
TrainSetID <- subset(TrainSet, IBGP1 == "Gra" )
TrainSetIE <- subset(TrainSet, IBGP1 == "Oth" )

ValidSetIA <- subset(ValidSet, IBGP1 == "Cro" )
ValidSetIB <- subset(ValidSet, IBGP1 == "Shr" )
ValidSetIC <- subset(ValidSet, IBGP1 == "For" )
ValidSetID <- subset(ValidSet, IBGP1 == "Gra" )
ValidSetIE <- subset(ValidSet, IBGP1 == "Oth" )

TrainSetA <- subset(TrainSet, TopClimatetype == "A" )
TrainSetB <- subset(TrainSet, TopClimatetype == "B" )
TrainSetC <- subset(TrainSet, TopClimatetype == "C" )
TrainSetD <- subset(TrainSet, TopClimatetype == "D" )
TrainSetE <- subset(TrainSet, TopClimatetype == "E" )

ValidSetA <- subset(ValidSet, TopClimatetype == "A" )
ValidSetB <- subset(ValidSet, TopClimatetype == "B" )
ValidSetC <- subset(ValidSet, TopClimatetype == "C" )
ValidSetD <- subset(ValidSet, TopClimatetype == "D" )
ValidSetE <- subset(ValidSet, TopClimatetype == "E" )

###Set seed to replicate random generation
#set.seed (502)
#set.seed (124)
#set.seed (101)
#set.seed (1001)
#set.seed (2346)
#set.seed (801)
#set.seed (1021)
#set.seed (5001)
set.seed (556)
#set.seed (12346)


########All—Ts数据#######
fit_rf1 <- randomForest(Rs_Norm ~ P_LastMonth + T_LastMonth + Measure_Month + TS 
                        + Pm + Tm +absLat #+ Longitude 
                        + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSet, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)

fit_rf1 <- randomForest(Rs_Norm ~ #P_LastMonth + T_LastMonth + Measure_Month + TS 
                         Pm + Tm,# +absLat #+ Longitude 
                        #+ Climate_Koeppon 
                       # + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          #Elevation + Silt + N + OC + PH, 
                       data=TrainSet, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)
#The summary of the Standard Random Forest model
fit_rf1
#Variable importance measures
importance (fit_rf1,type=2)
#Looking at the OOB error reduction with the tree size
plot (fit_rf1) 
#Plotting the variable importance
varImpPlot(fit_rf1)




#Generate predictions of validation data using Random Forest Model
prediction1.validation <- predict(fit_rf1, ValidSet) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction1.validation, ValidSet$Rs_Norm, method = "pearson", alpha=.05)

Validation <- cbind(prediction1.validation, ValidSet)
names(Validation)[1] <- "Prediction"

validation.lm <- lm(Rs_Norm~Prediction, data=Validation)
summary(validation.lm)



#使用训练集，评估预测性能
plant_predict <- predict(fit_rf1,TrainSet)

plot(TrainSet$Rs_Norm, plant_predict, main = '训练集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(TrainSet$Rs_Norm,plant_predict)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.83,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
plant_predict <- predict(fit_rf1, ValidSet)

plot(ValidSet$Rs_Norm, plant_predict, main = '测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSet$Rs_Norm,plant_predict)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.83,p<2.2e-16"),cex=1,font=2,col="#000000")

########################

########All数据#########

set.seed (556)

fit_rf <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                         Pm + Tm + Latitude +absLog 
                       + Climate_Koeppon + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                         Elevation + Silt + N + OC + PH, data=TrainSet, 
                       keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rf
#Variable importance measures
importance (fit_rf)
#Looking at the OOB error reduction with the tree size
plot (fit_rf) 
#Plotting the variable importance
varImpPlot(fit_rf)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otu <- data.frame(importance(fit_rf))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otu <- importance_otu[order(importance(fit_rf,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otu, 'importance_otu.csv')



#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validation <- predict(fit_rf, ValidSet) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validation, ValidSet$Rs_Norm, method = "pearson", alpha=.05)

Validation <- cbind(prediction.validation, ValidSet)
names(Validation)[1] <- "Prediction"

validation.lm <- lm(Rs_Norm~Prediction, data=Validation)
summary(validation.lm)

#使用训练集，评估预测性能
plant_predict <- predict(fit_rf,TrainSet)

plot(TrainSet$Rs_Norm, plant_predict,
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(TrainSet$Rs_Norm,plant_predict)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
plant_predict <- predict(fit_rf, ValidSet)

plot(ValidSet$Rs_Norm, plant_predict, main = '测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSet$Rs_Norm,plant_predict)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

######################

########A数据的测试##

set.seed (556)

fit_rfA <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                         Pm + Tm + Latitude + Longitude + Climate_Koeppon 
                       + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                         Elevation + Silt + N + OC + PH, data=TrainSetA, 
                       keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfA
#Variable importance measures
importance (fit_rfA)
#Looking at the OOB error reduction with the tree size
plot (fit_rfA) 
#Plotting the variable importance
varImpPlot(fit_rfA)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuA <- data.frame(importance(fit_rfA))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuA <- importance_otuA[order(importance(fit_rfA,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuA, 'importance_otuA.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationAA <- predict(fit_rfA, ValidSetA) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationAA, ValidSetA$Rs_Norm, method = "pearson", alpha=.05)

ValidationAA <- cbind(prediction.validationAA, ValidSetA)
names(ValidationAA)[1] <- "Prediction"

validation.lmAA <- lm(Rs_Norm~Prediction, data=ValidationAA)
summary(validation.lmAA)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationA <- predict(fit_rf, ValidSetA) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationA, ValidSetA$Rs_Norm, method = "pearson", alpha=.05)

ValidationA <- cbind(prediction.validationA, ValidSetA)
names(ValidationA)[1] <- "Prediction"

validation.lmA <- lm(Rs_Norm~Prediction, data=ValidationA)
summary(validation.lmA)

AIC(validation.lmAA, validation.lmA)

#使用All数据集，评估预测性能
RS_predictTA <- predict(fit_rf,ValidSetA)

plot(ValidSetA$Rs_Norm, RS_predictTA,main = 'AA测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetA$Rs_Norm,RS_predictTA)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
RS_predictVA <- predict(fit_rfA, ValidSetA)

plot(ValidSetA$Rs_Norm, RS_predictVA, main = 'A测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetA$Rs_Norm,RS_predictVA)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#########################

########B数据的测试#####

set.seed (556)

fit_rfB <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + Longitude + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetB, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfB
#Variable importance measures
importance (fit_rfB)
#Looking at the OOB error reduction with the tree size
plot (fit_rfB) 
#Plotting the variable importance
varImpPlot(fit_rfB)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuB <- data.frame(importance(fit_rfB))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuB <- importance_otuB[order(importance(fit_rfB,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuB, 'importance_otuB.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationBB <- predict(fit_rfB, ValidSetB) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationBB, ValidSetB$Rs_Norm, method = "pearson", alpha=.05)

ValidationBB <- cbind(prediction.validationBB, ValidSetB)
names(ValidationBB)[1] <- "Prediction"

validation.lmBB <- lm(Rs_Norm~Prediction, data=ValidationBB)
summary(validation.lmBB)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationB <- predict(fit_rf, ValidSetB) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationB, ValidSetB$Rs_Norm, method = "pearson", alpha=.05)

ValidationB <- cbind(prediction.validationB, ValidSetB)
names(ValidationB)[1] <- "Prediction"

validation.lmB <- lm(Rs_Norm~Prediction, data=ValidationB)
summary(validation.lmB)

AIC(validation.lmBB, validation.lmB)

#使用All数据集，评估预测性能
RS_predictTB <- predict(fit_rf,ValidSetB)

plot(ValidSetB$Rs_Norm, RS_predictTB,main = 'AB测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetB$Rs_Norm,RS_predictTB)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
RS_predictVB <- predict(fit_rfB, ValidSetB)

plot(ValidSetB$Rs_Norm, RS_predictVB, main = 'B测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetB$Rs_Norm,RS_predictVB)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

##########################

########C数据的测试######

set.seed (556)

fit_rfC <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + Longitude + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetC, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfC
#Variable importance measures
importance (fit_rfC)
#Looking at the OOB error reduction with the tree size
plot (fit_rfC) 
#Plotting the variable importance
varImpPlot(fit_rfC)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuC <- data.frame(importance(fit_rfC))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuC <- importance_otuC[order(importance(fit_rfC,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuC, 'importance_otuC.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationCC <- predict(fit_rfC, ValidSetC) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationCC, ValidSetC$Rs_Norm, method = "pearson", alpha=.05)

ValidationCC <- cbind(prediction.validationCC, ValidSetC)
names(ValidationCC)[1] <- "Prediction"

validation.lmCC <- lm(Rs_Norm~Prediction, data=ValidationCC)
summary(validation.lmCC)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationC <- predict(fit_rf, ValidSetC) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationC, ValidSetC$Rs_Norm, method = "pearson", alpha=.05)

ValidationC <- cbind(prediction.validationC, ValidSetC)
names(ValidationC)[1] <- "Prediction"

validation.lmC <- lm(Rs_Norm~Prediction, data=ValidationC)
summary(validation.lmC)

AIC(validation.lmCC, validation.lmC)

#使用All数据集，评估预测性能
RS_predictTC <- predict(fit_rf,ValidSetC)

plot(ValidSetC$Rs_Norm, RS_predictTC,main = 'AC测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetC$Rs_Norm,RS_predictTC)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
RS_predictVC <- predict(fit_rfC, ValidSetC)

plot(ValidSetC$Rs_Norm, RS_predictVC, main = 'C测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetC$Rs_Norm,RS_predictVC)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

##########################

########D数据的测试######

set.seed (556)

fit_rfD <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + Longitude + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetD, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfD
#Variable importance measures
importance (fit_rfD)
#Looking at the OOB error reduction with the tree size
plot (fit_rfD) 
#Plotting the variable importance
varImpPlot(fit_rfD)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuD <- data.frame(importance(fit_rfD))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuD <- importance_otuD[order(importance(fit_rfD,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuD, 'importance_otuD.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationDD <- predict(fit_rfD, ValidSetD) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationDD, ValidSetD$Rs_Norm, method = "pearson", alpha=.05)

ValidationDD <- cbind(prediction.validationDD, ValidSetD)
names(ValidationDD)[1] <- "Prediction"

validation.lmDD <- lm(Rs_Norm~Prediction, data=ValidationDD)
summary(validation.lmDD)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationD <- predict(fit_rf, ValidSetD) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationD, ValidSetD$Rs_Norm, method = "pearson", alpha=.05)

ValidationD <- cbind(prediction.validationD, ValidSetD)
names(ValidationD)[1] <- "Prediction"

validation.lmD <- lm(Rs_Norm~Prediction, data=ValidationD)
summary(validation.lmD)

AIC(validation.lmDD, validation.lmD)

#使用All数据集，评估预测性能
RS_predictTD <- predict(fit_rf,ValidSetD)

plot(ValidSetD$Rs_Norm, RS_predictTD,main = 'AD测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetD$Rs_Norm,RS_predictTD)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
RS_predictVD <- predict(fit_rfD, ValidSetD)

plot(ValidSetD$Rs_Norm, RS_predictVD, main = 'D测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetD$Rs_Norm,RS_predictVD)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")


##########################

########E数据的测试######

set.seed (556)

fit_rfE <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + Longitude + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetE, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfE
#Variable importance measures
importance (fit_rfE)
#Looking at the OOB error reduction with the tree size
plot (fit_rfE) 
#Plotting the variable importance
varImpPlot(fit_rfE)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuE <- data.frame(importance(fit_rfE))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuE <- importance_otuE[order(importance(fit_rfE,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuE, 'importance_otuE.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationEE <- predict(fit_rfE, ValidSetE) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationEE, ValidSetE$Rs_Norm, method = "pearson", alpha=.05)

ValidationEE <- cbind(prediction.validationEE, ValidSetE)
names(ValidationEE)[1] <- "Prediction"

validation.lmEE <- lm(Rs_Norm~Prediction, data=ValidationEE)
summary(validation.lmEE)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationE <- predict(fit_rf, ValidSetE) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationE, ValidSetE$Rs_Norm, method = "pearson", alpha=.05)

ValidationE <- cbind(prediction.validationE, ValidSetE)
names(ValidationE)[1] <- "Prediction"

validation.lmE <- lm(Rs_Norm~Prediction, data=ValidationE)
summary(validation.lmE)

AIC(validation.lmEE, validation.lmE)

#使用All数据集，评估预测性能
RS_predictTE <- predict(fit_rf,ValidSetE)

plot(ValidSetE$Rs_Norm, RS_predictTE,main = 'AD测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetE$Rs_Norm,RS_predictTE)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

#使用测试集，评估预测性能
RS_predictVE <- predict(fit_rfE, ValidSetE)

plot(ValidSetE$Rs_Norm, RS_predictVE, main = 'D测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetE$Rs_Norm,RS_predictVE)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, expression(R^2==0.67),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")



########################
#读取数据，在给定的示例数据中，我已经提前按照总丰度水平、样本类型等作了排序
#并在读取后将指定列转化为因子类型，即指定了预先定义顺序，便于后续作图展示
otu <- read.delim('im.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu$All <- factor(otu$All, levels = unique(otu$All))
otu$A <- factor(otu$A, levels = unique(otu$A))
otu$B <- factor(otu$B, levels = unique(otu$B))
otu$C <- factor(otu$C, levels = unique(otu$C))
otu$D <- factor(otu$D, levels = unique(otu$D))
otu$E <- factor(otu$E, levels = unique(otu$E))


#合并 OTU 丰度、OTU 门水平分类、样本分组


#整理成 ggplot2 作图格式，以便于 ggalluvial 绘制冲击图
otu$link <- 1
otu <- reshape::melt(otu, id = 'link')
names(otu) <- c('link', 'type', 'detail')
type <- summary(otu$type)
otu$flow <- rep(1:type[1], length(type))

#预指定 OTU、门分类、样本、分组的颜色
color_otu <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
               '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8',
               '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#66C2A5','#6181BD','#c7475b')
color_sample <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#049a0b')
color_phylum <- c('#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_group <- c('#4253ff', '#ff4308')

#冲击图
library(ggalluvial)

p <- ggplot(otu, aes(x = type, y =link  ,
                     stratum = detail, alluvium = flow, fill = detail)) +
  geom_stratum() +  #类似堆叠柱形图
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #在各个区块中添加文字标签
  geom_flow(aes.flow = 'backward') +  #绘制同类别之间的连接线
  scale_fill_manual(values = color_otu) +  #颜色赋值
  scale_x_discrete(limits = c('All', 'A', 'B', 'C','D','E')) +  #定义簇（列）的展示顺序
  #scale_y_continuous(expand = c(0, 0)) +
  labs(x = '', y = '') +  #从这儿开始是一些主题参数等的设置
  theme(legend.position = 'none', axis.line = element_line(),
        panel.background = element_blank())

p


########################
#读取数据，两个靶向关系表
otu <- read.delim('im.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu$All <- factor(otu$All, levels = unique(otu$All))
otu$A <- factor(otu$A, levels = unique(otu$A))
otu$B <- factor(otu$B, levels = unique(otu$B))
otu$C <- factor(otu$C, levels = unique(otu$C))
otu$D <- factor(otu$D, levels = unique(otu$D))
otu$E <- factor(otu$E, levels = unique(otu$E))



#整合靶向关系，合并为 3 列的表

#预指定颜色
color_circRNA <- '#8DD3C7'
color_miRNA <- c('#A65628', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
                 '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8',
                 '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33')
color_mRNA <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#66C2A5',
                '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_otu <- c('#8DD3C7', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
               '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8',
               '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33', '#A65628', '#F781BF', '#66C2A5','#6181BD','#c7475b')
#冲击图
otu$link <- 1
otu <- reshape::melt(otu, id = 'link')

variable <- summary(otu$variable)
otu$flow <- rep(1:variable[1], length(variable))

link <- 1 / summary(otu$value)
for (i in names(link)) otu[which(otu$value == i),'link'] <- link[i]

library(ggalluvial)
#冲击图
p <- ggplot(otu, aes(x = variable, y = link,
                           stratum = value, alluvium = flow, fill = value)) +
  geom_stratum() +  #类似堆叠柱形图
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #在各个区块中添加文字标签
  geom_flow(aes.flow = 'backward') +  #这次，连线颜色就不用 miRNA 赋值了，将 mRNA 也加进来，看一下效果
  scale_fill_manual(values = color_otu) +  #颜色赋值
  scale_x_discrete(limits = c('All', 'A', 'B', 'C','D','E')) +  #定义簇（列）的展示顺序
  #scale_y_continuous(expand = c(0, 0))+
  labs(x = " ", y = '') +  #从这儿开始是一些主题参数等的设置
  theme(legend.position = 'none', panel.background = element_blank(),
        line = element_blank(), axis.text.x = element_text(colour = "blue"),axis.text.y=element_blank())

p



library(tidyverse)
library(ggplot2)
library(gghalves)
library(ggridges)
library(cols4all)

A <- subset(RsForRandomForest_naOMIT, TopClimatetype == "A" )
B <- subset(RsForRandomForest_naOMIT, TopClimatetype == "B" )
C <- subset(RsForRandomForest_naOMIT, TopClimatetype == "C" )
D <- subset(RsForRandomForest_naOMIT, TopClimatetype == "D" )
E <- subset(RsForRandomForest_naOMIT, TopClimatetype == "E" )

#write.csv(A$Rs_Norm, 'RSA.csv')
#write.csv(B$Rs_Norm, 'RSB.csv')
#write.csv(C$Rs_Norm, 'RSC.csv')
#write.csv(D$Rs_Norm, 'RSD.csv')
#write.csv(E$Rs_Norm, 'RSE.csv')
RSA<- read_csv("E:/R/RSA.csv",locale=locale(encoding="GBK"))

dtt <- RSA %>%
  as_tibble(rownames = 'gene') %>%
  gather(key = Koeppon,
         value = exp,
         -gene)
dtt$Koeppon <- factor(dtt$Koeppon, levels = unique(dtt$Koeppon)) #转换为因子，固定绘图celltype顺序
head(dtt)

c4a_gui()
mycol1 <- c4a('10',6)
mycol2 <- c4a('bold',6)

#山脊图添加密度短竖线：

p5 <- ggplot(data = dtt, aes(x = exp, y = Koeppon, fill = Koeppon)) +
  geom_density_ridges(alpha = 0.8,
                      color = 'white',
                      rel_min_height = 0.01, #尾部修剪，数值越大修剪程度越高
                      scale = 1.6, #山脊重叠程度调整，scale = 1时刚好触及基线，数值越大重叠度越高
                      quantile_lines = TRUE, #显示分位数线
                      quantiles = 2 #仅显示中位数线
  ) +labs(x = "RS", y = 'Koeppon')+scale_x_continuous(limits =c(-1,13))+
  scale_fill_manual(values = mycol1) +
  theme_classic()
p5

#################################################################








###########################

set.seed (556)

mod <- randomForest(Rs_Norm ~ P_LastMonth  + Measure_Month + #TS + 
                         Pm + Tm + Latitude +absLog + Climate_Koeppon 
                      + IGBP + LAI  + Elevation , data=TrainSet, 
                       keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
mod
#Variable importance measures
importance (mod)
#Looking at the OOB error reduction with the tree size
plot (mod) 
#Plotting the variable importance
varImpPlot(mod)

CK<-TrainSet$Climate_Koeppon
head(CK)
K<-globalData$IGBP
head(K)

globalData<- read_csv("E:/论文/图片/globalData.csv",locale=locale(encoding="GBK"))
#globalData <- data.frame(globalData)
#globalData <-filter(globalData,Climate_Koeppon %in% TrainSet$Climate_Koeppon)
#globalData <-filter(globalData, IGBP %in% TrainSet$IGBP)

globalData$Climate_Koeppon <- as.factor(globalData$Climate_Koeppon)
globalData$IGBP <- as.factor(globalData$IGBP)
globalData$Measure_Month <- as.factor(globalData$Measure_Month)
globalData$absLat <- abs(globalData$Latitude)
globalData$absLog <- abs(globalData$Longitude)
#globalData$Rs_Norm<-0

globalData$prediction <- predict(mod, globalData)


G10 <- subset(globalData, Measure_Month==10 )
theme_update(plot.title = element_text(hjust = 0.5))


# Plot predictions globally
ggplot(G10, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()

globalDataA<-subset(G10, G10$TopClimate=="A")
globalDataA <-filter(globalData, TopClimate %in% globalDataA$TopClimate)
GA10 <-subset(globalDataA,Measure_Month==10)
GA10 <- data.frame(GA10)


ggplot(GA10, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()


# Look at the distribution of predictions
ggplot(G5, aes(x = prediction)) +
  geom_histogram(color = "black", fill = "lightgrey") +
  labs(x = "RC Prediction")



intA <- c('A', 'A', 'A', 'D', 'D', 'F', 'L')
intB <- c('B', 'D', 'T', 'T', 'B', 'K', 'M')
num <- c(1:7)
num2 <- c(6,6,7,7,7,6,7)
df <- data.frame(intA, intB, num, num2)
df

library(dplyr)
p <- c('A', 'T')

filter(df, intA %in% p)
filter(df, intB %in% p)


