#########Climate Model##########

set.seed (556)

fit_rfA <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
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

rmseTA <- sqrt(mean((ValidSetA$Rs_Norm - RS_predictTA )^2))
rmseTA
# 计算观测值数量
n <- length(RS_predictTA)

# 计算每个观测值的差异，并求平方
diff <- ValidSetA$Rs_Norm - RS_predictTA
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVA <- predict(fit_rfA, ValidSetA)

plot(ValidSetA$Rs_Norm, RS_predictVA, main = 'A测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetA$Rs_Norm,RS_predictVA)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVA <- sqrt(mean((ValidSetA$Rs_Norm - RS_predictVA )^2))
rmseVA
# 计算观测值数量
n <- length(RS_predictVA)

# 计算每个观测值的差异，并求平方
diff <- ValidSetA$Rs_Norm - RS_predictVA
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)
####

########B数据的测试##

set.seed (556)

fit_rfB <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
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

rmseTB <- sqrt(mean((ValidSetB$Rs_Norm - RS_predictTB )^2))
rmseTB
# 计算观测值数量
n <- length(RS_predictTB)

# 计算每个观测值的差异，并求平方
diff <- ValidSetB$Rs_Norm - RS_predictTB
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVB <- predict(fit_rfB, ValidSetB)

plot(ValidSetB$Rs_Norm, RS_predictVB, main = 'B测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetB$Rs_Norm,RS_predictVB)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVB <- sqrt(mean((ValidSetB$Rs_Norm - RS_predictVB )^2))
rmseVB
# 计算观测值数量
n <- length(RS_predictVB)

# 计算每个观测值的差异，并求平方
diff <- ValidSetB$Rs_Norm - RS_predictVB
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

##########################

########C数据的测试######

set.seed (556)

fit_rfC <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
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

rmseTC <- sqrt(mean((ValidSetC$Rs_Norm - RS_predictTC )^2))
rmseTC
# 计算观测值数量
n <- length(RS_predictTC)

# 计算每个观测值的差异，并求平方
diff <- ValidSetC$Rs_Norm - RS_predictTC
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVC <- predict(fit_rfC, ValidSetC)

plot(ValidSetC$Rs_Norm, RS_predictVC, main = 'C测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetC$Rs_Norm,RS_predictVC)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVC <- sqrt(mean((ValidSetC$Rs_Norm - RS_predictVC )^2))
rmseVC
# 计算观测值数量
n <- length(RS_predictVC)

# 计算每个观测值的差异，并求平方
diff <- ValidSetC$Rs_Norm - RS_predictVC
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

##########################

########D数据的测试######

set.seed (556)

fit_rfD <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
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

rmseTD <- sqrt(mean((ValidSetD$Rs_Norm - RS_predictTD )^2))
rmseTD
# 计算观测值数量
n <- length(RS_predictTD)

# 计算每个观测值的差异，并求平方
diff <- ValidSetD$Rs_Norm - RS_predictTD
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVD <- predict(fit_rfD, ValidSetD)

plot(ValidSetD$Rs_Norm, RS_predictVD, main = 'D测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetD$Rs_Norm,RS_predictVD)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVD <- sqrt(mean((ValidSetD$Rs_Norm - RS_predictVD )^2))
rmseVD
# 计算观测值数量
n <- length(RS_predictVD)

# 计算每个观测值的差异，并求平方
diff <- ValidSetD$Rs_Norm - RS_predictVD
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

##########################

########E数据的测试######

set.seed (556)

fit_rfE <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
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

plot(ValidSetE$Rs_Norm, RS_predictTE,main = 'AE测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetE$Rs_Norm,RS_predictTE)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTE <- sqrt(mean((ValidSetE$Rs_Norm - RS_predictTE )^2))
rmseTE
# 计算观测值数量
n <- length(RS_predictTE)

# 计算每个观测值的差异，并求平方
diff <- ValidSetE$Rs_Norm - RS_predictTE
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVE <- predict(fit_rfE, ValidSetE)

plot(ValidSetE$Rs_Norm, RS_predictVE, main = 'E测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetE$Rs_Norm,RS_predictVE)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, expression(R^2==0.67),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")



rmseVE <- sqrt(mean((ValidSetE$Rs_Norm - RS_predictVE )^2))
rmseVE
# 计算观测值数量
n <- length(RS_predictVE)

# 计算每个观测值的差异，并求平方
diff <- ValidSetE$Rs_Norm - RS_predictVE
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

validationABC <- rbind(ValidationAA, ValidationBB,ValidationCC,ValidationDD,ValidationEE)
names(validationABC)[1] <- "Prediction"

#write.csv(validationABC,"validationABC.csv")

validation.lmABC <- lm(Rs_Norm~Prediction, data=validationABC)
summary(validation.lmABC)
cor.test(validationABC$Prediction, validationABC$Rs_Norm, method = "spearman", alpha=.05)

plot(validationABC$Rs_Norm,validationABC$Prediction, main = 'Climate model',
     xlab = 'Observated data', ylab = 'Predicted data', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(validationABC$Rs_Norm,validationABC$Prediction)
abline(coef(z),lwd=2, lty = 2,xaxs="s",col = "red")
text(2, 10, expression(R^2==0.677),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

mean(validationABC$Prediction,na.rm = T)

rmse <- sqrt(mean((validationABC$Prediction - validationABC$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(validationABC$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- validationABC$Prediction - validationABC$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

AIC(validation.lm, validation.lmABC)

mean(RS_predictTA,na.rm = T)
mean(RS_predictVA,na.rm = T)

mean(RS_predictTB,na.rm = T)
mean(RS_predictVB,na.rm = T)

mean(RS_predictTC,na.rm = T)
mean(RS_predictVC,na.rm = T)

mean(RS_predictTD,na.rm = T)
mean(RS_predictVD,na.rm = T)

mean(RS_predictTE,na.rm = T)
mean(RS_predictVE,na.rm = T)

e <- 1-((sum(squared_diff))/sum((validationABC$Rs_Norm - mean(validationABC$Rs_Norm))^2))
e

mre <- sum(validationABC$Prediction-validationABC$Rs_Norm)/mean(validationABC$Rs_Norm)/n
mre



#读取数据，两个靶向关系表
otu <- read.delim('im.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otu$All <- factor(otu$All, levels = unique(otu$All))
otu$A <- factor(otu$A, levels = unique(otu$A))
otu$B <- factor(otu$B, levels = unique(otu$B))
otu$C <- factor(otu$C, levels = unique(otu$C))
otu$D <- factor(otu$D, levels = unique(otu$D))
otu$E <- factor(otu$E, levels = unique(otu$E))

#预指定颜色
color_circRNA <- '#8DD3C7'
color_miRNA <- c('#A65628', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
                 '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8', '#984EA3', '#FF7F00', '#FFFF33')
color_mRNA <- c( '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#66C2A5',
                 '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_otu <- c('#FD8072','#FDB462','#FF9F22','#FF7F00','#cc6600','#996633', '#999933', '#669900', '#4DAF4A', '#339900','#009933','#339955','#006400','#80B1D3','#377EB8','#386CB0','#6181BD','#BEBAE0','#BC80BD','#984EA3','#c7475b','#E41A1C','#F0027F','#F781BF','#FCCDE5')
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