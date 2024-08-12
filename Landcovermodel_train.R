###########Land cover model############
########A数据的测试##

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

set.seed (556)

fit_rfIA <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetIA, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfIA
#Variable importance measures
importance (fit_rfIA)
#Looking at the OOB error reduction with the tree size
plot (fit_rfIA) 
#Plotting the variable importance
varImpPlot(fit_rfIA)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuIA <- data.frame(importance(fit_rfIA))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuIA <- importance_otuIA[order(importance(fit_rfIA,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuIA, 'importance_otuIA.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationIAA <- predict(fit_rfIA, ValidSetIA) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIAA, ValidSetIA$Rs_Norm, method = "pearson", alpha=.05)

ValidationIAA <- cbind(prediction.validationIAA, ValidSetIA)
names(ValidationIAA)[1] <- "Prediction"

validation.lmIAA <- lm(Rs_Norm~Prediction, data=ValidationIAA)
summary(validation.lmIAA)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationIA <- predict(fit_rf, ValidSetIA) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIA, ValidSetIA$Rs_Norm, method = "pearson", alpha=.05)

ValidationIA <- cbind(prediction.validationIA, ValidSetIA)
names(ValidationIA)[1] <- "Prediction"

validation.lmIA <- lm(Rs_Norm~Prediction, data=ValidationIA)
summary(validation.lmIA)

AIC(validation.lmIAA, validation.lmIA)

#使用All数据集，评估预测性能
RS_predictTIA <- predict(fit_rf,ValidSetIA)

plot(ValidSetIA$Rs_Norm, RS_predictTIA,main = 'AA测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIA$Rs_Norm,RS_predictTIA)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTIA <- sqrt(mean((ValidSetIA$Rs_Norm - RS_predictTIA )^2))
rmseTIA
# 计算观测值数量
n <- length(RS_predictTIA)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIA$Rs_Norm - RS_predictTIA
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVIA <- predict(fit_rfIA, ValidSetIA)

plot(ValidSetIA$Rs_Norm, RS_predictVIA, main = 'A测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIA$Rs_Norm,RS_predictVIA)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVIA <- sqrt(mean((ValidSetIA$Rs_Norm - RS_predictVIA )^2))
rmseVIA
# 计算观测值数量
n <- length(RS_predictVIA)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIA$Rs_Norm - RS_predictVIA
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

####

########B数据的测试##

set.seed (556)

fit_rfIB <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetIB, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfIB
#Variable importance measures
importance (fit_rfIB)
#Looking at the OOB error reduction with the tree size
plot (fit_rfIB) 
#Plotting the variable importance
varImpPlot(fit_rfIB)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuIB <- data.frame(importance(fit_rfIB))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuIB <- importance_otuIB[order(importance(fit_rfIB,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuIB, 'importance_otuIB.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationIBB <- predict(fit_rfIB, ValidSetIB) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIBB, ValidSetIB$Rs_Norm, method = "pearson", alpha=.05)

ValidationIBB <- cbind(prediction.validationIBB, ValidSetIB)
names(ValidationIBB)[1] <- "Prediction"

validation.lmIBB <- lm(Rs_Norm~Prediction, data=ValidationIBB)
summary(validation.lmIBB)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationIB <- predict(fit_rf, ValidSetIB) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIB, ValidSetIB$Rs_Norm, method = "pearson", alpha=.05)

ValidationIB <- cbind(prediction.validationIB, ValidSetIB)
names(ValidationIB)[1] <- "Prediction"

validation.lmIB <- lm(Rs_Norm~Prediction, data=ValidationIB)
summary(validation.lmIB)

AIC(validation.lmIBB, validation.lmIB)

#使用All数据集，评估预测性能
RS_predictTIB <- predict(fit_rf,ValidSetIB)

plot(ValidSetIB$Rs_Norm, RS_predictTIB,main = 'AB测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIB$Rs_Norm,RS_predictTIB)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTIB <- sqrt(mean((ValidSetIB$Rs_Norm - RS_predictTIB )^2))
rmseTIB

# 计算观测值数量
n <- length(RS_predictTIB)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIB$Rs_Norm - RS_predictTIB
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVIB <- predict(fit_rfIB, ValidSetIB)

plot(ValidSetIB$Rs_Norm, RS_predictVIB, main = 'B测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIB$Rs_Norm,RS_predictVIB)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVIB <- sqrt(mean((ValidSetIB$Rs_Norm - RS_predictVIB )^2))
rmseVIB
# 计算观测值数量
n <- length(RS_predictVIB)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIB$Rs_Norm - RS_predictVIB
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

##########################

########C数据的测试######

set.seed (556)

fit_rfIC <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetIC, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfIC
#Variable importance measures
importance (fit_rfIC)
#Looking at the OOB error reduction with the tree size
plot (fit_rfIC) 
#Plotting the variable importance
varImpPlot(fit_rfIC)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuIC <- data.frame(importance(fit_rfIC))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuIC <- importance_otuIC[order(importance(fit_rfIC,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuIC, 'importance_otuIC.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationICC <- predict(fit_rfIC, ValidSetIC) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationICC, ValidSetIC$Rs_Norm, method = "pearson", alpha=.05)

ValidationICC <- cbind(prediction.validationICC, ValidSetIC)
names(ValidationICC)[1] <- "Prediction"

validation.lmICC <- lm(Rs_Norm~Prediction, data=ValidationICC)
summary(validation.lmICC)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationIC <- predict(fit_rf, ValidSetIC) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIC, ValidSetIC$Rs_Norm, method = "pearson", alpha=.05)

ValidationIC <- cbind(prediction.validationIC, ValidSetIC)
names(ValidationIC)[1] <- "Prediction"

validation.lmIC <- lm(Rs_Norm~Prediction, data=ValidationIC)
summary(validation.lmIC)

AIC(validation.lmICC, validation.lmIC)

#使用All数据集，评估预测性能
RS_predictTIC <- predict(fit_rf,ValidSetIC)

plot(ValidSetIC$Rs_Norm, RS_predictTIC,main = 'AC测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIC$Rs_Norm,RS_predictTIC)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTIC <- sqrt(mean((ValidSetIC$Rs_Norm - RS_predictTIC )^2))
rmseTIC
# 计算观测值数量
n <- length(RS_predictTIC)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIC$Rs_Norm - RS_predictTIC
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVIC <- predict(fit_rfIC, ValidSetIC)

plot(ValidSetIC$Rs_Norm, RS_predictVIC, main = 'C测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIC$Rs_Norm,RS_predictVIC)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVIC <- sqrt(mean((ValidSetIC$Rs_Norm - RS_predictVIC )^2))
rmseVIC
# 计算观测值数量
n <- length(RS_predictVIC)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIC$Rs_Norm - RS_predictVIC
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

##########################

########D数据的测试######

set.seed (556)

fit_rfID <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetID, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfID
#Variable importance measures
importance (fit_rfID)
#Looking at the OOB error reduction with the tree size
plot (fit_rfID) 
#Plotting the variable importance
varImpPlot(fit_rfID)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuID <- data.frame(importance(fit_rfID))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuID <- importance_otuID[order(importance(fit_rfID,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuID, 'importance_otuID.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationIDD <- predict(fit_rfID, ValidSetID) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIDD, ValidSetID$Rs_Norm, method = "pearson", alpha=.05)

ValidationIDD <- cbind(prediction.validationIDD, ValidSetID)
names(ValidationIDD)[1] <- "Prediction"

validation.lmIDD <- lm(Rs_Norm~Prediction, data=ValidationIDD)
summary(validation.lmIDD)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationID <- predict(fit_rf, ValidSetID) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationID, ValidSetID$Rs_Norm, method = "pearson", alpha=.05)

ValidationID <- cbind(prediction.validationID, ValidSetID)
names(ValidationID)[1] <- "Prediction"

validation.lmID <- lm(Rs_Norm~Prediction, data=ValidationID)
summary(validation.lmID)

AIC(validation.lmIDD, validation.lmID)

#使用All数据集，评估预测性能
RS_predictTID <- predict(fit_rf,ValidSetID)

plot(ValidSetID$Rs_Norm, RS_predictTID,main = 'AD测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetID$Rs_Norm,RS_predictTID)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTID <- sqrt(mean((ValidSetID$Rs_Norm - RS_predictTID )^2))
rmseTID
# 计算观测值数量
n <- length(RS_predictTID)

# 计算每个观测值的差异，并求平方
diff <- ValidSetID$Rs_Norm - RS_predictTID
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVID <- predict(fit_rfID, ValidSetID)

plot(ValidSetID$Rs_Norm, RS_predictVID, main = 'D测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetID$Rs_Norm,RS_predictVID)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVID <- sqrt(mean((ValidSetID$Rs_Norm - RS_predictVID )^2))
rmseVID
# 计算观测值数量
n <- length(RS_predictVID)

# 计算每个观测值的差异，并求平方
diff <- ValidSetID$Rs_Norm - RS_predictVID
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)


##########################

########E数据的测试######

set.seed (556)

fit_rfIE <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetIE, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfIE
#Variable importance measures
importance (fit_rfIE)
#Looking at the OOB error reduction with the tree size
plot (fit_rfIE) 
#Plotting the variable importance
varImpPlot(fit_rfIE)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuIE <- data.frame(importance(fit_rfIE))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuIE <- importance_otuIE[order(importance(fit_rfIE,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuIE, 'importance_otuIE.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationIEE <- predict(fit_rfIE, ValidSetIE) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIEE, ValidSetIE$Rs_Norm, method = "pearson", alpha=.05)

ValidationIEE <- cbind(prediction.validationIEE, ValidSetIE)
names(ValidationIEE)[1] <- "Prediction"

validation.lmIEE <- lm(Rs_Norm~Prediction, data=ValidationIEE)
summary(validation.lmIEE)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationIE <- predict(fit_rf, ValidSetIE) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationIE, ValidSetIE$Rs_Norm, method = "pearson", alpha=.05)

ValidationIE <- cbind(prediction.validationIE, ValidSetIE)
names(ValidationIE)[1] <- "Prediction"

validation.lmIE <- lm(Rs_Norm~Prediction, data=ValidationIE)
summary(validation.lmIE)

AIC(validation.lmIEE, validation.lmIE)

#使用All数据集，评估预测性能
RS_predictTIE <- predict(fit_rf,ValidSetIE)

plot(ValidSetIE$Rs_Norm, RS_predictTIE,main = 'AE测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIE$Rs_Norm,RS_predictTIE)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTIE <- sqrt(mean((ValidSetIE$Rs_Norm - RS_predictTIE )^2))
rmseTIE
# 计算观测值数量
n <- length(RS_predictTIE)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIE$Rs_Norm - RS_predictTIE
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVIE <- predict(fit_rfIE, ValidSetIE)

plot(ValidSetIE$Rs_Norm, RS_predictVIE, main = 'E测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetIE$Rs_Norm,RS_predictVIE)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, expression(R^2==0.67),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

rmseVIE <- sqrt(mean((ValidSetIE$Rs_Norm - RS_predictVIE )^2))
rmseVIE
# 计算观测值数量
n <- length(RS_predictVIE)

# 计算每个观测值的差异，并求平方
diff <- ValidSetIE$Rs_Norm - RS_predictVIE
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

validationIABC <- rbind(ValidationIAA, ValidationIBB,ValidationICC,ValidationIDD,ValidationIEE)
names(validationIABC)[1] <- "Prediction"

validation.lmIABC <- lm(Rs_Norm~Prediction, data=validationIABC)
summary(validation.lmIABC)

cor.test(validationIABC$Prediction, validationIABC$Rs_Norm, method = "spearman", alpha=.05)

plot(validationIABC$Rs_Norm,validationIABC$Prediction, main = 'Land_cover model',
     xlab = 'Observated data', ylab = 'Predicted data', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(validationIABC$Rs_Norm,validationIABC$Prediction)
abline(coef(z),lwd=2, lty = 2,xaxs="s",col = "red")
text(2, 10, expression(R^2==0.679),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

mean(validationIABC$Prediction,na.rm = T)

rmse <- sqrt(mean((validationIABC$Prediction - validationIABC$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(validationIABC$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- validationIABC$Prediction - validationIABC$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

AIC(validation.lm, validation.lmIABC)

mean(RS_predictTIA,na.rm = T)
mean(RS_predictVIA,na.rm = T)

mean(RS_predictTIB,na.rm = T)
mean(RS_predictVIB,na.rm = T)

mean(RS_predictTIC,na.rm = T)
mean(RS_predictVIC,na.rm = T)

mean(RS_predictTID,na.rm = T)
mean(RS_predictVID,na.rm = T)

mean(RS_predictTIE,na.rm = T)
mean(RS_predictVIE,na.rm = T)

e <- 1-((sum(squared_diff))/sum((validationIABC$Rs_Norm - mean(validationIABC$Rs_Norm))^2))
e

mre <- sum(validationIABC$Prediction-validationIABC$Rs_Norm)/mean(validationIABC$Rs_Norm)/n
mre


#读取数据，两个靶向关系表
otuI <- read.delim('IIm.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otuI$All <- factor(otuI$All, levels = unique(otuI$All))
otuI$Cro <- factor(otuI$Cro, levels = unique(otuI$Cro))
otuI$Shr <- factor(otuI$Shr, levels = unique(otuI$Shr))
otuI$For <- factor(otuI$For, levels = unique(otuI$For))
otuI$Gra <- factor(otuI$Gra, levels = unique(otuI$Gra))
otuI$Oth <- factor(otuI$Oth, levels = unique(otuI$Oth))

#预指定颜色
color_circRNA <- '#8DD3C7'
color_miRNA <- c('#A65628', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
                 '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8',
                 '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33')
color_mRNA <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#66C2A5',
                '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_otu <- c('#FD8072','#FDB462','#FF9F22','#FF7F00','#cc6600','#996633', '#999933', '#669900', '#4DAF4A', '#339900','#009933','#339955','#006400','#80B1D3','#377EB8','#386CB0','#6181BD','#BEBAE0','#BC80BD','#984EA3','#c7475b','#E41A1C','#F0027F','#F781BF','#FCCDE5')
#冲击图
otuI$link <- 1
otuI <- reshape::melt(otuI, id = 'link')

variable <- summary(otuI$variable)
otuI$flow <- rep(1:variable[1], length(variable))

link <- 1 / summary(otuI$value)
for (i in names(link)) otuI[which(otuI$value == i),'link'] <- link[i]

library(ggalluvial)
#冲击图
p <- ggplot(otuI, aes(x = variable, y = link,
                      stratum = value, alluvium = flow, fill = value)) +
  geom_stratum() +  #类似堆叠柱形图
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #在各个区块中添加文字标签
  geom_flow(aes.flow = 'backward') +  #这次，连线颜色就不用 miRNA 赋值了，将 mRNA 也加进来，看一下效果
  scale_fill_manual(values = color_otu) +  #颜色赋值
  scale_x_discrete(limits = c('All', 'Cro', 'Shr', 'For','Gra','Oth')) +  #定义簇（列）的展示顺序
  #scale_y_continuous(expand = c(0, 0))+
  labs(x = " ", y = '') +  #从这儿开始是一些主题参数等的设置
  theme(legend.position = 'none', panel.background = element_blank(),
        line = element_blank(), axis.text.x = element_text(colour = "blue"),axis.text.y=element_blank())

p