############elevation model#################

##海拔划分##

set.seed (556)

fit_rfL <- randomForest(Rs_Norm ~ P_LastMonth  + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon +  T_LastMonth
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetL, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfL
#Variable importance measures
importance (fit_rfL)
#Looking at the OOB error reduction with the tree size
plot (fit_rfL) 
#Plotting the variable importance
varImpPlot(fit_rfL)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuL <- data.frame(importance(fit_rfL))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuL <- importance_otuL[order(importance(fit_rfL,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
#write.csv(importance_otuL, 'importance_otuL.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationLL <- predict(fit_rfL, ValidSetL) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationLL, ValidSetL$Rs_Norm, method = "pearson", alpha=.05)

ValidationLL <- cbind(prediction.validationLL, ValidSetL)
names(ValidationLL)[1] <- "Prediction"

validation.lmLL <- lm(Rs_Norm~Prediction, data=ValidationLL)
summary(validation.lmLL)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationL <- predict(fit_rf, ValidSetL) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationL, ValidSetL$Rs_Norm, method = "pearson", alpha=.05)

ValidationL <- cbind(prediction.validationL, ValidSetL)
names(ValidationL)[1] <- "Prediction"

validation.lmL <- lm(Rs_Norm~Prediction, data=ValidationL)
summary(validation.lmL)

AIC(validation.lmLL, validation.lmL)

#使用All数据集，评估预测性能
RS_predictTL <- predict(fit_rf,ValidSetL)

plot(ValidSetL$Rs_Norm, RS_predictTL,main = 'AD测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetL$Rs_Norm,RS_predictTL)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTL <- sqrt(mean((ValidSetL$Rs_Norm - RS_predictTL )^2))
rmseTL
# 计算观测值数量
n <- length(RS_predictTL)

# 计算每个观测值的差异，并求平方
diff <- ValidSetL$Rs_Norm - RS_predictTL
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVL <- predict(fit_rfL, ValidSetL)

plot(ValidSetL$Rs_Norm, RS_predictVL, main = 'D测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetL$Rs_Norm,RS_predictVL)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVL <- sqrt(mean((ValidSetL$Rs_Norm - RS_predictVL )^2))
rmseVL
# 计算观测值数量
n <- length(RS_predictVL)

# 计算每个观测值的差异，并求平方
diff <- ValidSetL$Rs_Norm - RS_predictVL
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)
##########################

########M数据的测试######

set.seed (556)

fit_rfM <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetM, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfM
#Variable importance measures
importance (fit_rfM)
#Looking at the OOB error reduction with the tree size
plot (fit_rfM) 
#Plotting the variable importance
varImpPlot(fit_rfM)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuM <- data.frame(importance(fit_rfM))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuM <- importance_otuM[order(importance(fit_rfM,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
#write.csv(importance_otuM, 'importance_otuM.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationMM <- predict(fit_rfM, ValidSetM) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationMM, ValidSetM$Rs_Norm, method = "pearson", alpha=.05)

ValidationMM <- cbind(prediction.validationMM, ValidSetM)
names(ValidationMM)[1] <- "Prediction"

validation.lmMM <- lm(Rs_Norm~Prediction, data=ValidationMM)
summary(validation.lmMM)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationM <- predict(fit_rf, ValidSetM) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationM, ValidSetM$Rs_Norm, method = "pearson", alpha=.05)

ValidationM <- cbind(prediction.validationM, ValidSetM)
names(ValidationM)[1] <- "Prediction"

validation.lmM <- lm(Rs_Norm~Prediction, data=ValidationM)
summary(validation.lmM)

AIC(validation.lmMM, validation.lmM)

#使用All数据集，评估预测性能
RS_predictTM <- predict(fit_rf,ValidSetM)

plot(ValidSetM$Rs_Norm, RS_predictTM,main = 'AE测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetM$Rs_Norm,RS_predictTM)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTM <- sqrt(mean((ValidSetM$Rs_Norm - RS_predictTM )^2))
rmseTM
# 计算观测值数量
n <- length(RS_predictTM)

# 计算每个观测值的差异，并求平方
diff <- ValidSetM$Rs_Norm - RS_predictTM
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVM <- predict(fit_rfM, ValidSetM)

plot(ValidSetM$Rs_Norm, RS_predictVM, main = 'E测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetM$Rs_Norm,RS_predictVM)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, expression(R^2==0.67),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

rmseVM <- sqrt(mean((ValidSetM$Rs_Norm - RS_predictVM )^2))
rmseVM
# 计算观测值数量
n <- length(RS_predictVM)

# 计算每个观测值的差异，并求平方
diff <- ValidSetM$Rs_Norm - RS_predictVM
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)
########H数据的测试######

set.seed (556)

fit_rfH <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                          Pm + Tm + Latitude + absLog + Climate_Koeppon 
                        + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                          Elevation + Silt + N + OC + PH, data=TrainSetH, 
                        keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfH
#Variable importance measures
importance (fit_rfH)
#Looking at the OOB error reduction with the tree size
plot (fit_rfH) 
#Plotting the variable importance
varImpPlot(fit_rfH)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuH <- data.frame(importance(fit_rfH))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuH <- importance_otuH[order(importance(fit_rfH,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
#write.csv(importance_otuH, 'importance_otuH.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationHH <- predict(fit_rfH, ValidSetH) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationHH, ValidSetH$Rs_Norm, method = "pearson", alpha=.05)

ValidationHH <- cbind(prediction.validationHH, ValidSetH)
names(ValidationHH)[1] <- "Prediction"

validation.lmHH <- lm(Rs_Norm~Prediction, data=ValidationHH)
summary(validation.lmHH)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationH <- predict(fit_rf, ValidSetH) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationH, ValidSetH$Rs_Norm, method = "pearson", alpha=.05)

ValidationH <- cbind(prediction.validationH, ValidSetH)
names(ValidationH)[1] <- "Prediction"

validation.lmH <- lm(Rs_Norm~Prediction, data=ValidationH)
summary(validation.lmH)

AIC(validation.lmHH, validation.lmH)

#使用All数据集，评估预测性能
RS_predictTH <- predict(fit_rf,ValidSetH)

plot(ValidSetH$Rs_Norm, RS_predictTH,main = 'AE测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetH$Rs_Norm,RS_predictTH)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTH <- sqrt(mean((ValidSetH$Rs_Norm - RS_predictTH )^2))
rmseTH
# 计算观测值数量
n <- length(RS_predictTH)

# 计算每个观测值的差异，并求平方
diff <- ValidSetH$Rs_Norm - RS_predictTH
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVH <- predict(fit_rfH, ValidSetH)

plot(ValidSetH$Rs_Norm, RS_predictVH, main = 'E测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetH$Rs_Norm,RS_predictVH)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, expression(R^2==0.67),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

rmseVH <- sqrt(mean((ValidSetH$Rs_Norm - RS_predictVH )^2))
rmseVH
# 计算观测值数量
n <- length(RS_predictVH)

# 计算每个观测值的差异，并求平方
diff <- ValidSetH$Rs_Norm - RS_predictVH
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

validationLMH <- rbind(ValidationLL, ValidationMM,ValidationHH)
names(validationLMH)[1] <- "Prediction"

validation.lmLMH <- lm(Rs_Norm~Prediction, data=validationLMH)
summary(validation.lmLMH)

cor.test(validationLMH$Prediction, validationLMH$Rs_Norm, method = "spearman", alpha=.05)

plot(validationLMH$Rs_Norm,validationLMH$Prediction, main = 'Elevation model',
     xlab = 'Observated data', ylab = 'Predicted data', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(validationLMH$Rs_Norm,validationLMH$Prediction)
abline(coef(z),lwd=2, lty = 2,xaxs="s",col = "red")
text(2, 10, expression(R^2==0.676),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

mean(validationLMH$Prediction,na.rm = T)

rmse <- sqrt(mean((validationLMH$Prediction - validationLMH$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(validationLMH$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- validationLMH$Prediction - validationLMH$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

AIC(validation.lm, validation.lmLMH)


mean(RS_predictTL,na.rm = T)
mean(RS_predictVL,na.rm = T)

mean(RS_predictTM,na.rm = T)
mean(RS_predictVM,na.rm = T)

mean(RS_predictTH,na.rm = T)
mean(RS_predictVH,na.rm = T)

e <- 1-((sum(squared_diff))/sum((validationLMH$Rs_Norm - mean(validationLMH$Rs_Norm))^2))
e

mre <- sum(validationLMH$Prediction-validationLMH$Rs_Norm)/mean(validationLMH$Rs_Norm)/n
mre

##########Order of importance#############
otuEE <- read.delim('E.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otuEE$All <- factor(otuEE$All, levels = unique(otuEE$All))
otuEE$L <- factor(otuEE$L, levels = unique(otuEE$L))
otuEE$M <- factor(otuEE$M, levels = unique(otuEE$M))
otuEE$H <- factor(otuEE$H, levels = unique(otuEE$H))

#预指定颜色
color_circRNA <- '#8DD3C7'
color_miRNA <- c('#A65628', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
                 '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8',
                 '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33')
color_mRNA <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#66C2A5',
                '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_otu <- c('#FD8072','#FDB462','#FF9F22','#FF7F00','#cc6600','#996633', '#999933', '#669900', '#4DAF4A', '#339900','#009933','#339955','#006400','#80B1D3','#377EB8','#386CB0','#6181BD','#BEBAE0','#BC80BD','#984EA3','#c7475b','#E41A1C','#F0027F','#F781BF','#FCCDE5')
#冲击图
otuEE$link <- 1
otuEE <- reshape::melt(otuEE, id = 'link')

variable <- summary(otuEE$variable)
otuEE$flow <- rep(1:variable[1], length(variable))

link <- 1 / summary(otuEE$value)
for (i in names(link)) otuEE[which(otuEE$value == i),'link'] <- link[i]

library(ggalluvial)
#冲击图
p <- ggplot(otuEE, aes(x = variable, y = link,
                       stratum = value, alluvium = flow, fill = value)) +
  geom_stratum() +  #类似堆叠柱形图
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #在各个区块中添加文字标签
  geom_flow(aes.flow = 'backward') +  #这次，连线颜色就不用 miRNA 赋值了，将 mRNA 也加进来，看一下效果
  scale_fill_manual(values = color_otu) +  #颜色赋值
  scale_x_discrete(limits = c('All', 'L', 'M', 'H')) +  #定义簇（列）的展示顺序
  #scale_y_continuous(expand = c(0, 0))+
  labs(x = " ", y = '') +  #从这儿开始是一些主题参数等的设置
  theme(legend.position = 'none', panel.background = element_blank(),
        line = element_blank(), axis.text.x = element_text(colour = "blue"),axis.text.y=element_blank())

p