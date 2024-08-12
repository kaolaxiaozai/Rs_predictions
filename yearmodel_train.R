############Year model#############


set.seed (556)

fit_rfYQ <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetYQ, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfYQ
#Variable importance measures
importance (fit_rfYQ)
#Looking at the OOB error reduction with the tree size
plot (fit_rfYQ) 
#Plotting the variable importance
varImpPlot(fit_rfYQ)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuYQ <- data.frame(importance(fit_rfYQ))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuYQ <- importance_otuYQ[order(importance(fit_rfYQ,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuYQ, 'importance_otuYQ.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationYQQ <- predict(fit_rfYQ, ValidSetYQ) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationYQQ, ValidSetYQ$Rs_Norm, method = "pearson", alpha=.05)

ValidationYQQ <- cbind(prediction.validationYQQ, ValidSetYQ)
names(ValidationYQQ)[1] <- "Prediction"

validation.lmYQQ <- lm(Rs_Norm~Prediction, data=ValidationYQQ)
summary(validation.lmYQQ)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationYQ <- predict(fit_rf, ValidSetYQ) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationYQ, ValidSetYQ$Rs_Norm, method = "pearson", alpha=.05)

ValidationYQ <- cbind(prediction.validationYQ, ValidSetYQ)
names(ValidationYQ)[1] <- "Prediction"

validation.lmYQ <- lm(Rs_Norm~Prediction, data=ValidationYQ)
summary(validation.lmYQ)

AIC(validation.lmYQQ, validation.lmYQ)

#使用All数据集，评估预测性能
RS_predictTYQ <- predict(fit_rf,ValidSetYQ)

plot(ValidSetYQ$Rs_Norm, RS_predictTYQ,main = 'AD测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetYQ$Rs_Norm,RS_predictTYQ)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTYQ <- sqrt(mean((ValidSetYQ$Rs_Norm - RS_predictTYQ )^2))
rmseTYQ
# 计算观测值数量
n <- length(RS_predictTYQ)

# 计算每个观测值的差异，并求平方
diff <- ValidSetYQ$Rs_Norm - RS_predictTYQ
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVYQ <- predict(fit_rfYQ, ValidSetYQ)

plot(ValidSetYQ$Rs_Norm, RS_predictVYQ, main = 'D测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetYQ$Rs_Norm,RS_predictVYQ)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseVYQ <- sqrt(mean((ValidSetYQ$Rs_Norm - RS_predictVYQ )^2))
rmseVYQ
# 计算观测值数量
n <- length(RS_predictVYQ)

# 计算每个观测值的差异，并求平方
diff <- ValidSetYQ$Rs_Norm - RS_predictVYQ
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)
##########################

########E数据的测试######

set.seed (556)

fit_rfYH <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                           Pm + Tm + Latitude + absLog + Climate_Koeppon 
                         + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay + Sand + 
                           Elevation + Silt + N + OC + PH, data=TrainSetYH, 
                         keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
fit_rfYH
#Variable importance measures
importance (fit_rfYH)
#Looking at the OOB error reduction with the tree size
plot (fit_rfYH) 
#Plotting the variable importance
varImpPlot(fit_rfYH)

##关键 OTUs 识别
#查看表示每个变量（OTUs）重要性的得分

#或者使用函数 importance()
importance_otuYH <- data.frame(importance(fit_rfYH))
#head(importance_otu)

#可以根据某种重要性的高低排个序，例如根据“Mean Decrease Accuracy”指标
importance_otuYH <- importance_otuYH[order(importance(fit_rfYH,type=1), decreasing = TRUE), ]
#head(importance_otu)

#输出表格
write.csv(importance_otuYH, 'importance_otuYH.csv')

#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
#######
prediction.validationYHH <- predict(fit_rfYH, ValidSetYH) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationYHH, ValidSetYH$Rs_Norm, method = "pearson", alpha=.05)

ValidationYHH <- cbind(prediction.validationYHH, ValidSetYH)
names(ValidationYHH)[1] <- "Prediction"

validation.lmYHH <- lm(Rs_Norm~Prediction, data=ValidationYHH)
summary(validation.lmYHH)

#########Step 3: All Factor Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validationYH <- predict(fit_rf, ValidSetYH) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validationYH, ValidSetYH$Rs_Norm, method = "pearson", alpha=.05)

ValidationYH <- cbind(prediction.validationYH, ValidSetYH)
names(ValidationYH)[1] <- "Prediction"

validation.lmYH <- lm(Rs_Norm~Prediction, data=ValidationYH)
summary(validation.lmYH)

AIC(validation.lmYHH, validation.lmYH)

#使用All数据集，评估预测性能
RS_predictTYH <- predict(fit_rf,ValidSetYH)

plot(ValidSetYH$Rs_Norm, RS_predictTYH,main = 'AE测试集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetYH$Rs_Norm,RS_predictTYH)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseTYH <- sqrt(mean((ValidSetYH$Rs_Norm - RS_predictTYH )^2))
rmseTYH
# 计算观测值数量
n <- length(RS_predictTYH)

# 计算每个观测值的差异，并求平方
diff <- ValidSetYH$Rs_Norm - RS_predictTYH
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
RS_predictVYH <- predict(fit_rfYH, ValidSetYH)

plot(ValidSetYH$Rs_Norm, RS_predictVYH, main = 'E测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSetYH$Rs_Norm,RS_predictVYH)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, expression(R^2==0.678),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

rmseVYH <- sqrt(mean((ValidSetYH$Rs_Norm - RS_predictVYH )^2))
rmseVYH
# 计算观测值数量
n <- length(RS_predictVYH)

# 计算每个观测值的差异，并求平方
diff <- ValidSetYH$Rs_Norm - RS_predictVYH
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

validationY <- rbind(ValidationYHH, ValidationYQQ)
names(validationY)[1] <- "Prediction"

validation.lmY <- lm(Rs_Norm~Prediction, data=validationY)
summary(validation.lmY)

cor.test(validationY$Prediction, validationY$Rs_Norm, method = "spearman", alpha=.05)

plot(validationY$Rs_Norm,validationY$Prediction, main = 'Year model',
     xlab = 'Observated data', ylab = 'Predicted data', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(validationY$Rs_Norm,validationY$Prediction)
abline(coef(z),lwd=2, lty = 2,xaxs="s",col = "red")
text(2, 10, expression(R^2==0.678),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

mean(validationY$Prediction,na.rm = T)

rmse <- sqrt(mean((validationY$Prediction - validationY$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(validationY$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- validationY$Prediction - validationY$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)


AIC(validation.lm, validation.lmY)

mean(RS_predictTYQ,na.rm = T)
mean(RS_predictVYQ,na.rm = T)

mean(RS_predictTYH,na.rm = T)
mean(RS_predictVYH,na.rm = T)

e <- 1-((sum(squared_diff))/sum((validationY$Rs_Norm - mean(validationY$Rs_Norm))^2))
e

mre <- sum(validationY$Prediction-validationY$Rs_Norm)/mean(validationY$Rs_Norm)/n
mre

#############Order of importance################


otuY <- read.delim('YM.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
otuY$All <- factor(otuY$All, levels = unique(otuY$All))
otuY$YH <- factor(otuY$YH, levels = unique(otuY$YH))
otuY$YQ <- factor(otuY$YQ, levels = unique(otuY$YQ))


#预指定颜色
color_circRNA <- '#8DD3C7'
color_miRNA <- c('#A65628', '#FFFFB3', '#BEBADA', '#FB8072', '#80B1D3', '#FDB462',
                 '#B3DE69', '#FCCDE5', '#BC80BD', '#CCEBC5', '#FFED6F', '#E41A1C', '#377EB8',
                 '#4DAF4A', '#984EA3', '#FF7F00', '#FFFF33')
color_mRNA <- c('#6181BD', '#F34800', '#64A10E', '#FF00FF', '#c7475b', '#66C2A5',
                '#BEAED4', '#FDC086', '#FFFF99', '#386CB0', '#F0027F')
color_otu <- c('#FD8072','#FDB462','#FF9F22','#FF7F00','#cc6600','#996633', '#999933', '#669900', '#4DAF4A', '#339900','#009933','#339955','#006400','#80B1D3','#377EB8','#386CB0','#6181BD','#BEBAE0','#BC80BD','#984EA3','#c7475b','#E41A1C','#F0027F','#F781BF','#FCCDE5')
#冲击图
otuY$link <- 1
otuY <- reshape::melt(otuY, id = 'link')

variable <- summary(otuY$variable)
otuY$flow <- rep(1:variable[1], length(variable))

link <- 1 / summary(otuY$value)
for (i in names(link)) otu[which(otuY$value == i),'link'] <- link[i]

library(ggalluvial)
#冲击图
p <- ggplot(otuY, aes(x = variable, y = link,
                      stratum = value, alluvium = flow, fill = value)) +
  geom_stratum() +  #类似堆叠柱形图
  geom_text(stat = 'stratum', infer.label = TRUE, size = 2.5) +  #在各个区块中添加文字标签
  geom_flow(aes.flow = 'backward') +  #这次，连线颜色就不用 miRNA 赋值了，将 mRNA 也加进来，看一下效果
  scale_fill_manual(values = color_otu) +  #颜色赋值
  scale_x_discrete(limits = c('All', 'YQ', 'YH')) +  #定义簇（列）的展示顺序
  #scale_y_continuous(expand = c(0, 0))+
  labs(x = " ", y = '') +  #从这儿开始是一些主题参数等的设置
  theme(legend.position = 'none', panel.background = element_blank(),
        line = element_blank(), axis.text.x = element_text(colour = "blue"),axis.text.y=element_blank())

p