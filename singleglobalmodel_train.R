###Set seed to replicate random generation

set.seed (556)
#set.seed (12346)
#Y <- TrainSet[,2]
#X <-TrainSet[,c(4,6,7,16,17,18,20,23,24,25,26,27,28,29,30:36,39)]
#X2<-TrainSet[,7]
#X1 <- data.frame(X2)
#Y1 <- data.frame(Y)

#fit_rf11 <- quantregForest(x=X1,y=Y,keep.inbag=TRUE)


fit_rf1 <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + TS + 
                          Pm + Tm + Latitude #+Longitude# 
                        +absLog 
                        + Climate_Koeppon + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay                         + Sand + Elevation + Silt + N + OC + PH, data=TrainSet, 
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

Validation1 <- cbind(prediction1.validation, ValidSet)
names(Validation1)[1] <- "Prediction"

validation1.lm <- lm(Rs_Norm~Prediction, data=Validation1)
summary(validation1.lm)


#使用训练集，评估预测性能
plant_predict1 <- predict(fit_rf1,TrainSet)

plot(TrainSet$Rs_Norm, plant_predict1, main = '训练集',
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(TrainSet$Rs_Norm,plant_predict1)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.83,p<2.2e-16"),cex=1,font=2,col="#000000")

# 计算均方根误差（RMSE）
rmseT1 <- sqrt(mean((TrainSet$Rs_Norm - plant_predict1 )^2))
rmseT1

# 计算观测值数量
n <- length(plant_predict1)

# 计算每个观测值的差异，并求平方
diff <- TrainSet$Rs_Norm - plant_predict1
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
plant_predict1 <- predict(fit_rf1, ValidSet)

plot(ValidSet$Rs_Norm, plant_predict1, main = '测试集',
     xlab = 'Data of validating', ylab = 'Predict', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSet$Rs_Norm,plant_predict1)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.83,p<2.2e-16"),cex=1,font=2,col="#000000")

rmseV1 <- sqrt(mean(( ValidSet$Rs_Norm - plant_predict1 )^2))
rmseV1

# 计算观测值数量
n <- length(plant_predict1)

# 计算每个观测值的差异，并求平方
diff <- ValidSet$Rs_Norm - plant_predict1
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

###ALL全球

set.seed (556)

fit_rf <- randomForest(Rs_Norm ~ P_LastMonth +  T_LastMonth + Measure_Month + #TS + 
                         Pm + Tm + Latitude #+Longitude#
                       + absLog 
                       + Climate_Koeppon + IGBP + LAI + BD + BS + CN + CaCO3 + CEC + Clay                         + Sand + Elevation + Silt + N + OC + PH, data=TrainSet, 
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

#然后取出排名靠前的因素

#提示保留 9-13 个重要的 OTU，可以使随机森林回归的精度最大化
#首先根据某种重要性的高低排个序，例如根据“IncNodePurity”指标
importance_otu <- importance_otu[order(importance(fit_rf,type=1), decreasing = TRUE), ]

#然后取出排名靠前的 OTU，例如 top10 最重要的 OTU
importance_otu.select <- importance_otu[1:10, ]
importance_otu.select

#输出表格
#write.table(importance_otu.select, 'importance_otu.select.txt', sep = '\t', col.names = NA, quote = FALSE)

#有关 OTU 的物种分类信息等其它细节，可后续自行补充
otu_id.select <- rownames(importance_otu.select)
otu.select <- TrainSet[ ,c(otu_id.select, 'Rs_Norm')]
otu.select <- reshape2::melt(otu.select, id = 'Rs_Norm')

ggplot(otu.select, aes(x = value, y = Rs_Norm)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~variable, ncol = 3, scale = 'free_y') +
  labs(title = '',x = 'Plant age (days)', y = 'Relative abundance')




# 查看下这些重要的 vars 与Ozone的关系
ggplot() +
  geom_point() +
  geom_smooth(data= TrainSet, aes(x =  Elevation, y = Rs_Norm),method = "loess") +
  #facet_wrap(~variable, ncol = 2, scale = 'free_y') +
  labs(title = '',x = ' Elevation', y = 'Rs_Norm')

ggplot() +
  geom_point() +
  geom_smooth(data= TrainSet, aes(x =  P_LastMonth, y = Rs_Norm),method = "loess") +
  #facet_wrap(~variable, ncol = 2, scale = 'free_y') +
  labs(title = '',x = ' P_LastMonth', y = 'Rs_Norm')

ggplot() +
  geom_point(data= TrainSet,aes(x =  Measure_Month, y = Rs_Norm)) +
  geom_smooth() +
  #facet_wrap(~variable, ncol = 2, scale = 'free_y') +
  labs(title = '',x = ' Measure_Month', y = 'Rs_Norm')

ggplot() +
  geom_point() +
  geom_smooth(data= TrainSet, aes(x =  LAI, y = Rs_Norm),method = "loess") +
  #facet_wrap(~variable, ncol = 2, scale = 'free_y') +
  labs(title = '',x = ' LAI', y = 'Rs_Norm')
#########Step 2: Evaluate the model
#Generate predictions of validation data using Random Forest Model
prediction.validation <- predict(fit_rf, ValidSet) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.validation, ValidSet$Rs_Norm, method = "spearman", alpha=.05)

Validation <- cbind(prediction.validation, ValidSet)
names(Validation)[1] <- "Prediction"

validation.lm <- lm(Rs_Norm~Prediction, data=Validation)
summary(validation.lm)

mean(prediction.validation,na.rm = T)
#write.csv(Validation, 'Validation.csv')

#使用训练集，评估预测性能
plant_predict <- predict(fit_rf,TrainSet)

plot(TrainSet$Rs_Norm, plant_predict,
     xlab = 'Data of training', ylab = 'Predict',xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(TrainSet$Rs_Norm,plant_predict)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(2, 10, c("R=0.82,p<2.2e-16"),cex=1,font=2,col="#000000")

mean(plant_predict,na.rm = T)

rmseT <- sqrt(mean((TrainSet$Rs_Norm - plant_predict )^2))
rmseT


# 计算观测值数量
n <- length(plant_predict)

# 计算每个观测值的差异，并求平方
diff <- TrainSet$Rs_Norm - plant_predict
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

#使用测试集，评估预测性能
plant_predict <- predict(fit_rf, ValidSet)

plot(ValidSet$Rs_Norm, plant_predict, main = 'Single Global Model (SGM)',
     xlab = 'Observated data', ylab = 'Predicted data ', xlim=c(0,12), ylim=c(0,12))
abline(0, 1)
z<-line(ValidSet$Rs_Norm,plant_predict)
abline(coef(z),lwd=2, lty = 2,xaxs="s",col = "red")
text(2, 10, expression(R^2==0.671),cex=1,font=2,col="#000000")
text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")

rmseV <- sqrt(mean((ValidSet$Rs_Norm - plant_predict )^2))
rmseV
# 计算观测值数量
n <- length(plant_predict)

# 计算每个观测值的差异，并求平方
diff <- ValidSet$Rs_Norm - plant_predict
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

mean(plant_predict,na.rm = T)

e <- 1-((sum(squared_diff))/sum((ValidSet$Rs_Norm - mean(ValidSet$Rs_Norm))^2))
e

mre <- sum(plant_predict-ValidSet$Rs_Norm)/mean(ValidSet$Rs_Norm)/n
mre