##########Bootstrapping 200 times to validate the model############

library(randomForest)

# 设置随机种子以确保结果的可重复性
set.seed(123456789)

# 定义一个函数，用于训练随机森林模型
train_random_forest <- function(data) {
  # 设置随机森林参数，这里根据你的需求来调整
  rf <- randomForest(Rs_Norm ~  Measure_Month  #+TS  + IGBP
                     +P_LastMonth   +Pm
                     + Tm + Latitude +absLog + Climate_Koeppon + T_LastMonth
                     + LAI  + Elevation+ IGBP, data = data, ntree = 200)
  
  # 返回训练好的模型
  return(rf)
}

# 重复200次引导
num_bootstrap <- 200
modelsL <- list()

for (i in 1:num_bootstrap) {
  # 从原始数据集中进行 Bootstrap 抽样
  boot_data <- sample(nrow(TrainSetL), replace = TRUE)
  boot_sample <- TrainSetL[boot_data, ]
  
  # 训练随机森林模型
  rf_model <- train_random_forest(boot_sample)
  
  # 将训练好的模型存储起来
  modelsL[[i]] <- rf_model
}

# 对模型进行评估
# 这里可以根据需要添加你想要的评估指标和分析
# 例如，你可以计算每个模型的准确率并取平均值和标准差
accuraciesL <- sapply(modelsL, function(model) {
  pred <- predict(model, newdata = ValidSetL)
  #mean(pred == ValidSet$Class)
})

#accuracies <- rbind(ValidSet, accuracies)
write.csv(ValidSetL,"ValidSetL.csv")
write.csv(accuraciesL,"accuraciesL.csv")

mean_accuracyL <- mean(accuraciesL)
sd_accuracyL <- sd(accuraciesL)

# 输出结果
cat("Mean Accuracy:", mean_accuracyL, "\n")
cat("Standard Deviation of Accuracy:", sd_accuracyL, "\n")

modelsM <- list()

for (i in 1:num_bootstrap) {
  # 从原始数据集中进行 Bootstrap 抽样
  boot_data <- sample(nrow(TrainSetM), replace = TRUE)
  boot_sample <- TrainSetM[boot_data, ]
  
  # 训练随机森林模型
  rf_model <- train_random_forest(boot_sample)
  
  # 将训练好的模型存储起来
  modelsM[[i]] <- rf_model
}

# 对模型进行评估
# 这里可以根据需要添加你想要的评估指标和分析
# 例如，你可以计算每个模型的准确率并取平均值和标准差
accuraciesM <- sapply(modelsM, function(model) {
  pred <- predict(model, newdata = ValidSetM)
  #mean(pred == ValidSet$Class)
})

#accuracies <- rbind(ValidSet, accuracies)
write.csv(ValidSetM,"ValidSetM.csv")
write.csv(accuraciesM,"accuraciesM.csv")

modelsH <- list()

for (i in 1:num_bootstrap) {
  # 从原始数据集中进行 Bootstrap 抽样
  boot_data <- sample(nrow(TrainSetH), replace = TRUE)
  boot_sample <- TrainSetH[boot_data, ]
  
  # 训练随机森林模型
  rf_model <- train_random_forest(boot_sample)
  
  # 将训练好的模型存储起来
  modelsH[[i]] <- rf_model
}

# 对模型进行评估
# 这里可以根据需要添加你想要的评估指标和分析
# 例如，你可以计算每个模型的准确率并取平均值和标准差
accuraciesH <- sapply(modelsH, function(model) {
  pred <- predict(model, newdata = ValidSetH)
  #mean(pred == ValidSet$Class)
})

#accuracies <- rbind(ValidSet, accuracies)
write.csv(ValidSetH,"ValidSetH.csv")
write.csv(accuraciesH,"accuraciesH.csv")

RMSEI <- list()
library(mlr3measures)
xL<-vector(mode="numeric",length=0)
yL<-vector(mode="numeric",length=0)
Rs_DATAL<- read_csv("E:/R/LMH.csv",locale=locale(encoding="GBK"))
Rs_DATAL<-as.data.frame(Rs_DATAL) 
#class(Rs_DATAI)
for (j in 1:num_bootstrap) {
  VL <- Rs_DATAL[,j+2]
  rmseL <- sqrt(mean((Rs_DATAL$Rs_Norm - VL )^2))
  xL[j] <- rmseL
  #R2I <- sum((Rs_DATAI$Rs_Norm - VI )^2) / sum((Rs_DATAI$Rs_Norm - mean(Rs_DATAI$Rs_Norm))^2)
  R2L<- rsq( Rs_DATAL$Rs_Norm, VL)
  yL[j] <-  R2L
}  
xL
yL
XX<-cbind(x,xA,xI,xY,xL)
yy<-cbind(y,yA,yI,yY,yL)
write.csv(XX,"XX.csv")
write.csv(yy,"yy.csv")


############a leave-one-month-out  cross-validation method###############

library(caret)  # 载入caret包

# 假设您的数据框是df，包含日期列和其他特征列
df<- TrainSetH # 提取月份信息
df$month<-df$Measure_Month
months <- unique(df$month)

resultsH <- list()

for (m in months) {
  train_data <- df[df$month != m, ]
  test_data  <- df[df$month == m, ]
  
  # 训练模型
  # fit <- train(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
  #                       Pm + Tm +Latitude  +absLog #+absLat#
  #                       #+Longitude 
  #                       + Climate_Koeppon  + IGBP + T_LastMonth
  #                       + LAI  + Elevation , data = train_data, method = "randomForest")
  
  fit <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm +Latitude  +absLog #+absLat#
                      #+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation , data=train_data, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=200)
  # 预测
  predictions <- predict(fit, test_data)
  
  # 存储结果
  resultsH[[m]] <- postResample(predictions, test_data$Rs_Norm)
}

# 查看结果
resultsH
write.csv(resultsH,"resultsH.csv")


################a leave-one-site-out cross-validation method#######################
df<- RsForRandomForest_naOMIT 
df$site <- df$studynumber
sites <- unique(df$site)

resultss <- list()

for (s in sites) {
  train_data <- df[df$site != s, ]
  test_data  <- df[df$site == s, ]
  
  # 训练模型
  fit <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm +Latitude  +absLog #+absLat#
                      #+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation , data=train_data, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=200)
  
  # 预测
  predictions <- predict(fit, test_data)
  
  # 存储结果
  resultss[[s]] <- postResample(predictions, test_data$Rs_Norm)
}

# 查看结果
resultss
#resultsss <- na.omit(resultss)
write.csv(resultsss,"resultsss.csv")


remove_empty_list_elements <- function(lst) {
  lst <- lst[sapply(lst, function(x) !is.null(x) && length(x) > 0)]
  return(lst)
}

# 示例列表
example_list <- resultss

# 删除空行
cleaned_list <- remove_empty_list_elements(example_list)
write.csv(cleaned_list,"cleaned_lists.csv")