############yearmodel############


set.seed (556)

modYH <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + + IGBP
                        Pm + Tm + Latitude +absLog + Climate_Koeppon + T_LastMonth
                      + LAI  + Elevation+ IGBP , data=TrainSetYH, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modYH
#Variable impor1tance measures
importance (modYH)
#Looking at the OOB error reduction with the tree size
plot (modYH) 
#Plotting the variable importance
varImpPlot(modYH)


prediction.vaYH <- predict(modYH, ValidSetYH) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaYH, ValidSetYH$Rs_Norm, method = "pearson", alpha=.05)

VaYH <- cbind(prediction.vaYH, ValidSetYH)
names(VaYH)[1] <- "Prediction"

valYH <- lm(Rs_Norm~Prediction, data=VaYH)
summary(valYH)

set.seed (556)

modYQ <- randomForest(Rs_Norm ~  Measure_Month  #+TS  + IGBP
                      +P_LastMonth   +Pm + Tm + Latitude +absLog + Climate_Koeppon + T_LastMonth
                      + LAI  + Elevation+ IGBP , data=TrainSetYQ, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modYQ
#Variable impor1tance measures
importance (modYQ)
#Looking at the OOB error reduction with the tree size
plot (modYQ) 
#Plotting the variable importance
varImpPlot(modYQ)


prediction.vaYQ <- predict(modYQ, ValidSetYQ) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaYQ, ValidSetYQ$Rs_Norm, method = "pearson", alpha=.05)

VaYQ <- cbind(prediction.vaYQ, ValidSetYQ)
names(VaYQ)[1] <- "Prediction"

valYQ <- lm(Rs_Norm~Prediction, data=VaYQ)
summary(valYQ)




vaYQH <- rbind(VaYQ, VaYH)
names(vaYQH)[1] <- "Prediction"

valYQH <- lm(Rs_Norm~Prediction, data=vaYQH)
summary(valYQH)

cor.test(vaYQH$Prediction, vaYQH$Rs_Norm, method = "spearman", alpha=.05)

mean(vaYQH$Prediction,na.rm = T)

rmse <- sqrt(mean((vaYQH$Prediction - vaYQH$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(vaYQH$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- vaYQH$Prediction - vaYQH$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

e <- 1-((sum(squared_diff))/sum((vaYQH$Rs_Norm - mean(vaYQH$Rs_Norm))^2))
e
# 打印结果
print(mse)

AIC(val, valYQH)



#A <- subset(globalData, TopClimate == "A" )
#B <- subset(globalData, TopClimate == "B" )
#C <- subset(globalData, TopClimate == "C" )
#D <- subset(globalData, TopClimate == "D" )
#E <- subset(globalData, TopClimate == "E" )

#globalData <- globalData[complete.cases(globalData),]
globalData$prediction <- predict(mod1, globalData)

mean(globalData$prediction,na.rm = T)

globalData$predictionYH <- predict(modYH, globalData)

mean(globalData$predictionYH,na.rm = T)

globalData$predictionYQ <- predict(modYQ, globalData)

mean(globalData$predictionYQ,na.rm = T)





#mean(globalData$prediction,na.rm = T)
#globalData <- globalData[complete.cases(globalData),]

GY7 <- subset(globalData, Measure_Month==7 )
theme_update(plot.title = element_text(hjust = 0.5))

# 设置色带
#cols <- c("#FBF8EB", "#F2E49B", "#CAE784", "#1D934D", "#CE6BBB")
# 在计算分位数之前排除包含缺失值的观测
GY7_no_na <- GY7[!is.na(GY7 $predictionYQ), ]
# 定义分位数值，可以根据需要进行调整
#quantiles <- quantile(G7_no_na$prediction, probs = c(0, 0.25, 0.5, 0.75, 1))
# 绘图

ggplot(GY7_no_na, aes(Longitude, Latitude, color = predictionYQ)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )

G1 <- subset(globalData, Measure_Month==1 )
G2 <- subset(globalData, Measure_Month==2 )
G3 <- subset(globalData, Measure_Month==3 )
G4 <- subset(globalData, Measure_Month==4 )
G5 <- subset(globalData, Measure_Month==5 )
G6 <- subset(globalData, Measure_Month==6 )
G7 <- subset(globalData, Measure_Month==7 )
G8 <- subset(globalData, Measure_Month==8 )
G9 <- subset(globalData, Measure_Month==9 )
G10 <- subset(globalData, Measure_Month==10 )
G11 <- subset(globalData, Measure_Month==11 )
G12 <- subset(globalData, Measure_Month==12 )

G1$predictionYH <-(G1$predictionYH+G2$predictionYH+G3$predictionYH+G4$predictionYH+G5$predictionYH+G6$predictionYH+G7$predictionYH+G8$predictionYH+G9$predictionYH+G10$predictionYH+G11$predictionYH)/11#+G12$prediction


GG1<-G1
GG1$Predicted_Rs<-GG1$predictionYH

G1_no_na <- G1[!is.na(G1 $predictionYH), ]
write.csv(G1_no_na,"G1_no_naY.csv")

GG1_no_na <- GG1[!is.na(GG1 $Predicted_Rs), ]

GC1<-subset(G1_no_na, G1_no_na$predictionYH<=0.5)
GC2<-subset(G1_no_na, G1_no_na$predictionYH>0.5 & G1_no_na$predictionYH<=1)
GC3<-subset(G1_no_na, G1_no_na$predictionYH>1 & G1_no_na$predictionYH<=1.4)
GC4<-subset(G1_no_na, G1_no_na$predictionYH>1.4 & G1_no_na$predictionYH <=1.8)
GC5<-subset(G1_no_na, G1_no_na$predictionYH>1.8 & G1_no_na$predictionYH <=2.2)
GC6<-subset(G1_no_na, G1_no_na$predictionYH>2.2 & G1_no_na$predictionYH <=2.6)
GC7<-subset(G1_no_na, G1_no_na$predictionYH>2.6)

GC<-c(GC1,GC2,GC3,GC4,GC5,GC6,GC7)
ggplot(G1_no_na, aes(Longitude, Latitude, color = predictionYH)) +
  scale_colour_steps(low = "white", high = "#006400",n.breaks=7 )+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "Predicted Rs") +
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    legend.position = c(0.11,0.3)
  )

ggplot(G1_no_na, aes(Longitude, Latitude, color = predictionYH)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )


AIC(val, valYQH,valIABC,valABC,valLMH)

my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
GYHR <- ggplot() + 
  geom_sf() + 
  geom_tile(data = GG1_no_na,aes(Longitude, Latitude, fill = Predicted_Rs))+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
GYHR<-GYHR+ annotate("text", x = -150 , y = -10,label= ("Year model"),size = 3)+ annotate("text", x = -150 , y = -19,label= ("Global Mean Rs"),size = 3)+ annotate("text", x = -150 , y = -28,label= expression(paste("1.95"," ",g," ","C"," ", m^-2," ", "day"^-1)),size = 3)

GYHR

GG1$Uncertainty <- (GG1$Predicted_Rs-GG1$prediction)/GG1$prediction
GG1_no_na <- GG1[!is.na(GG1$Uncertainty), ]

mean(GG1$Uncertainty,na.rm = T)

my_colormap <- colorRampPalette(rev(brewer.pal(9,'BrBG')))(32)
GYHU <- ggplot() + 
  geom_sf() + 
  geom_tile(data =GG1_no_na,aes(Longitude, Latitude, fill = Uncertainty))+
  scale_fill_gradientn(colours = my_colormap,n.breaks=7,limits=c(-1.5,1.5))+labs( "Predicted Rs")+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  ) 
#GABCU<-GABCU+ annotate("text", x = -120 , y = -20,label= expression(paste("Global mean Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")))

GYHU <- GYHU+ annotate("text", x = -150 , y = -10,label= ("Single global model"),size = 3)+ annotate("text", x = -150 , y = -18,label= ("vs."),size = 3)+ annotate("text", x = -150 , y = -26,label= ("Year model"),size = 3)

GYHU

library(patchwork)
GABCU/GYHU|GIABCU/GLMHU