#########climatemodel#############

set.seed (556)

modA <- randomForest(Rs_Norm ~ P_LastMonth   +  Measure_Month + #TS + 
                       Pm + Tm + Latitude +absLog + Climate_Koeppon 
                     + T_LastMonth
                     + LAI  + Elevation+ IGBP , data=TrainSetA, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modA
#Variable impor1tance measures
importance (modA)
#Looking at the OOB error reduction with the tree size
plot (modA) 
#Plotting the variable importance
varImpPlot(modA)


prediction.vaA <- predict(modA, ValidSetA) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaA, ValidSetA$Rs_Norm, method = "pearson", alpha=.05)

VaA <- cbind(prediction.vaA, ValidSetA)
names(VaA)[1] <- "Prediction"

valA <- lm(Rs_Norm~Prediction, data=VaA)
summary(valA)

set.seed (556)

modB <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                       Pm + Tm + Latitude +absLog + Climate_Koeppon 
                     + T_LastMonth
                     + LAI + IGBP + Elevation , data=TrainSetB, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modB
#Variable impor1tance measures
importance (modB)
#Looking at the OOB error reduction with the tree size
plot (modB) 
#Plotting the variable importance
varImpPlot(modB)


prediction.vaB <- predict(modB, ValidSetB) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaB, ValidSetB$Rs_Norm, method = "pearson", alpha=.05)

VaB <- cbind(prediction.vaB, ValidSetB)
names(VaB)[1] <- "Prediction"

valB <- lm(Rs_Norm~Prediction, data=VaB)
summary(valB)

set.seed (556)

modC <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + + IGBP
                       Pm + Tm + Latitude +absLog + Climate_Koeppon 
                     + T_LastMonth
                     + LAI  + Elevation+ IGBP  , data=TrainSetC, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modC
#Variable impor1tance measures
importance (modC)
#Looking at the OOB error reduction with the tree size
plot (modC) 
#Plotting the variable importance
varImpPlot(modC)


prediction.vaC <- predict(modC, ValidSetC) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaC, ValidSetC$Rs_Norm, method = "pearson", alpha=.05)

VaC <- cbind(prediction.vaC, ValidSetC)
names(VaC)[1] <- "Prediction"

valC <- lm(Rs_Norm~Prediction, data=VaC)
summary(valC)

set.seed (556)

modD <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + + IGBP
                       Pm + Tm + Latitude +absLog + Climate_Koeppon 
                     + T_LastMonth
                     + LAI  + Elevation+ IGBP  , data=TrainSetD, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modD
#Variable impor1tance measures
importance (modD)
#Looking at the OOB error reduction with the tree size
plot (modD) 
#Plotting the variable importance
varImpPlot(modD)


prediction.vaD <- predict(modD, ValidSetD) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaD, ValidSetD$Rs_Norm, method = "pearson", alpha=.05)

VaD <- cbind(prediction.vaD, ValidSetD)
names(VaD)[1] <- "Prediction"

valD <- lm(Rs_Norm~Prediction, data=VaD)
summary(valD)

set.seed (556)

modE <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + + IGBP
                       Pm + Tm + Latitude +absLog + Climate_Koeppon 
                     + T_LastMonth
                     + LAI + Elevation+ IGBP , data=TrainSetE, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modE
#Variable impor1tance measures
importance (modE)
#Looking at the OOB error reduction with the tree size
plot (modE) 
#Plotting the variable importance
varImpPlot(modE)


prediction.vaE <- predict(modE, ValidSetE) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaE, ValidSetE$Rs_Norm, method = "pearson", alpha=.05)

VaE <- cbind(prediction.vaE, ValidSetE)
names(VaE)[1] <- "Prediction"

valE <- lm(Rs_Norm~Prediction, data=VaE)
summary(valE)

#CK<-TrainSet$Climate_Koeppon
#head(CK)
#K<-globalData$IGBP
#head(K)

mean(VaA$Prediction,na.rm = T)
mean(VaB$Prediction,na.rm = T)
mean(VaC$Prediction,na.rm = T)
mean(VaD$Prediction,na.rm = T)
mean(VaE$Prediction,na.rm = T)

vaABC <- rbind(VaA, VaB,VaC,VaD,VaE)
names(vaABC)[1] <- "Prediction"

valABC <- lm(Rs_Norm~Prediction, data=vaABC)
summary(valABC)

cor.test(vaABC$Prediction, vaABC$Rs_Norm, method = "spearman", alpha=.05)

mean(vaABC$Prediction,na.rm = T)

rmse <- sqrt(mean((vaABC$Prediction - vaABC$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(vaABC$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- vaABC$Prediction - vaABC$Rs_Norm
squared_diff <- diff^2

e <- 1-((sum(squared_diff))/sum((vaABC$Rs_Norm - mean(vaABC$Rs_Norm))^2))
e
# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

AIC(val, valABC)


A <- subset(globalData, TopClimate == "A" )
B <- subset(globalData, TopClimate == "B" )
C <- subset(globalData, TopClimate == "C" )
D <- subset(globalData, TopClimate == "D" )
E <- subset(globalData, TopClimate == "E" )

#globalData <- globalData[complete.cases(globalData),]
globalData$prediction <- predict(mod1, globalData)

mean(globalData$prediction,na.rm = T)

A$prediction <- predict(modA, A)
B$prediction <- predict(modB, B)
C$prediction <- predict(modC, C)
D$prediction <- predict(modD, D)
E$prediction <- predict(modE, E)

mean(A$prediction,na.rm = T)
mean(B$prediction,na.rm = T)
mean(C$prediction,na.rm = T)
mean(D$prediction,na.rm = T)
mean(E$prediction,na.rm = T)

globalDataABC <-rbind(A,B,C,D,E)

mean(globalDataABC$prediction,na.rm = T)



mean(globalDataABC$prediction,na.rm = T)
#mean(globalDataABC$prediction1,na.rm = T)


globalData<-data.frame(globalData)
globalData<-globalData[order(globalData$...1),]


globalDataABC<-data.frame(globalDataABC)
globalDataABC<-globalDataABC[order(globalDataABC$...1),]


#mean(globalData$prediction,na.rm = T)
#globalData <- globalData[complete.cases(globalData),]

G77 <- subset(globalDataABC, Measure_Month==7 )
theme_update(plot.title = element_text(hjust = 0.5))

# 设置色带
#cols <- c("#FBF8EB", "#F2E49B", "#CAE784", "#1D934D", "#CE6BBB")
# 在计算分位数之前排除包含缺失值的观测
G77_no_na <- G77[!is.na(G77 $prediction), ]
# 定义分位数值，可以根据需要进行调整
#quantiles <- quantile(G7_no_na$prediction, probs = c(0, 0.25, 0.5, 0.75, 1))
# 绘图

ggplot(G77_no_na, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )


GABC1 <- subset(globalDataABC, Measure_Month==1 )
GABC2 <- subset(globalDataABC, Measure_Month==2 )
GABC3 <- subset(globalDataABC, Measure_Month==3 )
GABC4 <- subset(globalDataABC, Measure_Month==4 )
GABC5 <- subset(globalDataABC, Measure_Month==5 )
GABC6 <- subset(globalDataABC, Measure_Month==6 )
GABC7 <- subset(globalDataABC, Measure_Month==7 )
GABC8 <- subset(globalDataABC, Measure_Month==8 )
GABC9 <- subset(globalDataABC, Measure_Month==9 )
GABC10 <- subset(globalDataABC, Measure_Month==10 )
GABC11 <- subset(globalDataABC, Measure_Month==11 )
GABC12 <- subset(globalDataABC, Measure_Month==12 )


GABC1$Predicted_Rs <-(GABC1$prediction+GABC2$prediction+GABC3$prediction+GABC4$prediction+GABC5$prediction+GABC6$prediction+GABC7$prediction+GABC8$prediction+GABC9$prediction+GABC10$prediction+GABC11$prediction)/11#+G12$prediction

GABC1$predictions <-(GABC1$prediction+GABC2$prediction+GABC3$prediction+GABC4$prediction+GABC5$prediction+GABC6$prediction+GABC7$prediction+GABC8$prediction+GABC9$prediction+GABC10$prediction+GABC11$prediction)/11#+G12$prediction

W<-expression(paste("Predicted Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")"))
GABC1_no_na <- GABC1[!is.na(GABC1 $predictions), ]
write.csv(GABC1_no_na,"GABC1_no_na.csv")
ggplot(GABC1_no_na, aes(Longitude, Latitude, color = predictions)) +
  scale_colour_steps(low = "white", high = "#006400", n.breaks=8)+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "Predicted Rs") +
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    legend.position = c(0.11,0.3)
  )






ggplot(GABC1_no_na, aes(Longitude, Latitude, color = predictions)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )

my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
GABCR <- ggplot() + 
  geom_sf() + 
  geom_tile(data = GABC1_no_na,aes(Longitude, Latitude, fill = Predicted_Rs))+
  scale_fill_gradientn(colours = my_colormap,breaks=c(0.5,1.5,2.5,3.5,4.5,5.5))+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
GABCR<-GABCR+ annotate("text", x = -150 , y = -10,label= ("Climate model"),size = 3)+ annotate("text", x = -150 , y = -19,label= ("Global Mean Rs"),size = 3)+ annotate("text", x = -150 , y = -28,label= expression(paste("2.00"," ",g," ","C"," ", m^-2," ", "day"^-1)),size = 3)
GABCR

GABC1$Uncertainty <- (GABC1$Predicted_Rs-G1$Predicted_Rs)/G1$Predicted_Rs
GABC1_no_na <- GABC1[!is.na(GABC1$Uncertainty), ]

mean(GABC1$Uncertainty,na.rm = T)
my_colormap <- colorRampPalette(rev(brewer.pal(11,'BrBG')))(32)
GABCU <- ggplot() + 
  geom_sf() + 
  geom_tile(data =GABC1_no_na,aes(Longitude, Latitude, fill = Uncertainty))+
  scale_fill_gradientn(colours = my_colormap,n.breaks=7,limits=c(-1,1))+labs( "Predicted Rs")+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  ) 
#GABCU<-GABCU+ annotate("text", x = -120 , y = -20,label= expression(paste("Global mean Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")))

GABCU <-GABCU+ annotate("text", x = -150 , y = -10,label= ("Single global model"),size = 3)+ annotate("text", x = -150 , y = -18,label= ("vs."),size = 3)+ annotate("text", x = -150 , y = -26,label= ("Climate model"),size = 3)

GABCU