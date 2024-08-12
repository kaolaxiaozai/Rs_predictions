#############elevationmodel#############


set.seed (556)

modL <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + + IGBP
                       Pm + Tm + Latitude +absLog + Climate_Koeppon + T_LastMonth
                     + LAI  + Elevation+ IGBP , data=TrainSetL, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modL
#Variable impor1tance measures
importance (modL)
#Looking at the OOB error reduction with the tree size
plot (modL) 
#Plotting the variable importance
varImpPlot(modL)


prediction.vaL <- predict(modL, ValidSetL) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaL, ValidSetL$Rs_Norm, method = "pearson", alpha=.05)

VaL <- cbind(prediction.vaL, ValidSetL)
names(VaL)[1] <- "Prediction"

valL <- lm(Rs_Norm~Prediction, data=VaL)
summary(valL)

set.seed (556)

modH <- randomForest(Rs_Norm ~  Measure_Month  #+TS  + IGBP
                     +P_LastMonth   +Pm
                     + Tm + Latitude +absLog + Climate_Koeppon + T_LastMonth
                     + LAI  + Elevation+ IGBP , data=TrainSetH, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modH
#Variable impor1tance measures
importance (modH)
#Looking at the OOB error reduction with the tree size
plot (modH) 
#Plotting the variable importance
varImpPlot(modH)


prediction.vaH <- predict(modH, ValidSetH) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaH, ValidSetH$Rs_Norm, method = "pearson", alpha=.05)

VaH <- cbind(prediction.vaH, ValidSetH)
names(VaH)[1] <- "Prediction"

valH <- lm(Rs_Norm~Prediction, data=VaH)
summary(valH)

set.seed (556)

modM <- randomForest(Rs_Norm ~  Measure_Month  #+TS  + IGBP
                     +P_LastMonth   +Pm
                     + Tm + Latitude +absLog + Climate_Koeppon + T_LastMonth
                     + LAI  + Elevation+ IGBP , data=TrainSetM, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modM
#Variable impor1tance measures
importance (modM)
#Looking at the OOB error reduction with the tree size
plot (modM) 
#Plotting the variable importance
varImpPlot(modM)


prediction.vaM <- predict(modM, ValidSetM) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaM, ValidSetM$Rs_Norm, method = "pearson", alpha=.05)

VaM <- cbind(prediction.vaM, ValidSetM)
names(VaM)[1] <- "Prediction"

valM <- lm(Rs_Norm~Prediction, data=VaM)
summary(valM)


vaLMH <- rbind(VaL, VaM,VaH)
names(vaLMH)[1] <- "Prediction"

valLMH <- lm(Rs_Norm~Prediction, data=vaLMH)
summary(valLMH)

cor.test(vaLMH$Prediction, vaLMH$Rs_Norm, method = "spearman", alpha=.05)

mean(vaLMH$Prediction,na.rm = T)

rmse <- sqrt(mean((vaLMH$Prediction - vaLMH$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(vaLMH$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- vaLMH$Prediction - vaLMH$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

e <- 1-((sum(squared_diff))/sum((vaLMH$Rs_Norm - mean(vaLMH$Rs_Norm))^2))
e
# 打印结果
print(mse)

AIC(val, valLMH)

globalDataL <- subset(globalData, globalData$Elevation < 500 )
globalDataM <- subset(globalData, globalData$Elevation >= 500 & globalData$Elevation <1500 )
globalDataH <- subset(globalData, globalData$Elevation >=1500 )

#A <- subset(globalData, TopClimate == "A" )
#B <- subset(globalData, TopClimate == "B" )
#C <- subset(globalData, TopClimate == "C" )
#D <- subset(globalData, TopClimate == "D" )
#E <- subset(globalData, TopClimate == "E" )

#globalData <- globalData[complete.cases(globalData),]
globalData$prediction <- predict(mod1, globalData)

mean(globalData$prediction,na.rm = T)

globalDataL$prediction <- predict(modL, globalDataL)
globalDataM$prediction <- predict(modM, globalDataM)
globalDataH$prediction <- predict(modH, globalDataH)

mean(globalDataL$prediction,na.rm = T)
mean(globalDataM$prediction,na.rm = T)
mean(globalDataH$prediction,na.rm = T)

globalDataLMH <-rbind(globalDataL,globalDataM,globalDataH)

mean(globalDataLMH$prediction,na.rm = T)


globalData<-data.frame(globalData)
globalData<-globalData[order(globalData$...1),]


globalDataLMH<-data.frame(globalDataLMH)
globalDataLMH<-globalDataLMH[order(globalDataLMH$...1),]


#mean(globalData$prediction,na.rm = T)
#globalData <- globalData[complete.cases(globalData),]

GL7 <- subset(globalDataLMH, Measure_Month==7 )
theme_update(plot.title = element_text(hjust = 0.5))

# 设置色带
#cols <- c("#FBF8EB", "#F2E49B", "#CAE784", "#1D934D", "#CE6BBB")
# 在计算分位数之前排除包含缺失值的观测
GL7_no_na <- GL7[!is.na(GL7 $prediction), ]
# 定义分位数值，可以根据需要进行调整
#quantiles <- quantile(G7_no_na$prediction, probs = c(0, 0.25, 0.5, 0.75, 1))
# 绘图

ggplot(GL7_no_na, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )

GLMH1 <- subset(globalDataLMH, Measure_Month==1 )
GLMH2 <- subset(globalDataLMH, Measure_Month==2 )
GLMH3 <- subset(globalDataLMH, Measure_Month==3 )
GLMH4 <- subset(globalDataLMH, Measure_Month==4 )
GLMH5 <- subset(globalDataLMH, Measure_Month==5 )
GLMH6 <- subset(globalDataLMH, Measure_Month==6 )
GLMH7 <- subset(globalDataLMH, Measure_Month==7 )
GLMH8 <- subset(globalDataLMH, Measure_Month==8 )
GLMH9 <- subset(globalDataLMH, Measure_Month==9 )
GLMH10 <- subset(globalDataLMH, Measure_Month==10 )
GLMH11 <- subset(globalDataLMH, Measure_Month==11 )
GLMH12 <- subset(globalDataLMH, Measure_Month==12 )


GLMH1$Predicted_Rs <-(GLMH1$prediction+GLMH2$prediction+GLMH3$prediction+GLMH4$prediction+GLMH5$prediction+GLMH6$prediction+GLMH7$prediction+GLMH8$prediction+GLMH9$prediction+GLMH10$prediction+GLMH11$prediction)/11#+G12$prediction

GLMH1$predictions <-(GLMH1$prediction+GLMH2$prediction+GLMH3$prediction+GLMH4$prediction+GLMH5$prediction+GLMH6$prediction+GLMH7$prediction+GLMH8$prediction+GLMH9$prediction+GLMH10$prediction+GLMH11$prediction)/11#+G12$prediction

GLMH1_no_na <- GLMH1[!is.na(GLMH1 $predictions), ]
write.csv(GLMH1_no_na,"GLMH1_no_na.csv")
ggplot(GLMH1_no_na, aes(Longitude, Latitude, color = predictions)) +
  scale_colour_steps(low = "white", high = "#006400", n.breaks=8)+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "Predicted Rs") +
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    legend.position = c(0.12,0.3)
  )

ggplot(GLMH1_no_na, aes(Longitude, Latitude, color = predictions)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )

my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
GLMHR <- ggplot() + 
  geom_sf() + 
  geom_tile(data = GLMH1_no_na,aes(Longitude, Latitude, fill = Predicted_Rs))+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
GLMHR<-GLMHR+ annotate("text", x = -150 , y = -10,label= ("Elevation model"),size = 3)+ annotate("text", x = -150 , y = -19,label= ("Global Mean Rs"),size = 3)+ annotate("text", x = -150 , y = -28,label= expression(paste("1.80"," ",g," ","C"," ", m^-2," ", "day"^-1)),size = 3)
GLMHR

GLMH1_no_na$Uncertainty <- (GLMH1_no_na$Predicted_Rs-G1_no_na$Predicted_Rs)/G1_no_na$Predicted_Rs
GLMH_no_na <- GLMH1_no_na[!is.na(GLMH1_no_na$Uncertainty), ]

mean(GLMH1_no_na$Uncertainty,na.rm = T)

my_colormap <- colorRampPalette(rev(brewer.pal(11,'BrBG')))(32)
GLMHU <- ggplot() + 
  geom_sf() + 
  geom_tile(data =GLMH_no_na,aes(Longitude, Latitude, fill = Uncertainty))+
  scale_fill_gradientn(colours = my_colormap,n.breaks=7,limits=c(-1,1))+labs( "Predicted Rs")+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  ) 
#GABCU<-GABCU+ annotate("text", x = -120 , y = -20,label= expression(paste("Global mean Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")))

GLMHU<-GLMHU + annotate("text", x = -150 , y = -10,label= ("Single global model"),size = 3)+ annotate("text", x = -150 , y = -18,label= ("vs."),size = 3)+ annotate("text", x = -150 , y = -26,label= ("Elevation model"),size = 3)
GLMHU