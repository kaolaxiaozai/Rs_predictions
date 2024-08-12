set.seed (556)

mod1 <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                       Pm + Tm +Latitude  +absLog #+absLat#
                     #+Longitude 
                     + Climate_Koeppon  + IGBP + T_LastMonth
                     + LAI  + Elevation , data=TrainSet, 
                     keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
mod1
#Variable impor1tance measures
importance (mod1)
#Looking at the OOB error reduction with the tree size
plot (mod1) 
#Plotting the variable importance
varImpPlot(mod1)


prediction.va <- predict(mod1, ValidSet) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.va, ValidSet$Rs_Norm, method = "pearson", alpha=.05)

cor.test(prediction.va, ValidSet$Rs_Norm, method = "spearman", alpha=.05)

va <- cbind(prediction.va, ValidSet)
names(va)[1] <- "Prediction"

val <- lm(Rs_Norm~Prediction, data=va)
summary(val)

#valIABC <- lm(Rs_Norm~Prediction, data=vaIABC)
#summary(valIABC)

mean(va$Prediction,na.rm = T)

rmse <- sqrt(mean((va$Prediction - va$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(va$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- va$Prediction - va$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

e <- 1-((sum(squared_diff))/sum((va$Rs_Norm - mean(va$Rs_Norm))^2))
e
# 打印结果
print(mse)

#write.csv(va, 'JD.csv')
#################################
set.seed (556)

modIA <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm + Latitude +absLog #+absLat#+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation ,  data=TrainSetIA, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modIA
#Variable impor1tance measures
importance (modIA)
#Looking at the OOB error reduction with the tree size
plot (modIA) 
#Plotting the variable importance
varImpPlot(modIA)


prediction.vaIA <- predict(modIA, ValidSetIA) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaIA, ValidSetIA$Rs_Norm, method = "pearson", alpha=.05)

VaIA <- cbind(prediction.vaIA, ValidSetIA)
names(VaIA)[1] <- "Prediction"

valIA <- lm(Rs_Norm~Prediction, data=VaIA)
summary(valIA)



set.seed (556)

modIB <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm + Latitude +absLog #+absLat#+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation ,  data=TrainSetIB, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modIB
#Variable impor1tance measures
importance (modIB)
#Looking at the OOB error reduction with the tree size
plot (modIB) 
#Plotting the variable importance
varImpPlot(modIB)


prediction.vaIB <- predict(modIB, ValidSetIB) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaIB, ValidSetIB$Rs_Norm, method = "pearson", alpha=.05)

VaIB <- cbind(prediction.vaIB, ValidSetIB)
names(VaIB)[1] <- "Prediction"

valIB <- lm(Rs_Norm~Prediction, data=VaIB)
summary(valIB)

set.seed (556)

modIC <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm + Latitude +absLog #+absLat#+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation ,  data=TrainSetIC, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modIC
#Variable impor1tance measures
importance (modIC)
#Looking at the OOB error reduction with the tree size
plot (modIC) 
#Plotting the variable importance
varImpPlot(modIC)


prediction.vaIC <- predict(modIC, ValidSetIC) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaIC, ValidSetIC$Rs_Norm, method = "pearson", alpha=.05)

VaIC <- cbind(prediction.vaIC, ValidSetIC)
names(VaIC)[1] <- "Prediction"

valIC <- lm(Rs_Norm~Prediction, data=VaIC)
summary(valIC)

set.seed (556)

modID <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm + Latitude +absLog #+absLat#+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation ,data=TrainSetID, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modID
#Variable impor1tance measures
importance (modID)
#Looking at the OOB error reduction with the tree size
plot (modID) 
#Plotting the variable importance
varImpPlot(modID)


prediction.vaID <- predict(modID, ValidSetID) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaID, ValidSetID$Rs_Norm, method = "pearson", alpha=.05)

VaID <- cbind(prediction.vaID, ValidSetID)
names(VaID)[1] <- "Prediction"

valID <- lm(Rs_Norm~Prediction, data=VaID)
summary(valID)

set.seed (556)

modIE <- randomForest(Rs_Norm ~ P_LastMonth   + Measure_Month + #TS + 
                        Pm + Tm + Latitude +absLog #+absLat#+Longitude 
                      + Climate_Koeppon  + IGBP + T_LastMonth
                      + LAI  + Elevation , data=TrainSetIE, 
                      keep.inbag=TRUE, mtry=5, importance=TRUE, ntree=1000)



#The summary of the Standard Random Forest model
modIE
#Variable impor1tance measures
importance (modIE)
#Looking at the OOB error reduction with the tree size
plot (modIE) 
#Plotting the variable importance
varImpPlot(modIE)


prediction.vaIE <- predict(modIE, ValidSetIE) #predict w/ validation 

#Compare correlation b/w validation predictions and observations
cor.test(prediction.vaIE, ValidSetIE$Rs_Norm, method = "pearson", alpha=.05)

VaIE <- cbind(prediction.vaIE, ValidSetIE)
names(VaIE)[1] <- "Prediction"

valIE <- lm(Rs_Norm~Prediction, data=VaIE)
summary(valIE)

#CK<-TrainSet$Climate_Koeppon
#head(CK)
#K<-globalData$IGBP
#head(K)

mean(VaIA$Prediction,na.rm = T)
mean(VaIB$Prediction,na.rm = T)
mean(VaIC$Prediction,na.rm = T)
mean(VaID$Prediction,na.rm = T)
mean(VaIE$Prediction,na.rm = T)

vaIABC <- rbind(VaIA, VaIB,VaIC,VaID,VaIE)
names(vaIABC)[1] <- "Prediction"

#write.csv(vaIABC, 'ABC.csv')

valIABC <- lm(Rs_Norm~Prediction, data=vaIABC)
summary(valIABC)

cor.test(vaIABC$Prediction, vaIABC$Rs_Norm, method = "spearman", alpha=.05)

mean(vaIABC$Prediction,na.rm = T)

rmse <- sqrt(mean((vaIABC$Prediction - vaIABC$Rs_Norm )^2))
rmse
# 计算观测值数量
n <- length(vaIABC$Rs_Norm)

# 计算每个观测值的差异，并求平方
diff <- vaIABC$Prediction - vaIABC$Rs_Norm
squared_diff <- diff^2

# 计算MSE
mse <- sum(squared_diff) / n

# 打印结果
print(mse)

e <- 1-((sum(squared_diff))/sum((vaIABC$Rs_Norm - mean(vaIABC$Rs_Norm))^2))
e

AIC(val, valIABC)

globalData<- read_csv("E:/论文/图片/globalData.csv",locale=locale(encoding="GBK"))
#globalData <- data.frame(globalData)
#globalData <-filter(globalData,Climate_Koeppon %in% TrainSet$Climate_Koeppon)
#globalData <-filter(globalData, IGBP %in% TrainSet$IGBP)
#globalData <- subset(globalData, !is.na(globalData$T_LastMonth))
#globalData <-globalData[complete.cases(globalData),]

#globalData$Rs_Norm<-0
globalData %>% 
  mutate(IGBP1 = case_when(
    IGBP %in% c("CRO", "CVM") ~ "Cro",
    IGBP %in% c("BSV", "OSH") ~ "Shr",
    IGBP %in% c("DBF", "EBF", "ENF","MF") ~ "For",
    #ClimateTypes %in% c() ~ "Cs",
    #ClimateTypes %in% c("Cwa", "Cwb", "Cwc") ~ "Cw",
    IGBP %in% c("GRA", "SAV", "WSA") ~ "Gra",
    #ClimateTypes %in% c("Dsa", "Dsb", "Dsc", "Dwa", "Dwb", "Dwc", "Dwd") ~ "Dsw",
    IGBP %in% c("SNO", "URB","WAT") ~ "Oth",
    TRUE ~ "Oth")) -> globalData

globalData$Climate_Koeppon <- as.factor(globalData$Climate_Koeppon)
globalData$IGBP <- as.factor(globalData$IGBP)
globalData$Measure_Month <- as.factor(globalData$Measure_Month)
globalData$absLat <- abs(globalData$Latitude)
globalData$absLog <- abs(globalData$Longitude)
globalData$IGBP1 <- as.factor(globalData$IGBP1)


GIA <- subset(globalData, IGBP1 == "Cro" )
GIB <- subset(globalData, IGBP1 == "Shr" )
GIC <- subset(globalData, IGBP1 == "For" )
GID <- subset(globalData, IGBP1 == "Gra" )
GIE <- subset(globalData, IGBP1 == "Oth" )

#globalData <- globalData[complete.cases(globalData),]
globalData$prediction <- predict(mod1, globalData)

mean(globalData$prediction,na.rm = T)

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

G1$predictions <-(G1$prediction+G2$prediction+G3$prediction+G4$prediction+G5$prediction+G6$prediction+G7$prediction+G8$prediction+G9$prediction+G10$prediction+G11$prediction)/11#+G12$prediction


G1$Predicted_Rs <-(G1$prediction+G2$prediction+G3$prediction+G4$prediction+G5$prediction+G6$prediction+G7$prediction+G8$prediction+G9$prediction+G10$prediction+G11$prediction)/11#+G12$prediction

G1_no_na <- G1[!is.na(G1 $predictions), ]
write.csv(G1_no_na,"G1_no_na.csv")

ggplot(G1_no_na, aes(Longitude, Latitude, color = predictions)) +#geom_sf()+
  #scale_fill_viridis_c(option = "D",direction = -1,name='MAT(℃)',na.value = 'transparent')+
  #scale_colour_steps(low = "white", high = "#386CB0", n.breaks=7)+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "Predicted Rs") +
  scale_color_viridis_c(option = "E")+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    legend.position = c(0.11,0.3)
  )

ggplot() + 
  geom_sf() + 
  geom_tile(data = G1_no_na,aes(Longitude, Latitude, fill = predictions))+
  #scale_fill_gradientn(colours = my_colormap)+
  labs(colour = "Predicted Rs") +
  scale_color_viridis_c(option = "C")+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )
my_colormap <- colorRampPalette(rev(brewer.pal(11,'PiYG')))(32)
Map_point_kde_nomask <- ggplot() + 
  geom_sf() + 
  geom_tile(data = G1_no_na,aes(Longitude, Latitude, fill = predictions))+
  scale_fill_gradientn(colours = my_colormap)+
  labs(colour = "Predicted Rs") +
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )

Map_point_kde_nomask

my_colormap <- colorRampPalette(rev(brewer.pal(9,'Oranges')))(32)
Map_point_kde_nomask <- ggplot() + 
  geom_sf() + 
  geom_tile(data = G1_no_na,aes(Longitude, Latitude, fill = predictions))+
  scale_fill_gradientn(colours = my_colormap)+
  labs(colour = "Predicted Rs") +
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )

Map_point_kde_nomask

GIA$prediction <- predict(modIA, GIA)
GIB$prediction <- predict(modIB, GIB)
GIC$prediction <- predict(modIC, GIC)
GID$prediction <- predict(modID, GID)
GIE$prediction <- predict(modIE, GIE)

mean(GIA$prediction,na.rm = T)
mean(GIB$prediction,na.rm = T)
mean(GIC$prediction,na.rm = T)
mean(GID$prediction,na.rm = T)
mean(GIE$prediction,na.rm = T)

GIA$prediction1 <- predict(mod1, GIA)
GIB$prediction1 <- predict(mod1, GIB)
GIC$prediction1 <- predict(mod1, GIC)
GID$prediction1 <- predict(mod1, GID)
GIE$prediction1 <- predict(mod1, GIE)


globalDataIABC <-rbind(GIA,GIB,GIC,GID,GIE)

mean(globalDataIABC$prediction,na.rm = T)
mean(globalDataIABC$prediction1,na.rm = T)


globalData<-data.frame(globalData)
globalData<-globalData[order(globalData$...1),]


globalDataIABC<-data.frame(globalDataIABC)
globalDataIABC<-globalDataIABC[order(globalDataIABC$...1),]

GIABC1 <- subset(globalDataIABC, Measure_Month==1 )
GIABC2 <- subset(globalDataIABC, Measure_Month==2 )
GIABC3 <- subset(globalDataIABC, Measure_Month==3 )
GIABC4 <- subset(globalDataIABC, Measure_Month==4 )
GIABC5 <- subset(globalDataIABC, Measure_Month==5 )
GIABC6 <- subset(globalDataIABC, Measure_Month==6 )
GIABC7 <- subset(globalDataIABC, Measure_Month==7 )
GIABC8 <- subset(globalDataIABC, Measure_Month==8 )
GIABC9 <- subset(globalDataIABC, Measure_Month==9 )
GIABC10 <- subset(globalDataIABC, Measure_Month==10 )
GIABC11 <- subset(globalDataIABC, Measure_Month==11 )
GIABC12 <- subset(globalDataIABC, Measure_Month==12 )


GIABC1$Predicted_Rs <-(GIABC1$prediction+GIABC2$prediction+GIABC3$prediction+GIABC4$prediction+GIABC5$prediction+GIABC6$prediction
                       +GIABC7$prediction+GIABC8$prediction+GIABC9$prediction+GIABC10$prediction+GIABC11$prediction)/11#+G12$prediction

GIABC1$predictions <-(GIABC1$prediction+GIABC2$prediction+GIABC3$prediction+GIABC4$prediction+GIABC5$prediction+GIABC6$prediction
                      +GIABC7$prediction+GIABC8$prediction+GIABC9$prediction+GIABC10$prediction+GIABC11$prediction)/11#+G12$prediction

GIABC1_no_na <- GIABC1[!is.na(GIABC1$predictions), ]

write.csv(GIABC1_no_na,"GIABC1_no_na.csv")

ggplot(GIABC1_no_na, aes(Longitude, Latitude, color = predictions)) +
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

my_colormap <- colorRampPalette(rev(brewer.pal(9,'GnBu')))(32)
GIABC <- ggplot() + 
  geom_sf() + 
  geom_tile(data =GIABC1_no_na,aes(Longitude, Latitude, fill = predictions))+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
GIABC
#GIABC1$predictions <-data.frame(GIABC1$predictions)
#G1$predictions <-data.frame(G1$predictions)
GIABC1$predictionI1 <- (GIABC1$predictions-G1$predictions)/G1$predictions
GIABCI1_no_na <- GIABC1[!is.na(GIABC1$predictionI1), ]

ggplot(GIABCI1_no_na, aes(Longitude, Latitude, color = predictionI1)) + 
  scale_colour_steps2(low = "#984EA3",mid="white" ,high = "#386CB0",n.breaks=8)+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction")  +labs(color = "Predicted Rs") +
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    legend.position = c(0.11,0.3))

#mean(globalData$prediction,na.rm = T)
#globalData <- globalData[complete.cases(globalData),]
G7 <- subset(globalData, Measure_Month==7 )
theme_update(plot.title = element_text(hjust = 0.5))

GI7 <- subset(globalDataIABC, Measure_Month==7 )
theme_update(plot.title = element_text(hjust = 0.5))

# 设置色带
#cols <- c("#FBF8EB", "#F2E49B", "#CAE784", "#1D934D", "#CE6BBB")
# 在计算分位数之前排除包含缺失值的观测
G7_no_na <- G7[!is.na(G7 $prediction), ]
# 定义分位数值，可以根据需要进行调整
#quantiles <- quantile(G7_no_na$prediction, probs = c(0, 0.25, 0.5, 0.75, 1))
# 绘图

ggplot(G7, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )

G7_no_na <-data.frame(G7_no_na)
ggplot(G7_no_na, aes(Longitude, Latitude, colour = prediction)) + 
  scale_colour_steps(low = "white", high = "red", n.breaks=5)+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction")+ #+ scale_color_viridis_c(option = "B")
  #theme(panel.grid=element_blank()) +
  #geom_text(mapping = aes(label = round(pergdp,2)), size = 3, hjust = -0.3)+
  #scale_y_continuous(limits = c(0,35),expand = c(0,0))+
  theme_bw()
#scale_fill_gradientn(colours = cols, values = scales::rescale(quantiles))+theme_classic()
#panel.background=element_blank()
#panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))


ggplot(GI7, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") +
  scale_color_viridis_c()+theme(
    panel.grid=element_blank(), 
    #panel.background=element_blank()
    #panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
  )
GI7$predictionI <- (GI7$prediction-G7$prediction)/G7$prediction
ggplot(GI7, aes(Longitude, Latitude, color = predictionI)) + 
  scale_colour_steps(low = "#386CB0", high = "red",n.breaks=5)+
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90)) +
  labs(color = "RC prediction") 
#scale_color_viridis_c()+theme(
#panel.grid=element_blank(), 
#panel.background=element_blank()
#panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))

#ggplot(globalData, aes(x = Longitude, y = Latitude)) +
#  geom_point(size = 0.3) +
#  lims(x = c(-180, 180), y = c(-90, 90))

# Get rid of points with missing warner Rs, myco, and climate data
#globalData <- globalData[complete.cases(globalData),]

globalDataD<-subset(G7, G7$TopClimate=="D")
#globalDataD <-filter(globalData, TopClimate %in% globalDataD$TopClimate)
GA10 <-subset(globalDataD,Measure_Month==10)
GA10 <- data.frame(GA10)

mean(globalDataD$prediction,na.rm = T)

library(ggplot2)
library(plyr)
library(maptools)
library(sp)

mapWorld <- borders("world", colour="gray60", fill="#DCDCDC") # create a layer of borders
#m <-ggplot()+ theme_bw()+ mapWorld
#m

mp <- ggplot(data = globalDataD, aes(Longitude, Latitude, color = prediction))+       
  theme_bw() +  mapWorld + 
  geom_point(size = 0.3) +
  #lims(x = c(-180, 180), y = c(-90, 90))+ 
  labs(color = "RC prediction") +
  scale_color_viridis_c()+scale_x_continuous(breaks = seq(-100,100,100))

mp

ggplot(GA10, aes(Longitude, Latitude, color = prediction)) + 
  geom_point(size = 0.3) +
  lims(x = c(-180, 180), y = c(-90, 90))+ 
  labs(color = "RC prediction") +
  scale_color_viridis_c()
# Look at the distribution of predictions

ggplot(G7, aes(x = prediction)) +
  geom_histogram(color = "black", fill = "lightgrey") +
  labs(x = "RC Prediction")

my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
Map <- ggplot() + 
  geom_sf() + 
  geom_tile(data = G1_no_na,aes(Longitude, Latitude, fill = Predicted_Rs))+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
Map <-Map+ annotate("text", x = -150 , y = -10,label= ("Single global model"),size = 3)+ annotate("text", x = -150 , y = -19,label= ("Global Mean Rs"),size = 3)+ annotate("text", x = -150 , y = -28,label= expression(paste("1.75"," ",g," ","C"," ", m^-2," ", "day"^-1)),size = 3)

Map

my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
GIABC <- ggplot() + 
  geom_sf() + 
  geom_tile(data =GIABC1_no_na,aes(Longitude, Latitude, fill = Predicted_Rs))+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
GIABC<-GIABC+ annotate("text", x = -150 , y = -10,label= ("Land cover model"),size = 3)+ annotate("text", x = -150 , y = -19,label= ("Global Mean Rs"),size = 3)+ annotate("text", x = -150 , y = -28,label= expression(paste("1.58"," ",g," ","C"," ", m^-2," ", "day"^-1)),size = 3)

GIABC

GIABC1$Uncertainty <- (GIABC1$Predicted_Rs-G1$Predicted_Rs)/G1$Predicted_Rs
GIABC1_no_na <- GIABC1[!is.na(GIABC1$Uncertainty), ]

mean(GIABC1$Uncertainty,na.rm = T)

my_colormap <- colorRampPalette(rev(brewer.pal(11,'BrBG')))(32)
GIABCU <- ggplot() + 
  geom_sf() + 
  geom_tile(data =GIABC1_no_na,aes(Longitude, Latitude, fill = Uncertainty))+
  scale_fill_gradientn(colours = my_colormap,n.breaks=7,limits=c(-1,1))+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )+labs(colour = "Predicted Rs") 
GIABCU<-GIABCU+ annotate("text", x = -150 , y = -10,label= ("Single global model"),size = 3)+ annotate("text", x = -150 , y = -18,label= ("vs."),size = 3)+ annotate("text", x = -150 , y = -26,label= ("Land cover model"),size = 3)
GIABCU

#library(patchwork)
#GABCR/GIABC|GYHR/GLMHR
