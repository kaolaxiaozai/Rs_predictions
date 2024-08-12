


set.seed (556)
RsForRandomForest_naOMIT <- missRanger(RsForRandomForest_naOM)

#print(RsForRandomForest_naOMIT)
mean(RsForRandomForest_naOMIT$Rs_Norm,na.rm = T)
#write.csv(RsForRandomForest_naOMIT, 'RsForRandomForest_naOMIT.csv')
#plot(RsForRandomForest_naOMIT$TS,RsForRandomForest_naOMIT$Tm)
plot(RsForRandomForest_naOMIT$Tm,RsForRandomForest_naOMIT$TS, #main = '训练集',
     xlab = 'Tm', ylab = 'Ts',xlim=c(-30,50), ylim=c(-30,50),col = "#00000030",pch=19)
abline(0, 1,lty = "dashed")
z<-line(RsForRandomForest_naOMIT$TS,RsForRandomForest_naOMIT$Tm)
abline(coef(z),lwd=2, lty = 1,xaxs="s")
text(-18, 40, c("R=0.91,p<2.2e-16"),cex=1,font=2,col="#000000")

RsForRandomForest_naOMIT %>% 
  mutate(IBGP1 = case_when(
    IGBP %in% c("CRO", "CVM") ~ "Cro",
    IGBP %in% c("BSV", "OSH") ~ "Shr",
    IGBP %in% c("DBF", "EBF", "ENF","MF") ~ "For",
    #ClimateTypes %in% c() ~ "Cs",
    #ClimateTypes %in% c("Cwa", "Cwb", "Cwc") ~ "Cw",
    IGBP %in% c("GRA", "SAV", "WSA") ~ "Gra",
    #ClimateTypes %in% c("Dsa", "Dsb", "Dsc", "Dwa", "Dwb", "Dwc", "Dwd") ~ "Dsw",
    IGBP %in% c("SNO", "URB","WAT") ~ "Oth",
    TRUE ~ "Oth")) -> RsForRandomForest_naOMIT

A <- subset(RsForRandomForest_naOMIT, TopClimatetype == "A" )
B <- subset(RsForRandomForest_naOMIT, TopClimatetype == "B" )
C <- subset(RsForRandomForest_naOMIT, TopClimatetype == "C" )
D <- subset(RsForRandomForest_naOMIT, TopClimatetype == "D" )
E <- subset(RsForRandomForest_naOMIT, TopClimatetype == "E" )

mean(A$Rs_Norm,na.rm = T)
mean(B$Rs_Norm,na.rm = T)
mean(C$Rs_Norm,na.rm = T)
mean(D$Rs_Norm,na.rm = T)
mean(E$Rs_Norm,na.rm = T)

#write.csv(A$Rs_Norm, 'RSA.csv')
#write.csv(B$Rs_Norm, 'RSB.csv')
#write.csv(C$Rs_Norm, 'RSC.csv')
#write.csv(D$Rs_Norm, 'RSD.csv')
#write.csv(E$Rs_Norm, 'RSE.csv')
RSA<- read_csv("E:/R/RSA.csv",locale=locale(encoding="GBK"))

dtt <- RSA %>%
  as_tibble(rownames = 'gene') %>%
  gather(key = Climate_Koeppon,
         value = exp,
         -gene)
dtt$Climate_Koeppon <- factor(dtt$Climate_Koeppon, levels = unique(dtt$Climate_Koeppon)) #转换为因子，固定绘图celltype顺序
#head(dtt)

c4a_gui()
mycol1 <- c4a('10',6)
mycol2 <- c4a('bold',6)

#山脊图添加密度短竖线：

p5 <- ggplot(data = dtt, aes(x = exp, y = Climate_Koeppon, fill = Climate_Koeppon)) +
  geom_density_ridges(alpha = 0.8,
                      color = 'white',
                      rel_min_height = 0.01, #尾部修剪，数值越大修剪程度越高
                      scale = 1.6, #山脊重叠程度调整，scale = 1时刚好触及基线，数值越大重叠度越高
                      quantile_lines = TRUE, #显示分位数线
                      quantiles = 2 #仅显示中位数线
  ) +labs(x = expression(paste("Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")), y = 'Climate_Koeppon')+scale_x_continuous(limits =c(-1,13))+
  scale_fill_manual(values = mycol1) +theme_classic()
p5<-p5 + annotate("text", x = 11 , y = 1.4,label = "n=1092",colour="black") + 
  annotate("text", x = 11 , y = 2.4,label = "n=782",colour="black")+ 
  annotate("text", x = 11 , y = 3.4,label = "n=7515",colour="black")+ 
  annotate("text", x = 11 , y = 4.4,label = "n=3196",colour="black")+ 
  annotate("text", x = 11 , y = 5.4,label = "n=126",colour="black") + theme(panel.grid =   element_blank())
p5



#A <- subset(RsForRandomForest_naOMIT, TopClimatetype == "A" )
#B <- subset(RsForRandomForest_naOMIT, TopClimatetype == "B" )
#C <- subset(RsForRandomForest_naOMIT, TopClimatetype == "C" )
#D <- subset(RsForRandomForest_naOMIT, TopClimatetype == "D" )
#E <- subset(RsForRandomForest_naOMIT, TopClimatetype == "E" )

IA <- subset(RsForRandomForest_naOMIT, IBGP1 == "Cro" )
IB <- subset(RsForRandomForest_naOMIT, IBGP1 == "Shr" )
IC <- subset(RsForRandomForest_naOMIT, IBGP1 == "For" )
ID <- subset(RsForRandomForest_naOMIT, IBGP1 == "Gra" )
IE <- subset(RsForRandomForest_naOMIT, IBGP1 == "Oth" )

mean(IA$Rs_Norm,na.rm = T)
mean(IB$Rs_Norm,na.rm = T)
mean(IC$Rs_Norm,na.rm = T)
mean(ID$Rs_Norm,na.rm = T)
mean(IE$Rs_Norm,na.rm = T)

#write.csv(IA$Rs_Norm, 'RSIA.csv')
#write.csv(IB$Rs_Norm, 'RSIB.csv')
#write.csv(IC$Rs_Norm, 'RSIC.csv')
#write.csv(ID$Rs_Norm, 'RSID.csv')
#write.csv(IE$Rs_Norm, 'RSIE.csv')
RSIA<- read_csv("E:/R/RSIA.csv",locale=locale(encoding="GBK"))

dttI <- RSIA %>%
  as_tibble(rownames = 'gene') %>%
  gather(key = LandCover,
         value = exp,
         -gene)
dttI$LandCover <- factor(dttI$LandCover, levels = unique(dttI$LandCover)) #转换为因子，固定绘图celltype顺序
#head(dtt)

c4a_gui()
mycol1 <- c4a('10',6)
mycol2 <- c4a('bold',6)

#山脊图添加密度短竖线：

p6 <- ggplot(data = dttI, aes(x = exp, y = LandCover, fill = LandCover)) +
  geom_density_ridges(alpha = 0.8,
                      color = 'white',
                      rel_min_height = 0.01, #尾部修剪，数值越大修剪程度越高
                      scale = 1.6, #山脊重叠程度调整，scale = 1时刚好触及基线，数值越大重叠度越高
                      quantile_lines = TRUE, #显示分位数线
                      quantiles = 2, #仅显示中位数线
                      na.rm=TRUE
  ) +labs(x = expression(paste("Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")), y = 'LandCover')+scale_x_continuous(limits =c(-1,13))+
  scale_fill_manual(values = mycol1) +
  theme_classic()
p6<-p6 + annotate("text", x = 10 , y = 1.4,label = "n=4065",colour="black") + 
  annotate("text", x = 10 , y = 2.4,label = "n=468",colour="black")+ 
  annotate("text", x = 10 , y = 3.4,label = "n=5387",colour="black")+ 
  annotate("text", x = 10 , y = 4.4,label = "n=2446",colour="black")+ 
  annotate("text", x = 10 , y = 5.4,label = "n=345",colour="black") + theme(panel.grid =   element_blank())
p6

YQ <- subset(RsForRandomForest_naOMIT, MYear == "Q" )
YH <- subset(RsForRandomForest_naOMIT, MYear == "H" )

mean(YQ$Rs_Norm,na.rm = T)
mean(YH$Rs_Norm,na.rm = T)

#write.csv(YQ$Rs_Norm, 'YQ.csv')
#write.csv(YH$Rs_Norm, 'YH.csv')

RSY<- read_csv("E:/R/YH.csv",locale=locale(encoding="GBK"))

dttY <- RSY %>%
  as_tibble(rownames = 'gene') %>%
  gather(key = Year,
         value = exp,
         -gene)
dttY$Year <- factor(dttY$Year, levels = unique(dttY$Year)) #转换为因子，固定绘图celltype顺序
#head(dtt)

c4a_gui()
mycol1 <- c4a('10',6)
mycol2 <- c4a('bold',6)

#山脊图添加密度短竖线：

p7 <- ggplot(data = dttY, aes(x = exp, y = Year, fill = Year)) +
  geom_density_ridges(alpha = 0.8,
                      color = 'white',
                      rel_min_height = 0.01, #尾部修剪，数值越大修剪程度越高
                      scale = 1.6, #山脊重叠程度调整，scale = 1时刚好触及基线，数值越大重叠度越高
                      quantile_lines = TRUE, #显示分位数线
                      quantiles = 2, #仅显示中位数线
                      na.rm=TRUE
  ) +labs(x = expression(paste("Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")), y = 'Year of data')+scale_x_continuous(limits =c(-1,13))+
  scale_fill_manual(values = mycol1) +
  theme_classic()
p7<-p7 + annotate("text", x = 10 , y = 1.4,label = "n=9060",colour="black") + 
  annotate("text", x = 10 , y = 2.4,label = "n=3651",colour="black")+ 
  theme(panel.grid =   element_blank())
p7

RsForRandomForest_naOMITL <- subset(RsForRandomForest_naOMIT, RsForRandomForest_naOMIT$Elevation < 500 )
RsForRandomForest_naOMITM <- subset(RsForRandomForest_naOMIT, RsForRandomForest_naOMIT$Elevation >= 500 & RsForRandomForest_naOMIT$Elevation <1500 )
RsForRandomForest_naOMITH <- subset(RsForRandomForest_naOMIT, RsForRandomForest_naOMIT$Elevation >1500 )
RSE <- cbind(RsForRandomForest_naOMITL$Rs_Norm,RsForRandomForest_naOMITM$Rs_Norm,RsForRandomForest_naOMITM$Rs_Norm)

mean(RsForRandomForest_naOMITL$Rs_Norm,na.rm = T)
mean(RsForRandomForest_naOMITM$Rs_Norm,na.rm = T)
mean(RsForRandomForest_naOMITH$Rs_Norm,na.rm = T)

#write.csv(RSE, 'RSE.csv')
RSE<- read_csv("E:/R/RSE.csv",locale=locale(encoding="GBK"))

#names(RSE)[1] <- "Low"
#names(RSE)[2] <- "Middle"
#names(RSE)[3] <- "High"

dttE <- RSE %>%
  as_tibble(rownames = 'gene') %>%
  gather(key = Elevation,
         value = exp,
         -gene)
dttE$Elevation <- factor(dttE$Elevation, levels = unique(dttE$Elevation)) #转换为因子，固定绘图celltype顺序
#head(dtt)

c4a_gui()
mycol1 <- c4a('10',6)
mycol2 <- c4a('bold',6)

#山脊图添加密度短竖线：

p8 <- ggplot(data = dttE, aes(x = exp, y = Elevation, fill = Elevation)) +
  geom_density_ridges(alpha = 0.8,
                      color = 'white',
                      rel_min_height = 0.01, #尾部修剪，数值越大修剪程度越高
                      scale = 1.6, #山脊重叠程度调整，scale = 1时刚好触及基线，数值越大重叠度越高
                      quantile_lines = TRUE, #显示分位数线
                      quantiles = 2, #仅显示中位数线
                      na.rm=TRUE
  ) +labs(x = expression(paste("Rs"," ","(",g," ","C"," ", m^-2," ", "day"^-1,")")), y = 'Elevation')+scale_x_continuous(limits =c(-1,13))+
  scale_fill_manual(values = mycol1) +
  theme_classic()
p8<-p8 + annotate("text", x = 10 , y = 1.4,label = "n=8634",colour="black") + 
  annotate("text", x = 10 , y = 2.4,label = "n=3139",colour="black")+ 
  annotate("text", x = 10 , y = 3.4,label = "n=938",colour="black")+ 
  theme(panel.grid =   element_blank())
p8


#text(2, 10, expression(R^2==0.67),cex=1,font=2,col="#000000")
#text(2, 9, expression(P<0.001),cex=1,font=2,col="#000000")


#Randomly split the overall data into 70% for training, 30% for validating
set.seed (556)
train <- sample(2, nrow(RsForRandomForest_naOMIT), replace = TRUE, prob = c(0.7,0.3))
TrainSet <- RsForRandomForest_naOMIT[train==1,] #To train the RF model
ValidSet <- RsForRandomForest_naOMIT[train==2,] #To validate the RF model
#summary(TrainSet)
#summary(ValidSet)

#set.seed (12345)
#train <- createDataPartition(y = RsForRandomForest$Rs_Norm,
#                             p =0.7,
#                             list = F,
#                             times = 1)
#TrainSet <- RsForRandomForest[train,]
#Traindata <- RsForRandomForest[train,]
#ValidSet <- RsForRandomForest[-train,]


mean(ValidSet$Rs_Norm,na.rm = T)


#Split data into chambers from convergent areas 
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

TrainSetA <- subset(TrainSet, TopClimatetype == "A" )
TrainSetB <- subset(TrainSet, TopClimatetype == "B" )
TrainSetC <- subset(TrainSet, TopClimatetype == "C" )
TrainSetD <- subset(TrainSet, TopClimatetype == "D" )
TrainSetE <- subset(TrainSet, TopClimatetype == "E" )

ValidSetA <- subset(ValidSet, TopClimatetype == "A" )
ValidSetB <- subset(ValidSet, TopClimatetype == "B" )
ValidSetC <- subset(ValidSet, TopClimatetype == "C" )
ValidSetD <- subset(ValidSet, TopClimatetype == "D" )
ValidSetE <- subset(ValidSet, TopClimatetype == "E" )

TrainSetYQ <- subset(TrainSet, MYear == "Q" )
TrainSetYH <- subset(TrainSet, MYear == "H" )

ValidSetYQ <- subset(ValidSet, MYear == "Q" )
ValidSetYH <- subset(ValidSet, MYear == "H" )


TrainSetL <- subset(TrainSet, TrainSet$Elevation<500 )
TrainSetM <- subset(TrainSet, TrainSet$Elevation>=500 & TrainSet$Elevation<1500 )
TrainSetH <- subset(TrainSet, TrainSet$Elevation>1500 )

ValidSetL <- subset(ValidSet, ValidSet$Elevation<500 )
ValidSetM <- subset(ValidSet, ValidSet$Elevation>=500 & ValidSet$Elevation<1500 )
ValidSetH <- subset(ValidSet, ValidSet$Elevation>1500 )