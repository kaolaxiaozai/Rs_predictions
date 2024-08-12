library(terra)
library(tidyterra)
library(ggplot2)
library(readr)
#http://127.0.0.1:14667/graphics/plot_zoom_png?width=1200&height=900
#Rs<- read_csv("E:/data/14098967/Table 1 MGRsD.csv",locale=locale(encoding="GBK"))
MAT<-rast("E:/cdt/126-2080.tif")
library(sf)
library(ggspatial)
library(RColorBrewer)
library(ggtext)
library(hrbrthemes)

plot(MAT)
#快速绘制下
plot(MAT$"126-2080_1")

#MAT <- MAT[!is.na(MAT$"20101_1"), ]
p3<-ggplot() +
  geom_spatraster(data=MAT$"20101_1")+
  #修改配色，取消图例名称，并使NA值为透明
  scale_fill_gradient(low="#FFFFFF",high="#eb9a46",na.value = 'transparent')+
  #scale_fill_viridis_c(option = "A",direction = -1,name='MAT(℃)',na.value = 'transparent')+
  #取消拓展绘图边缘，否则y的坐标刻度不能显示
  coord_sf(expand = FALSE)+ 
  theme_minimal() + #geom_sf(data = coast)+ #更换主题
  theme(legend.position="none",
        legend.key.size = unit(0.4, "cm"),
        panel.background=element_blank(),
        panel.grid=element_blank(), 
        panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA))
 # )
p3
my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)

GIABC <- ggplot() + 
  geom_sf() + 
  geom_tile(data=MAT$"20101_1")+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)
  )

GIABC

head(MAT)
ggplot(MAT) +#geom_sf()+
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
ggplot(MAT, aes(Longitude, Latitude, color = predictions)) +#geom_sf()+
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

##安装包####
#install.packages("vivid") # 计算变量交互以及重要性图
#install.packages("randomForest") # 建立随机森林模型
#install.packages(c("network", "sna", "scales", "intergraph", "lemon")) # 制作网络图

#if (!requireNamespace("graph", quietly = TRUE)){install.packages("BiocManager")
 # BiocManager::install("graph")
#}
install.packages("zenplots")


###加载bao####

library(vivid)
library(randomForest)
library(MASS)

set.seed(123)
rf_model <- randomForest(medv ~., data = Boston)

set.seed(123)
VIVI_rf <- vivi(data = Boston,
                fit = rf_model,
                response = "medv",
                reorder = TRUE,
                normalized = FALSE,
                importanceType = "agnostic",
                gridSize = 50,
                nmax = 500,
                class = 1,
                predictFun = NULL,
                numPerm = 4)


viviHeatmap(mat = VIVI_rf)

viviHeatmap(mat = VIVI_rf,
            impPal = rev(colorspace::sequential_hcl(palette = "Reds 3", n = 100)))

viviNetwork(mat = VIVI_rf)

viviNetwork(mat = VIVI_rf, intThreshold = 0.2, removeNode = TRUE)

pdpVars(data = Boston,
        fit = rf_model,
        response = "medv",
        vars = colnames(VIVI_rf)[1:5],
        nIce = 80)

pdpPairs(data = Boston,
         fit = rf_model,
         response = "medv",
         nmax = 500,
         gridSize = 10,
         vars = colnames(VIVI_rf)[1:5],
         nIce = 80)

R<- read_csv("E:/cdt/ssp126-2060.csv"
                             ,locale=locale(encoding="GBK"))

my_colormap <- colorRampPalette(rev(brewer.pal(11,'Spectral')))(32)
GYHR <- ggplot() + 
  geom_sf() + 
  geom_tile(data = R,aes(Longitude, Latitude, fill = Rs))+
  scale_fill_gradientn(colours = my_colormap)+
  theme(
    panel.grid=element_blank(), 
    panel.background=element_blank(),
    panel.border = element_rect(linetype = "solid", linewidth = 1,fill = NA),
    #legend.position = c(0.11,0.3)，
  )+labs(colour = "Predicted Rs") 
GYHR<-GYHR+ annotate("text", x = -150 , y = -10,label= ("SSP126-2060"),size = 3)+ annotate("text", x = -150 , y = -19,label= ("Global Mean Rs"),size = 3)+ annotate("text", x = -150 , y = -28,label= expression(paste("1.95"," ",g," ","C"," ", m^-2," ", "day"^-1)),size = 3)

GYHR