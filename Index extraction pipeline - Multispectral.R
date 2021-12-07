###############################################################
# Raster on R - INDEX Extraction By Plot Through QGIS Shapefile
# Cleber Vinicius Giaretta Azevedo
###############################################################
library(raster)
library(sp)
library(tidyr)
library(FIELDimageR)
library(ggplot2)
library(readxl)
library(dplyr)
library(plyr)
###########################

rm(list=ls())
#importing mosaics
setwd("C:/Users/utente/Desktop/Ortho")
dir()

mosaic <- stack("03-03-21_seq.tif") #ok

#print(mosaic)
plot(mosaic) #plot all
plot(mosaic, 4) #plot only band 4 (NIR)
#plotRGB(mosaic) #plot composite image RGB
#click(mosaic) #plot composite image - pixel composition

# importing shapefiles (plots)
setwd("C:/Users/utente/Desktop/Shapefile/Stress")
plots <- shapefile("SFstress")
plot(plots, add = T, col = "Yellow")

#Getting specific info
#crs(mosaic) #reference system
#crs(plots) #reference system
#extent(plots) #limits
#length(plots) #number of features (plots)

#clipping raster by plots - a function
plot_clip <- function(ortho, shape){
  plot_raster <- list()
  for (i in 1:nrow(shape)) {
    p <- shape[i,]
    plot_raster[[i]] <- crop(x = ortho, y = extent(p))
    plot_raster[[i]] <- mask(plot_raster[[i]], p)
    names(plot_raster)[i] <- shape$ID[i]
  }
  return(plot_raster)
}

# clipping all plots - it takes a while, depending on the number of plots and raster size
rasterbyplots <- plot_clip(mosaic, plots) # Choose the mosaic

#length(rasterbyplots)
#plot(rasterbyplots[[14]])

# Removing soil using vegetation indices - via image segmentation - find a best index & threshold 
EX1 <- rasterbyplots[[14]] #low leaf
EX2  <- rasterbyplots[[34]] # high leaf
EX1.RemSoil<- fieldIndex(mosaic = EX1, Green = 1, Red = 2, RedEdge=3, NIR=4, 
                         index = c("NDVI"))
EX2.RemSoil<- fieldIndex(mosaic = EX2, Green = 1, Red = 2, RedEdge=3, NIR=4, 
                         index = c("NDVI"))


EX1.RemSoil <- fieldIndex(mosaic = EX1, Green = 1, Red = 2, RedEdge=3, NIR=4, 
                          index = c("NDRE", "PSRI", "NDVI", "GNDVI", "RVI", "TVI",
                                    "CVI", "CIG", "CIRE", "DVI", "SCI"), plot = T)
EX2.RemSoil <- fieldIndex(mosaic = EX2, Green = 1, Red = 2, RedEdge=3, NIR=4, 
                          index = c("NDRE", "PSRI", "NDVI", "GNDVI", "RVI", "TVI",
                                    "CVI", "CIG", "CIRE", "DVI", "SCI"), plot = T)
hist(EX1.RemSoil$GNDVI) # Image segmentation start (soil and plants) - choose best index and threshold 
hist(EX2.RemSoil$GNDVI)

EX1.RemSoil<- fieldMask(mosaic = EX1, Green = 1, Red = 2, RedEdge=3, NIR=4, 
                        index = "NDVI", cropValue = 0.5, cropAbove = T) 

EX2.RemSoil<- fieldMask(mosaic = EX2, Green = 1, Red = 2, RedEdge=3, NIR=4, 
                        index = "NDVI", cropValue = 0.4, cropAbove = F) 

EX1.RemSoil$newMosaic
plot(EX1.RemSoil$newMosaic)

EX1.RemSoil$mask
plot(EX1.RemSoil$mask)

# saving rasters or masks
#writeRaster(EX1.RemSoil$mask, "mascara", overwrite = TRUE)
#writeRaster(rasterbyplots[[344]], "parcela", overwrite = TRUE)
#dados <- list(parcela = stack("parcela"), mascara = stack("mascara"), shape = plots[344,])
#saveRDS(dados, "dados")

# applying for all plots
rbpws <- list()
masks <- list()
for (i in 1:length(rasterbyplots)) {
  cat("plot", i, " ")
  temp1 <- fieldMask(mosaic = rasterbyplots[[i]], Green = 1, Red = 2, RedEdge=3, NIR=4, index = "NDVI", cropValue = 0.5, cropAbove = T, plot = F)
  rbpws[[i]] <- temp1[[1]]
  masks[[i]] <- temp1$mask
}

length(rbpws)
print(masks[[14]])
plot(masks[[14]])
print(rbpws[[14]])
plot(rbpws[[14]])

# Getting  indices estimates
# micasense sensor (B, G, R, NIR, RE) 
# parrot sequoia sensor (G, R, RE, NIR) 

NDVI <- overlay(x = rbpws[[14]],
                 fun = function(Green, Red, RedEdge, NIR){
                   return(((NIR - Red)/(NIR + Red)))}) 
class(NDVI)
plot(as.raster(NDVI))
median(raster::as.matrix(NDVI), na.rm = T) # median for the index. Much better than mean

#APPLYING MULTISPECTRAL INDICES - add/remove the indexes as you wish - a library here would be nice, get in contact if you want to help with that.
#With you want no mask use:
#rbpws <- rasterbyplots

# applying for all plots - NDVI
NDVI <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(((NIR - Red)/(NIR + Red)))}) 
  NDVI <- rbind(NDVI, data.frame(
    FID = plots@data[i, "fid"],        
    NDVI = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - PSRI
PSRI <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(((Red - Green)/(RedEdge)))}) 
  PSRI <- rbind(PSRI, data.frame(
    FID = plots@data[i, "fid"],        
    PSRI = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - GNDVI
GNDVI <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(((NIR - Green)/(NIR + Green)))}) 
  GNDVI <- rbind(GNDVI, data.frame(
    FID = plots@data[i, "fid"],        
    GNDVI = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - NDRE
NDRE <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(((NIR - RedEdge)/(NIR + RedEdge)))}) 
  NDRE <- rbind(NDRE, data.frame(
    FID = plots@data[i, "fid"],        
    NDRE = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - TVI
TVI <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return((0.5*(120*(NIR-Green)-200*(Red-Green))))}) 
  TVI <- rbind(TVI, data.frame(
    FID = plots@data[i, "fid"],        
    TVI = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - CIRE
CIRE <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(((NIR / RedEdge)-1))}) 
  CIRE <- rbind(CIRE, data.frame(
    FID = plots@data[i, "fid"],        
    CIRE = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - Green
Green <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(Green)}) 
  Green <- rbind(Green, data.frame(
    FID = plots@data[i, "fid"],        
    Green = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - Red
Red <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(Red)}) 
  Red <- rbind(Red, data.frame(
    FID = plots@data[i, "fid"],        
    Red = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - RedEdge
RedEdge <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(RedEdge)}) 
  RedEdge <- rbind(RedEdge, data.frame(
    FID = plots@data[i, "fid"],        
    RedEdge = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - NIR
NIR <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(NIR)}) 
  NIR <- rbind(NIR, data.frame(
    FID = plots@data[i, "fid"],        
    NIR = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - MCARI1
MCARI1 <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return(1.2*((2.5*(NIR-Red))-(1.3*(NIR-Green))))}) 
  MCARI1 <- rbind(MCARI1, data.frame(
    FID = plots@data[i, "fid"],        
    MCARI1 = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

# applying for all plots - MCARI2
MCARI2 <- data.frame()
for (i in 1:length(rbpws)) {
  cat("plot", i, "fid")
  idx.plot = overlay(x = rbpws[[i]],
                     fun = function(Green, Red, RedEdge, NIR){
                       return((1.2*(2.5*(NIR-Red)-1.3*(NIR-Green))/sqrt((2*NIR+1)^2-(6*NIR-5*sqrt(Red))-0.5)))}) 
  MCARI2 <- rbind(MCARI2, data.frame(
    FID = plots@data[i, "fid"],        
    MCARI2 = median(raster::as.matrix(idx.plot), na.rm = T)
  ))  
}

## Merging Shapefile informations
setwd("C:/Users/utente/Desktop/Shapefile/Stress")
dir()
results <- read_excel("ShapeFile_input.xlsx")
Index <- cbind(Green["Green"],Red["Red"],RedEdge["RedEdge"],NIR["NIR"],NDRE["NDRE"], 
               PSRI["PSRI"], NDVI["NDVI"], GNDVI["GNDVI"], TVI["TVI"], CIRE["CIRE"],MCARI1["MCARI1"], MCARI2["MCARI2"])
results <- bind_cols(results,Index)
head(results)
tail(results)

## Write Excel Table
library(openxlsx)
setwd("C:/Users/utente/Desktop/Results/")
write.xlsx(results, "Index 26-03-21.xlsx", sheetName = "Seq 26-03", 
           col.names = TRUE, row.names = F, append = FALSE)

### Separating Experiments
Solace <- filter(results, Exp == "1")
LxR <-  filter(results, Exp == "2")
Duro <- filter(results, Exp == "3")

head(LxR)
### Saving output files - CSV
#write.csv(results,file = "Index 26-04-21.csv",row.names = F)
#write.csv(Solace,file = "Solace Index 26-04-21.csv",row.names = F)
#write.csv(LxR,file = "LxR Index 26-04-21.csv",row.names = F)
#write.csv(Duro,file = "Duro Index 26-04-21.csv",row.names = F)
# Data.EX1.Info<-read.csv("EX1.Info.csv",header = T,check.names = F) # Reading the saved data table.
getwd()

### BOXPLOT
## All
All <- as.data.frame(results) %>% filter(Exp < 4) %>%
  pivot_longer(cols = 12:23, names_to = "Index", values_to = "Index Value")
head(All)
tail(All)

# Rename the column and the values in the factor
All$Exp <- as.factor(All$Exp)
levels(All$Exp)[levels(All$Exp)==1] <- "Solace"
levels(All$Exp)[levels(All$Exp)==2] <- "LxR"
levels(All$Exp)[levels(All$Exp)==3] <- "Duro"

# Box Plot All
ggplot(All, aes(x=factor(Exp), y=`Index Value`, fill=Trat)) +
  geom_boxplot() + 
  facet_wrap(~Index, scales = "free") + 
  labs(title = "Vegetative Index", x="Experiment", y="Index Value") +
  theme_minimal()

# Violin All
ggplot(All, aes(x=factor(Exp), y=`Index Value`)) + 
  geom_violin(aes(fill = Trat), trim = FALSE, position = position_dodge(0.7), width = 0.7 ) +
  geom_boxplot(aes(fill = Trat), width = 0.1, position = position_dodge(0.7)) +
  scale_fill_manual(values = c("turquoise", "orangered"))+
  facet_wrap(~Index, scales = "free") +
  theme_minimal()

# Basic box plot
boxplot(NDVI, main="NDVI 26-04-2021")

# Boxplot by treat
require(ggpubr)
ggboxplot(results, x = "Trat", y = "NDVI", width = 0.5, fill = "Trat", Title = "NDVI")

ggboxplot(results, "Trat", "NDVI",
          color = "Trat", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
          add = "jitter", shape = "Trat")

# Violin Plot
ggplot(results, aes(x=Trat, y=NDVI, fill=Trat)) + 
  geom_violin()+
  geom_boxplot(width=0.1) + 
  stat_summary(fun.data=mean_sdl, geom="pointrange", color="black") +
  theme_minimal()

# Histogram 
ggplot(output, aes(x=index)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="gray", bins = 50)+ 
  geom_density(alpha=.2, fill="blue") + stat_function(fun=function(x)
  dnorm(x, mean=mean(output$index, na.rm=T), sd=sd(output$index,na.rm=T)), colour="red") +
  labs(x="NDVI", y="Density", title = "NDVI 26-04-2021") +
  theme_minimal()

# Histogram by Treat
ggplot(results, aes(x=NDVI, color=Trat)) +
  geom_histogram(fill="gray")

ggplot(results, aes(x=NDVI, color=Trat)) +
  geom_histogram(fill="gray", alpha=0.5,  position="identity") +
  theme_minimal()

# Interleaved histograms with mean line
library(plyr)
mu <- ddply(results, "Trat", summarise, grp.mean=mean(NDVI))

ggplot(results, aes(x=NDVI, color=Trat)) +
  geom_histogram(fill="gray", position="dodge") +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Trat),
             linetype="dashed")+
  theme_minimal()

#With density curve
ggplot(results, aes(x=NDVI, color=Trat)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, bins = 50)+
  geom_density(alpha=0.6)+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Trat),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="NDVI Histogram",x="NDVI", y = "Density")+
  theme_minimal()
