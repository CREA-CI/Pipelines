###############################
# Plant Height via cloud points
###############################
library(data.table)
library(rlas)
library(lidR)
library(dplyr)
#####################

rm(list=ls())
#memory.limit()
## Data setup
setwd("C:\\Users\\utente\\Desktop\\Plant Height")
dir()
#importing cloud points
canopy <- fread("DC-07-06-2021.txt") 
colnames(canopy) <- c("X","Y", "Z")
head(canopy)

ground <- fread("DC-17-12-2020.txt") 
colnames(ground) <- c("X","Y", "Z")
head(ground)

# creating LAS files
write.las(file = (file.path(getwd(), "canopy.07-06-21.laz")), header = header_create(canopy), data = canopy)
write.las(file = (file.path(getwd(), "ground.17-12-20.laz")), header = header_create(ground), data = ground)

# loading the LAS files
las.canopy <- readLAS(files = "canopy.07-06-21.laz", select = "xyz")
#las.ground <- readLAS(files = "ground.17-12-20.laz", select = "xyz")
#plot(las.canopy)
#plot(las.ground)

# importing shapefiles (plots)
setwd("C:/Users/utente/Desktop/Shapefile/Stress")
plots <- shapefile("SFstress")
#print(plots)
#plot(plots, 
#     add = T, 
#     col = "Red")


## Auxiliary function to clip the point cloud using the QGIS shapefile
cloud.clip <- function(cloud, shape){
  pc <- list()
  for(i in 1:nrow(shape)){
    p <- shape[i,]
    c <- clip_rectangle(cloud, xleft = p@bbox['x','min'], ytop = p@bbox['y','max'], 
                        xright = p@bbox['x','max'], ybottom = p@bbox['y','min'])
    if(!is.null(c)){
      pc[[i]] <- c
      names(pc)[i] <- paste0(shape$fid[i])}}
  pc <- pc[!unlist(lapply(pc, is.null))]
  return(pc)
}

#cloud clip  by plot - first canopy as it is too heavy to work with canopy and ground together
setwd("C:\\Users\\utente\\Desktop\\Plant Height")
canopy.plot <- cloud.clip(las.canopy, plots) 
save(canopy.plot, file="Canopy.PC.by.plot.Rdata")

# visualising
#plot(canopy.plot$`1`@data$Y, canopy.plot$`1`@data$X) #nadir seeing 
#plot(canopy.plot$`1`@data$Y, canopy.plot$`1`@data$Z) #vista frontal
#plot(canopy.plot$`1`@data$X, canopy.plot$`1`@data$Z) #vista lateral
#abline(h = canopy.plot$`1`, col = "Blue")


#applying percentile to canopy height
canopy.99 <- lapply(canopy.plot, function(x) { quantile(x@data$Z, .99) }) #99th percentile
canopy.99$`1`
save(canopy.99, file="canopy.99.Rdata")

canopy.95 <- lapply(canopy.plot, function(x) { quantile(x@data$Z, .95) }) #95th percentile
save(canopy.95, file="canopy.95.Rdata")

canopy.90 <- lapply(canopy.plot, function(x) { quantile(x@data$Z, .90) }) #90th percentile
save(canopy.90, file="canopy.90.Rdata")

#releasing RAM
rm(canopy.plot,las.canopy)

#now cloud clip  by plot soil
las.ground <- readLAS(files = "ground.17-12-20.laz", select = "xyz")
ground.plot <- cloud.clip(las.ground, plots)
save(ground.plot, file="Ground.PC.by.plot.Rdata")

#applying percentile to ground height
ground.10 <- lapply(ground.plot, function(x) { quantile(x@data$Z, .1) }) #10th percentile
save(ground.10, file="ground.10.Rdata")

ground.05 <- lapply(ground.plot, function(x) { quantile(x@data$Z, .05) }) #05th percentile
save(ground.05, file="ground.05.Rdata")

ground.01 <- lapply(ground.plot, function(x) { quantile(x@data$Z, .01) }) #01st percentile
save(ground.01, file="ground.01.Rdata")

#plot(ground.plot$`1`@data$Y, ground.plot$`1`@data$Z) # lateral
#abline(h = ground.plot`1`, col = "Red")

#releasing RAM
rm(ground.plot,las.ground,plots)

#loading canopy
dir()
load("ground.01.Rdata")
load("ground.05.Rdata")
load("ground.10.Rdata")
load("canopy.99.Rdata")
load("canopy.95.Rdata")
load("canopy.90.Rdata")

#estimating plant height for all plots
# 99 - 01
PH99.01 <- data.frame()

for (i in 1:length(canopy.99)) {
  cat("plot", i, " ")
  PH99.01 <- rbind(PH99.01, data.frame(
    fid = names(canopy.99)[[i]],        
    Height99.01 = round(as.numeric(canopy.99[[i]]) - as.numeric(ground.01[[i]]), 2)
  ))  
}

head(PH99.01)

# 99 - 05
PH99.05 <- data.frame()

for (i in 1:length(canopy.99)) {
  cat("plot", i, " ")
  PH99.05 <- rbind(PH99.05, data.frame(
    fid = names(canopy.99)[[i]],        
    Height99.05 = round(as.numeric(canopy.99[[i]]) - as.numeric(ground.05[[i]]), 2)
  ))  
}

# 99 - 10
PH99.10 <- data.frame()

for (i in 1:length(canopy.99)) {
  cat("plot", i, " ")
  PH99.10 <- rbind(PH99.10, data.frame(
    fid = names(canopy.99)[[i]],        
    Height99.10 = round(as.numeric(canopy.99[[i]]) - as.numeric(ground.10[[i]]), 2)
  ))  
}

# 95 - 01
PH95.01 <- data.frame()

for (i in 1:length(canopy.95)) {
  cat("plot", i, " ")
  PH95.01 <- rbind(PH95.01, data.frame(
    fid = names(canopy.95)[[i]],        
    Height95.01 = round(as.numeric(canopy.95[[i]]) - as.numeric(ground.01[[i]]), 2)
  ))  
}

head(PH95.01)

# 95 - 05
PH95.05 <- data.frame()

for (i in 1:length(canopy.95)) {
  cat("plot", i, " ")
  PH95.05 <- rbind(PH95.05, data.frame(
    fid = names(canopy.95)[[i]],        
    Height95.05 = round(as.numeric(canopy.95[[i]]) - as.numeric(ground.05[[i]]), 2)
  ))  
}

# 95 - 10
PH95.10 <- data.frame()

for (i in 1:length(canopy.95)) {
  cat("plot", i, " ")
  PH95.10 <- rbind(PH95.10, data.frame(
    fid = names(canopy.95)[[i]],        
    Height95.10 = round(as.numeric(canopy.95[[i]]) - as.numeric(ground.10[[i]]), 2)
  ))  
}

# 90 - 01
PH90.01 <- data.frame()

for (i in 1:length(canopy.90)) {
  cat("plot", i, " ")
  PH90.01 <- rbind(PH90.01, data.frame(
    fid = names(canopy.90)[[i]],        
    Height90.01 = round(as.numeric(canopy.90[[i]]) - as.numeric(ground.01[[i]]), 2)
  ))  
}

head(PH90.01)

# 90 - 05
PH90.05 <- data.frame()

for (i in 1:length(canopy.90)) {
  cat("plot", i, " ")
  PH90.05 <- rbind(PH90.05, data.frame(
    fid = names(canopy.90)[[i]],        
    Height90.05 = round(as.numeric(canopy.90[[i]]) - as.numeric(ground.05[[i]]), 2)
  ))  
}

# 90 - 10
PH90.10 <- data.frame()

for (i in 1:length(canopy.90)) {
  cat("plot", i, " ")
  PH90.10 <- rbind(PH90.10, data.frame(
    fid = names(canopy.90)[[i]],        
    Height90.10 = round(as.numeric(canopy.90[[i]]) - as.numeric(ground.10[[i]]), 2)
  ))  
}

#Quantile PH estimation done, now lets organize
#Get Shapefile informations
require(readxl)
template <- read_excel("ShapeFile_input.xlsx")

#check order
template <- template[order(template$fids),]
head(template)

#get PHs together
PH <- cbind(template,
        PH99.01$Height99.01,PH99.05$Height99.05,PH99.10$Height99.10,
        PH95.01$Height95.01,PH95.05$Height95.05,PH95.10$Height95.10,
        PH90.01$Height90.01,PH90.05$Height90.05,PH90.10$Height90.10)

colnames(PH) <- c("Exp","fids","planter","plot","Row","Col","bloc","entry","Trat",
                  "name","code","99.01","99.05","99.10","95.01","95.05","95.10",
                  "90.01", "90.05", "90.10")
head(PH)

#data for pheno obs has been compiled, now let's correct the Plant Height Estimation
tidyEPH <- select(PH, Exp, plot, Trat, name, "99.01","99.05","99.10","95.01","95.05",
                  "95.10","90.01","90.05","90.10")
names(PH)
names(tidyEPH)

colnames(tidyEPH) <- c("Experiment", "Plot", "Treatment", "Name","99.01","99.05",
                       "99.10","95.01","95.05","95.10","90.01","90.05","90.10" )
#names(tidyAllspectra) [1:12] <- c("Experiment", "Plot", "Treatment", "Name", )
#str(tidyEPH)
tidyEPH$Experiment <- as.factor(tidyEPH$Experiment)

#correct the levels
tidyEPH$Treatment <- recode_factor(tidyEPH$Treatment, Irrigated = "Irr", Str = "No_irr")
levels(tidyEPH$Treatment)
tidyEPH$Experiment <- recode_factor(tidyEPH$Experiment, "1" = "SOLACE", "2" = "LXR", "3"="DURO", "99"="UNMARKED")

#levels(tidyEPH$Treatment)
#levels(tidyEPH$Experiment)

#PH estimation has been compiled, now let's correct the measured Plant Height
#combine together into one data set of pheno values
#Get measured PH
SOLACEphenodata <- read_excel("SOLACE 2021 Stress.xlsx")
LXR_irrigated_phenodata <- read_excel("LXR_RIL_2020-2021.xlsx", sheet = "Irrigated")
LXR_drought_phenodata <- read_excel("LXR_RIL_2020-2021.xlsx", sheet = "Rainfed")
Duro_phenodata <- read_excel("Duro Stress.xlsx")

#get selected columns
tidySOLACEphenodata <-select(SOLACEphenodata, plot...3, treat, Height, name)
names(tidySOLACEphenodata) [1:4] <- c("Plot", "Treatment", "Height", "Name")

tidyLXR_irrigated_phenodata <-select(LXR_irrigated_phenodata, Plot, treat, Height, name)
names(tidyLXR_irrigated_phenodata) [1:4] <- c("Plot", "Treatment", "Height", "Name")

tidyLXR_drought_phenodata <-select(LXR_drought_phenodata, Plot, treat, Height, name)
names(tidyLXR_drought_phenodata) [1:4] <- c("Plot", "Treatment", "Height", "Name")

tidyDuro_phenodata <-select(Duro_phenodata, plot, treat, "PH-GS92", name)
names(tidyDuro_phenodata) [1:4] <- c("Plot", "Treatment", "Height", "Name")

#add a column for the experiment in each
tidySOLACEphenodata <- tidySOLACEphenodata %>% mutate(Experiment="SOLACE")
tidyLXR_drought_phenodata <- tidyLXR_drought_phenodata %>% mutate(Experiment="LXR")
tidyLXR_irrigated_phenodata <- tidyLXR_irrigated_phenodata %>% mutate(Experiment="LXR")
tidyDuro_phenodata <- tidyDuro_phenodata %>% mutate(Experiment="DURO")

# make sure correct structure so they can bind together
tidySOLACEphenodata$Height <- as.numeric(tidySOLACEphenodata$Height)
tidyLXR_drought_phenodata$Height <- as.numeric(tidyLXR_drought_phenodata$Height)
tidyLXR_irrigated_phenodata$Height <- as.numeric(tidyLXR_irrigated_phenodata$Height)
tidyDuro_phenodata$Height <- as.numeric(tidyDuro_phenodata$Height)
#LxR have some plots with 2 heights, leave NA for now

#Combine together into one data set of pheno values

# first we need a function
getPhenodata <- function(tidySOLACEphenodata, tidyLXR_drought_phenodata, tidyLXR_irrigated_phenodata, tidyDuro_phenodata) {
  
  phenodata <- bind_rows(tidySOLACEphenodata, tidyLXR_drought_phenodata, tidyLXR_irrigated_phenodata, tidyDuro_phenodata)
  
  #check correct format 
  str(phenodata)
  phenodata$Treatment <- factor(phenodata$Treatment)
  levels(phenodata$Treatment)
  
  #correct the levels
  
  phenodata$Treatment <- recode_factor(phenodata$Treatment, irr = "Irr")
  phenodata$Treatment <- recode_factor(phenodata$Treatment, "no-irr" = "No_irr")
  phenodata$Treatment <- recode_factor(phenodata$Treatment, no_irr = "No_irr")
  levels(phenodata$Treatment)
  return(phenodata)
}


fieldHeight <- getPhenodata(tidySOLACEphenodata, tidyLXR_drought_phenodata, tidyLXR_irrigated_phenodata, tidyDuro_phenodata)
fieldHeight$Experiment <- as.factor(fieldHeight$Experiment)

levels(tidyEPH$Experiment)
levels(fieldHeight$Experiment)
levels(tidyEPH$Treatment)
levels(fieldHeight$Treatment)

fieldHeight$Name <- as.factor(fieldHeight$Name)
str(tidyEPH)
str(fieldHeight)

#Merging the whole data
CompleteData <- full_join(tidyEPH,fieldHeight, by=c("Experiment", "Treatment", "Plot", "Name")) %>% filter(Experiment != "UNMARKED")

#Rescale to Height to cm
CompleteData$Height <- CompleteData$Height/100


##data exploration##
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
PHcorrdata <- CompleteData[,c(5:14)]
chart.Correlation(PHcorrdata, histogram=TRUE, pch=19)

names(CompleteData)
ggplot(CompleteData, aes(x = `99.01`, y = Height)) +
  geom_point() +
  stat_smooth(method=lm)

model <- lm(Height ~ `99.01`, data = CompleteData)
model
summary(model)
confint(model)
sigma(model)*100/mean(CompleteData$Height,na.rm=T)

write.csv(CompleteData, "PlantHeight.csv")
getwd()
dir()
