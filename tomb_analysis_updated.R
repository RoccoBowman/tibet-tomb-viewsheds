### Replication Script for Viewsheds of Cubic Mountaintop Tombs in Upper Tibet (Zhangzhung) by Rocco Bowman and Karl Ryavec ###
### Contact Rocco @ rbowman2@ucmerced.edu for any questions ###

### Set Work Drive ###

wd <- "C:/Users/bowma/Documents/Tibet Tombs Article/Test/"
setwd(wd)

## Renaming final viewsheds ##
library(stringr)

path = paste(wd,'FinalClips/',sep = "")

rasters <- list.files(path = path, recursive = TRUE, full.names = TRUE)

for (r in rasters) {
  print(paste0('Grabbing ',r))
  id <- basename(r)
  id <- gsub(".tif$", "", id)
  trim <- str_sub(id, 28, -5)
  new_name <- paste0(dirname(r), "/", trim, "_viewshed", ".tif")
  print(paste0('Renaming ',r,'to','new_name'))
  file.rename(r, new_name)
}

## Figure 5 ##

library(sf)
library(raster)
library(rgdal)
library(ggspatial)
library(viridis)
library(ggpubr)
library(ggplot2)
library(plyr)
library(tidyverse)


tombpoints <- st_read("Data/tombs_albers.shp")
tombdata <- read.csv("Tabular/merge_30m.csv")

## Viewshed Map 1 ##

viewshed <- raster("FinalClips/Yitse Khar_viewshed.tif")
viewsheddf <- as.data.frame(viewshed, xy = TRUE)
viewsheddf[viewsheddf==0] <- NA
viewsheddf <- na.omit(viewsheddf)
colnames <- colnames(viewsheddf)
index <- colnames[3]
viewsheddf <- viewsheddf %>% rename(vi = index)

point <- tombpoints[tombpoints$Name=='Yitse Khar',]
thistomb <- tombdata[tombdata$Name=='Yitse Khar',]
tombvi <- thistomb$zonal_mean

map1 <-ggplot() +
  geom_raster(data = viewsheddf , aes(x = x, y = y, fill = vi)) +
  scale_fill_viridis_c()+
  geom_sf(data = point, size = 2, color = "red") +
  #geom_label(data=point, x = point$xcoord+1200, y = point$ycoord+700, label = point$Name) +
  ggtitle(point$Name)
map1 <- map1 + theme_void() + theme(legend.position = "none",  plot.title = element_text(size = 12))

histvi <- ggplot(viewsheddf, aes(vi)) + geom_histogram(bins = 50, color = NA, fill = viridis(50))
max <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax),1000)
mid <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax)/2,1000)
# y_coord <- max - 1000
# x_coord <- tombvi + 0.01

plot1 <- ggplot(viewsheddf, aes(vi)) + 
  geom_histogram(bins = 50, color = NA, fill = viridis(50))+
  labs(x ="Visibility Index", y = "Count") +
  geom_vline(xintercept = tombvi,
             color = "red", size=0.5) +
  # annotate(geom="text", x=x_coord, y=y_coord, label="Mean Tomb VI",
  #          color="red") +
  coord_flip()

plot1 <- plot1 + theme_classic() + scale_y_continuous(breaks=c(0,mid,max))


## Viewshed Map 2 ##

viewshed <- raster("FinalClips/Tashi Darlung_viewshed.tif")
viewsheddf <- as.data.frame(viewshed, xy = TRUE)
viewsheddf[viewsheddf==0] <- NA
viewsheddf <- na.omit(viewsheddf)
colnames <- colnames(viewsheddf)
index <- colnames[3]
viewsheddf <- viewsheddf %>% rename(vi = index)

point <- tombpoints[tombpoints$Name=='Tashi Darlung',]
thistomb <- tombdata[tombdata$Name=='Tashi Darlung',]
tombvi <- thistomb$zonal_mean

map2 <-ggplot() +
  geom_raster(data = viewsheddf , aes(x = x, y = y, fill = vi)) +
  scale_fill_viridis_c()+
  geom_sf(data = point, size = 2, color = "red") +
  #geom_label(data=point, x = point$xcoord+1200, y = point$ycoord+700, label = point$Name) +
  ggtitle(point$Name)
map2 <- map2 + theme_void() + theme(legend.position = "none",  plot.title = element_text(size = 12))

histvi <- ggplot(viewsheddf, aes(vi)) + geom_histogram(bins = 50, color = NA, fill = viridis(50))
max <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax),1000)
mid <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax)/2,1000)
# y_coord <- max - 1000
# x_coord <- tombvi + 0.01

plot2 <- ggplot(viewsheddf, aes(vi)) + 
  geom_histogram(bins = 50, color = NA, fill = viridis(50))+
  labs(x ="Visibility Index", y = "Count")+
  geom_vline(xintercept = tombvi,
             color = "red", size=0.5)+
  # annotate(geom="text", x=x_coord, y=y_coord, label="Mean Tomb VI",
  #          color="red")+
  coord_flip()
plot2 <- plot2 + theme_classic() + scale_y_continuous(breaks=c(0,mid,max))

## Viewshed Map 3 ##

viewshed <- raster("FinalClips/Ri Raser Mondur_viewshed.tif")
viewsheddf <- as.data.frame(viewshed, xy = TRUE)
viewsheddf[viewsheddf==0] <- NA
viewsheddf <- na.omit(viewsheddf)
colnames <- colnames(viewsheddf)
index <- colnames[3]
viewsheddf <- viewsheddf %>% rename(vi = index)

point <- tombpoints[tombpoints$Name=='Ri Raser Mondur',]
thistomb <- tombdata[tombdata$Name=='Ri Raser Mondur',]
tombvi <- thistomb$zonal_mean

map3 <-ggplot() +
  geom_raster(data = viewsheddf , aes(x = x, y = y, fill = vi)) +
  scale_fill_viridis_c()+
  geom_sf(data = point, size = 2, color = "red") +
  #geom_label(data=point, x = point$xcoord+1200, y = point$ycoord+700, label = point$Name) +
  ggtitle(point$Name)
map3 <- map3 + theme_void() + theme(legend.position = "none",  plot.title = element_text(size = 12))

histvi <- ggplot(viewsheddf, aes(vi)) + geom_histogram(bins = 50, color = NA, fill = viridis(50))
max <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax),1000)
mid <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax)/2,1000)
# y_coord <- max - 1000
# x_coord <- tombvi + 0.01

plot3 <- ggplot(viewsheddf, aes(vi)) + 
  geom_histogram(bins = 50, color = NA, fill = viridis(50))+
  labs(x ="Visibility Index", y = "Count")+
  geom_vline(xintercept = tombvi,
             color = "red", size=0.5)+
  # annotate(geom="text", x=x_coord, y=y_coord, label="Mean Tomb VI",
  #          color="red")+
  coord_flip()
plot3 <- plot3 + theme_classic() + scale_y_continuous(breaks=c(0,mid,max))

## Viewshed Map 4 ##

viewshed <- raster("FinalClips/Gelha Gokar_viewshed.tif")
viewsheddf <- as.data.frame(viewshed, xy = TRUE)
viewsheddf[viewsheddf==0] <- NA
viewsheddf <- na.omit(viewsheddf)
colnames <- colnames(viewsheddf)
index <- colnames[3]
viewsheddf <- viewsheddf %>% rename(vi = index)

point <- tombpoints[tombpoints$Name=='Gelha Gokar',]
thistomb <- tombdata[tombdata$Name=='Gelha Gokar',]
tombvi <- thistomb$zonal_mean

map4 <-ggplot() +
  geom_raster(data = viewsheddf , aes(x = x, y = y, fill = vi)) +
  scale_fill_viridis_c()+
  geom_sf(data = point, size = 2, color = "red") +
  #geom_label(data=point, x = point$xcoord+1200, y = point$ycoord+700, label = point$Name) +
  ggtitle(point$Name)
map4 <- map4 + theme_void() + theme(legend.position = "none",  plot.title = element_text(size = 12))

histvi <- ggplot(viewsheddf, aes(vi)) + geom_histogram(bins = 50, color = NA, fill = viridis(50))
max <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax),1000)
mid <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax)/2,1000)
# y_coord <- max - 1000
# x_coord <- tombvi + 0.01

plot4 <- ggplot(viewsheddf, aes(vi)) + 
  geom_histogram(bins = 50, color = NA, fill = viridis(50))+
  labs(x ="Visibility Index", y = "Count")+
  geom_vline(xintercept = tombvi,
             color = "red", size=0.5)+
  # annotate(geom="text", x=x_coord, y=y_coord, label="Mean Tomb VI",
  #          color="red")+
  coord_flip()
plot4 <- plot4 + theme_classic() + scale_y_continuous(breaks=c(0,mid,max))

## Viewshed Map 5 ##

viewshed <- raster("FinalClips/Pakthuk_viewshed.tif")
viewsheddf <- as.data.frame(viewshed, xy = TRUE)
viewsheddf[viewsheddf==0] <- NA
viewsheddf <- na.omit(viewsheddf)
colnames <- colnames(viewsheddf)
index <- colnames[3]
viewsheddf <- viewsheddf %>% rename(vi = index)

point <- tombpoints[tombpoints$Name=='Pakthuk',]
thistomb <- tombdata[tombdata$Name=='Pakthuk',]
tombvi <- thistomb$zonal_mean

map5 <-ggplot() +
  geom_raster(data = viewsheddf , aes(x = x, y = y, fill = vi)) +
  scale_fill_viridis_c()+
  geom_sf(data = point, size = 2, color = "red")+
  #geom_label(data=point, x = point$xcoord+1200, y = point$ycoord+700, label = point$Name) +
  ggtitle(thistomb$Name)
map5 <- map5 + theme_void() + theme(legend.position = "none",  plot.title = element_text(size = 12))

histvi <- ggplot(viewsheddf, aes(vi)) + geom_histogram(bins = 50, color = NA, fill = viridis(50))
max <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax),1000)
mid <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax)/2,1000)
# y_coord <- max - 1000
# x_coord <- tombvi + 0.01

plot5 <- ggplot(viewsheddf, aes(vi)) + 
  geom_histogram(bins = 50, color = NA, fill = viridis(50))+
  labs(x ="Visibility Index", y = "Count")+
  geom_vline(xintercept = tombvi,
             color = "red", size=0.5)+
  # annotate(geom="text", x=x_coord, y=y_coord, label="Mean Tomb VI",
  #          color="red")+
  coord_flip()

plot5 <- plot5 + theme_classic() + scale_y_continuous(breaks=c(0,mid,max))


## Viewshed Map 6 ##

viewshed <- raster("FinalClips/Dokralum Mondur_viewshed.tif")
viewsheddf <- as.data.frame(viewshed, xy = TRUE)
viewsheddf[viewsheddf==0] <- NA
viewsheddf <- na.omit(viewsheddf)
colnames <- colnames(viewsheddf)
index <- colnames[3]
viewsheddf <- viewsheddf %>% rename(vi = index)

point <- tombpoints[tombpoints$Name=='Dokralum Mondur',]
thistomb <- tombdata[tombdata$Name=='Dokralum Mondur',]
tombvi <- thistomb$zonal_mean

map6 <-ggplot() +
  geom_raster(data = viewsheddf , aes(x = x, y = y, fill = vi)) +
  scale_fill_viridis_c()+
  geom_sf(data = point, size = 2, color = "red") +
  #geom_label(data=point, x = point$xcoord+1200, y = point$ycoord+700, label = point$Name) +
  ggtitle(point$Name)
map6 <- map6 + theme_void() + theme(legend.position = "none",  plot.title = element_text(size = 12))

histvi <- ggplot(viewsheddf, aes(vi)) + geom_histogram(bins = 50, color = NA, fill = viridis(50))
max <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax),1000)
mid <- round_any(max(ggplot_build(histvi)$data[[1]]$ymax)/2,1000)
# y_coord <- max - 1000
# x_coord <- tombvi + 0.01

plot6 <- ggplot(viewsheddf, aes(vi)) + 
  geom_histogram(bins = 50, color = NA, fill = viridis(50))+
  labs(x ="Visibility Index", y = "Count")+
  geom_vline(xintercept = tombvi,
             color = "red", size=0.5)+
  # annotate(geom="text", x=x_coord, y=y_coord, label="Mean Tomb VI",
  #          color="red")+
  coord_flip()

plot6 <- plot6 + theme_classic() + scale_y_continuous(breaks=c(0,mid,max))

figure5 <- ggarrange(map1,plot1,map2,plot2,map3,plot3,map4,plot4,map5,plot5,map6,plot6,
                     ncol = 4, nrow = 3,  align = "hv")

#widths = c(1,0.75)) +
 # theme(plot.margin = margin(1,1,1,1, "cm"))

#Creating Data Frame Graphic

library(sjPlot)
library(dplyr)

backgrounddata <- read.csv("Tabular/merge_10k.csv")
tombdata <- read.csv("Tabular/merge_30m.csv")

graphicdf <- data.frame(cbind(tombdata$Name, tombdata$zonal_mean, backgrounddata$zonal_mean))
graphicdf$X2 <- as.numeric(graphicdf$X2)
graphicdf$X3 <- as.numeric(graphicdf$X3)

graphicdf$Difference <- (graphicdf$X2 - graphicdf$X3)
graphicdf$X2 <- round(graphicdf$X2, 3)
graphicdf$X3 <- round(graphicdf$X3, 3)
graphicdf$Difference <- round(graphicdf$Difference, 3)
colnames(graphicdf) = c("Tomb", "Tomb VI", "Background VI","Differential")
graphicdf <- arrange(graphicdf, -Differential)

tab_df(graphicdf, alternate.rows = TRUE, digits = 3)

## K-S Test
library(rstatix)
#Preparing data for KS test

tombdata <- read.csv("Tabular/merge_30m.csv")
totaldata <- read.csv("Tabular/merge_10k.csv")
tombsample <- tombdata$zonal_mean
totalsample <- totaldata$zonal_mean
group <- c(rep("tombsample", length(tombsample)), rep("totalsample", length(totalsample)))
ksdata <- data.frame(KSD = c(tombsample,totalsample), group = group)

cdf1 <- ecdf(tombsample) 
cdf2 <- ecdf(totalsample) 

test <- ks.test(tombsample,totalsample, alternative = 'two.sided')

minMax <- seq(min(tombsample, totalsample), max(tombsample, totalsample), length.out=length(tombsample)) 
x0 <- minMax[which(abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
y0 <- cdf1(x0) 
y1 <- cdf2(x0) 

#Plotting KS results
tiff("kstest.tiff", width = 800, height = 800)
plot(cdf1, verticals=TRUE, do.points=FALSE, col="red", main = "", xlab = "Visibility Index")
plot(cdf2, verticals=TRUE, do.points=FALSE, col="black", add=TRUE) 
points(c(x0, x0), c(y0, y1), pch=16, col="red") 
segments(x0, y0, x0, y1, col="red", lty="dotted") 
legend(0.25,0.2, 
       legend = c("Background", "Tomb Samples"),lty=1, col=c("black","red"))

dev.off()

# Writing out data

write.csv(ksdata, file = 'Kolmogorov-Smirnov Prepared Data.csv', row.names = FALSE)

ggsave("figure5.png", plot = figure5, device = "png", width = 8, height = 8, units = "in", dpi = 300)

write.csv(graphicdf,"table2.csv")


eight <- read.csv("C:/Users/bowma/Documents/Tibet Tombs Article/Test/8 Sightlines/merge_30m.csv")
eight <- select(eight,10) %>%
  mutate(sample = 8)
eight <- arrange(eight,zonal_mean)

sixteen <- read.csv("C:/Users/bowma/Documents/Tibet Tombs Article/Test/16 Sightlines/merge_30m.csv")
sixteen <- select(sixteen,10) %>%
  mutate(sample = 16)
sixteen <- arrange(sixteen,zonal_mean)

thirtytwo <- read.csv("C:/Users/bowma/Documents/Tibet Tombs Article/Test/32 Sightlines/merge_30m.csv")
thirtytwo <- select(thirtytwo,10) %>%
  mutate(sample = 32)
thirtytwo <- arrange(thirtytwo,zonal_mean)

sixtyfour <- read.csv("C:/Users/bowma/Documents/Tibet Tombs Article/Test/64 Sightlines/merge_30m.csv")
sixtyfour <- select(sixtyfour,10) %>%
  mutate(sample = 64)
sixtyfour <- arrange(sixtyfour,zonal_mean)

data <- data.frame(rbind(eight,sixteen,thirtytwo,sixtyfour))
data$index <- rep(1:31, each=1)
data$sample <- as.factor(data$sample)

corrdata <-rename(data,vis8 = zonal_mean, viz16=zonal_mean.1,viz32=zonal_mean.2,viz64=zonal_mean.3)
corrdata <- corrdata[,c(1,3,5,7)]
corr <- round(cor(corrdata), 4)

data %>%
ggplot(aes(index, zonal_mean, col = sample)) + 
  geom_smooth(color= "black", method = "loess", se = FALSE) +
  geom_point(aes(group = sample), size = 2, alpha=1) +
    labs(x = "Tomb Index (Lowest to Highest Score)", y = "Mean Visibility Score") +
  theme(legend.position = "right") +
  scale_colour_discrete("Sample") +
  theme_light()

  
library(ggcorrplot)
# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)
