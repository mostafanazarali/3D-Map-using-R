# The code to produce 3D map from LandSAT images and SRTM elevation data
# All input data can be downloaded from EarthExplorer website: earthexplorer.usgs.gov/
# This code is inspired from the original one written by Tyler Morgan-Wall (www.tylermw.com)
# written by Mostafa Nazarali (10 May 2020)

library(rayshader)
library(sp)
library(raster)
library(scales)
library(rayrender)
library(rgdal)

setwd("J:/LockdownActivities/Chabahar")  #You have to specify the path of your input files

# If your PC has enough RAM memory, comment these three lines
memory.limit(size=6000)
rasterOptions(maxmemory = 1e+10)
rasterOptions(memfrac=0.3)

# Read elevation data: In order to speed up the code, I have cropped the elevation data (.hgt format using QGIS software) into an smaller area
elevation <- raster("QGIS_ELEVATION.TIF")

# load satellite image: Just use Band 2,3, and 4
r <- raster("LC08_L1TP_157042_20200411_20200422_01_T1_B4.TIF")
g <- raster("LC08_L1TP_157042_20200411_20200422_01_T1_B3.TIF")
b <- raster("LC08_L1TP_157042_20200411_20200422_01_T1_B2.TIF")
image <- sqrt(stack(r, g, b))

# convert long/lat to UTM
elevation_UTM <- projectRaster(elevation,crs = crs(image), method = "bilinear")

# crop the data and convert lat/long to UTM:
bottom_left = c(y=25.18, x=60.13361) #Specify the extent of your region: Bottom Left corner
top_right   = c(y=25.47972, x=60.69722) #Specify the extent of your region: Top right corner
extent_latlong = SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
extent_utm = spTransform(extent_latlong, crs(elevation_UTM))
e = extent(extent_utm)
e@xmin <- 211099 # Specify the coordinates in UTM format
e@xmax <- 268498
e@ymin <- 2787956 
e@ymax <- 2820071

# crop in the mentioned coordinates
image_cropped = crop(image, e)
names(image_cropped) = c("r","g","b")
r_c <- raster_to_matrix(image_cropped$r)
r_g  <- raster_to_matrix(image_cropped$g)
r_b  <- raster_to_matrix(image_cropped$b)
image_array = array(0,dim=c(ncol(image_cropped1),nrow(image_cropped1),3))
image_array[,,1] <- r_c/255
image_array[,,2] <- r_g/255
image_array[,,3] <- r_b/255

elevation_cropped = crop(elevation_UTM, e)
elevation_matrix = raster_to_matrix(elevation_cropped)
elevation_matrix <- aperm(elevation_matrix)

# Flip the elevation matrix vertically
a <-array(0,c(nrow(elevation_matrix),ncol(elevation_matrix)))
dim(elevation_matrix)
for (i in c(1:ncol(elevation_matrix)))
  a[,i]=elevation_matrix[,ncol(elevation_matrix)-i+1]

# Flip the elevation matrix vertically
im<-array(0,c(nrow(image_array),ncol(image_array),3))
for (i in c(1:nrow(image_array)))
  im[i,,1:3]=image_array[nrow(image_array)-i+1,,1:3]

# Draw the 3D plot
plot_3d(rescale(im),a, zscale =3,
        fov = 0, theta = 60, zoom = 0.5, phi = 45, windowsize = c(3000, 3000),
        water = TRUE, wateralpha = 0.25, waterdepth =0.01, watercolor = "darkblue",
        background = "#F2E1D0", shadowcolor = "#523E2B")
render_label(elevation_matrix, text = "Beheshti Port", x = 650, y =300, z = 200, zscale = 1,linewidth = 2, 
             freetype="FALSE")
render_label(elevation_matrix, text = "Chabahar Bay", x = 400, y =650, z = 200, zscale = 1,linewidth = 2,
             linecolor = "darkred", freetype="FALSE",dashed= T)
render_label(elevation_matrix, text = "Pozm Bay", x = 400, y =1600, z = 200, zscale = 1,linewidth = 2,
             linecolor = "darkred", freetype="FALSE",dashed=T)
render_label(elevation_matrix, x = 900, y = 1000, z = 0, zscale = 1,  textcolor = "white", linecolor = "white",
             text = "Oman Sea", relativez = FALSE, linewidth = 5, freetype="FALSE")
render_snapshot()