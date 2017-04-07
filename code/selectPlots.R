###########################################
#https://github.com/heuner/stem_density/selectPlots.R

#purpose: SamplingDesign StemDensity, random Selection
#         using install_github("samuel-rosa/spsann")

#Authors: M. Heuner, A. Samuel-rosa
#Date:06.04.2017
#version:0.9
##########################################


# Load libraries
library(spsann)
library(rgeos)
library(xlsx)
#library(RCurl)
library(devtools)
# devtools::install_github("samuel-rosa/spsann")

# load data
# ASR: I suggest you to avoid setting the working directory to guarantee reproducibility, i.e. so that I will
# be able to use the same R script in my Ubuntu machine. Let RStudio do it by using RStudio projects. ;)
# setwd("E:/tibass/data/stem_density/data")
# getwd()
my_data <- read.table("data/kraut_cand.txt", header=TRUE, sep=";")

head(my_data, 16) # ASR: The first 15 lines contain the fixed points.
boundary <- my_data[-c(1:15), ]

plot(my_data[, 2:3], asp = 1, col = my_data$SampleArt)
# ASR: I do not understand this. I thought that the fixed points were inside the area where you were going to 
#      sample. But I only see seven points in the area with red circles. Is this correct?
#MH: Yes, that is true. That was why I would like to also sample plant properties from the outside points on the
#left side. Thank you for your asking!These points are not in the area which will be interpolated. They are
#above mean high water and thus they lose relevance for attenuating waves. I removed them from the dataset.
#The outside points on the right side are not really outside. They were outside due to marsh expansion
#between the years 2010-2016.I had in the moment only an old database of vegetation types and DEM from 2010.
#I wait for the database which were sampled 2016. Nevertheless, I filled the gap of candidates and have updated
#kraut_cand.txt thus these points are no longer outside. 
# ASR: OK!

# Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
# ASR: The 'boundary' argument is only necessary for plotting, i.e. if you set 'plotit = TRUE'. spsann can 
# estimate the 'boundary' from 'candi', but I imagine that you have a polygon, drawn on a satellite image,
# that indicates the boundaries of the study area.
# Should it be gridded?
sp::gridded(boundary) <- c("x", "y")
str(boundary)
# Disolve ploygones, here not nescessary
boundary <- rgeos::gUnaryUnion(as(boundary, "SpatialPolygons"))

# determining variables
#fix(data)
# ASR: Do not use the fixed points as candidate locations.
candi <- my_data[16:nrow(my_data), 2:3]
covars <- my_data[16:nrow(my_data), c(2:3, 6:8)] # ASR: Explicitly include the coordinates as covariates.
schedule <- scheduleSPSANN(initial.temperature = 5, stopping = 200)
free <- 22 # ASR: This is the number of additional points.

# Fixed points
fixed <- my_data[1:15, 2:3]
id <- SpatialTools::dist2(coords2 = as.matrix(fixed), coords1 = as.matrix(candi))
id <- apply(id, 2, which.min)
fixed <- cbind(id, fixed)
# ASR: spsann assumes that all fixed points are within the boundaries of the study area. Thus, the covariate
#      values asssociated to the fixed points do not matter because they will be sampled from the nearest 
#      candidate point.
plot(my_data[, 2:3], asp = 1, col = my_data$SampleArt)
points(candi[id, ], pch = 20)
objDIST(points = id, candi = candi, covars = covars)

# Optimization
set.seed(2001)
res <- optimDIST(
  points = list(free = free, fixed = fixed), candi = candi, covars = covars, 
  schedule = schedule, plotit = TRUE, boundary = boundary)
objSPSANN(res)
objDIST(points = res, candi = candi, covars = covars)
plot(res, boundary = boundary)

dev.off()
png("res/fig/optim.png", width = 480 * 2)
plot(res, boundary = boundary)
dev.off()

#Export table

#write.xlsx(res$points, "https://github.com/heuner/stem_density/blob/master/results/sampleKraut.xlsx") 
#write.table(res$points, "https://github.com/heuner/stem_density/blob/master/results/sampleKraut.txt", sep=";")
