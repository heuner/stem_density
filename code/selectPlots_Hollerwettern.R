###########################################
#https://github.com/heuner/stem_density/selectPlots.R

#purpose: SamplingDesign StemDensity, random Selection
#         using install_github("samuel-rosa/spsann")

#Authors: M. Heuner, A. Samuel-Rosa
#Date:23.06.2017
#version:0.9
##########################################


# Load libraries
library(spsann)
library(rgeos)
library(xlsx)
library(RCurl)
library(devtools)
# devtools::install_github("samuel-rosa/spsann")

# load data
# ASR: I suggest you to avoid setting the working directory to guarantee reproducibility, i.e. so that I will
# be able to use the same R script in my Ubuntu machine. Let RStudio do it by using RStudio projects. ;)
# setwd("E:/tibass/data/stem_density/data")
# getwd()
my_data_holl <- read.table("data/holl_cand.txt", header=TRUE, sep="")
fix(my_data_holl)
head(my_data_holl, 12) # MH: The first 11 lines contain the fixed points in Hollerwettern.
boundary_h <- my_data_holl[-c(1:11), ]

#MH:Hollerwettern
plot(my_data_holl[, 2:3], asp = 1, pch = 15, col = c("darkseagreen3","darkgreen")[as.integer(my_data_holl$Veg)])
points(my_data_holl[1:12, 2:3], pch = 3)

# Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
# ASR: The 'boundary' argument is only necessary for plotting, i.e. if you set 'plotit = TRUE'. spsann can 
# estimate the 'boundary' from 'candi', but I imagine that you have a polygon, drawn on a satellite image,
# that indicates the boundaries of the study area.
# Should it be gridded?
sp::gridded(boundary_h) <- c("x", "y")
#str(boundary)
# Disolve ploygones, here not nescessary
boundary <- rgeos::gUnaryUnion(as(boundary_h, "SpatialPolygons"))

# determining variables
# ASR: Do not use the fixed points as candidate locations.
#MH:Krautsand 
candi <- my_data_holl[12:nrow(my_data_holl), 2:3]
covars <- my_data_holl[12:nrow(my_data_holl), c(2:3, 6:8)] # ASR: Explicitly include the coordinates as covariates.
schedule <- scheduleSPSANN(initial.temperature = 5, stopping = 200, chain.length = 1, chains = 1000)
free <- 32 # ASR: This is the number of additional points.

# Fixed points
fixed <- my_data_holl[1:11, 2:3]
id <- SpatialTools::dist2(coords2 = as.matrix(fixed), coords1 = as.matrix(candi))
id<- apply(id, 2, which.min)
fixed <- cbind(id, fixed)
# ASR: spsann assumes that all fixed points are within the boundaries of the study area. Thus, the covariate
#      values asssociated to the fixed points do not matter because they will be sampled from the nearest 
#      candidate point.
plot(my_data_holl[, 2:3], asp = 1, col = my_data_holl$SampleArt)
points(candi[id, ], pch = 20)
objDIST(points = id, candi = candi, covars = covars)

# Optimization
set.seed(72)

res <- optimDIST(
  points = list(free = free, fixed = fixed), candi = candi, covars = covars, 
  schedule = schedule, plotit = TRUE, boundary = boundary)
objSPSANN(res)
objDIST(points = res, candi = candi, covars = covars)
png("res/fig/optim_holl_1000_1rep2.png", width = 480*2)
plot(res, boundary = boundary)
dev.off()

png("res/fig/optim_holl_1000_1_brep2.png", width = 480)
plot(my_data_holl[, 2:3], asp = 1, pch = 15, col = c("darkseagreen3", "darkgreen")[as.integer(my_data_holl$Veg)])
points(res$points[, 2:3], pch = c(3, 1)[res$points$free + 1])
dev.off()

#Export table
write.xlsx(res$points, "results/sampleHolle.xlsx")

str(res)
