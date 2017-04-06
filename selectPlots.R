###########################################
#https://github.com/heuner/stem_density/selectPlots.R

#purpose: SamplingDesign StemDensity, random Selection
#         using install_github("samuel-rosa/spsann")

#Author: M. Heuner, A. Samuel-rosa
#Date:06.04.2017
#version:0.9
##########################################


# Load libraries
library(spsann)
library(rgeos)
library(xlsx)
#install.packages("devtools")
#devtools::install_github("samuel-rosa/spsann")

my_data<-read.table("https://github.com/heuner/stem_density/data/kraut_cand.txt", header=TRUE)
head(my_data)
boundary <- my_data

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


#Formal class 'SpatialPointsDataFrame' [package "sp"] with 5 slots
sp::coordinates(boundary) <- c("x", "y")
#str(boundary)
#Should it be gridded?
sp::gridded(boundary) <- TRUE
# Disolve ploygones, here not nescessary
#boundary <- rgeos::gUnaryUnion(as(boundary, "SpatialPolygons"))

#determining variables
#fix(data)
candi <- my_data[, 2:3]
covars <- my_data[, 6:8]
schedule <- scheduleSPSANN(initial.temperature = 5, stopping = 200)
free <- 22


#id <- sample(1:nrow(candi), 40)
set.seed(2000)
id <- candi[1:15,]
fixed<-as.numeric(row.names(id))
#fixed <- cbind(id, candi[id, ])
objDIST(points = fixed, candi = candi, covars = covars, use.coords = TRUE)
set.seed(2001)
res <- optimDIST(
  points = list(free = free, fixed = fixed), candi = candi, covars = covars, use.coords = TRUE, 
  schedule = schedule, plotit = TRUE, boundary = boundary)
objSPSANN(res)
objDIST(points = res, candi = candi, covars = covars, use.coords = TRUE)
plot(res, boundary = boundary)

#Export table

#write.xlsx(res$points, "https://github.com/heuner/stem_density/results/sampleKraut.xlsx") 
write.table(res$points, "https://github.com/heuner/stem_density/results/sampleKraut.txt", sep=";")
