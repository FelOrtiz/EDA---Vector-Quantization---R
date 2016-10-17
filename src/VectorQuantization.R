library("ggplot2")

iris
class(iris)
###################################################################

#Calculate distance between two points
calculateDistance <- function(pointA, pointB)
{
  d <- sqrt( (pointB[1] - pointA[1])^2 + (pointB[2] - pointA[2])^2 )
  
  return(d)
}

###################################################################

# Best Match Unit algorithm:
# point: point to compare
# set: pointset to compare with x
# We assume the minimun distance as BMU.

findBMU <- function(point, set)
{
  mDist <- 10^10
  index <- -1
  
  max <- nrow(set)
  
  #Loop for each set point.
  for(i in 1:max)
  {
    p <- c(set[i, 1], set[i, 2])
    distance <- calculateDistance(point, p)
    
    #Store the minimun set point by its distance to 'x' 
    if(distance < mDist)
    {
      mDist <- distance
      index <- i
    }
  }
  
  return(index)
}
###################################################################

#Move the prototype toward 'x'
updateRule <- function(bmu, point, alpha)
{
  vector <- c(point[1] - bmu[1], point[2] - bmu[2])
  dist <- calculateDistance(bmu, point)
  
  #get unit vector
  unitVector <- c( (vector[1]/dist), (vector[2]/dist) )
  
  #get the alpha magnitud
  mg <- (dist * alpha)
  
  aux <- c( (mg*unitVector[1]), (mg*unitVector[2]) )
  
  newBMU <- c(bmu[1]+aux[1], bmu[2]+aux[2])
  
  return(newBMU)
  
}

###################################################################

# Vector Quantization algorithm:
# dataset: iris dataset
# k: number of prototypes
# alphaI: Maximum alpha value (max 1)
# alphaL: Minimun alpha value (min 0)
# t: number of iterations
VQ <- function(dataset, k, alphaI, alphaL, t)
{
  #Generate random points for prototypes (k).
  x <- runif(k, 4.0, 8.0)
  y <- runif(k, 2.0, 4.5)
  
  #Store them.
  prototypes <- data.frame(x, y)
  
  #Calculate alpha ratio, this will determinate how much each prototype 
  #is going to move to a certain x (from iris dataset).
  alphaRatio <- ( (alphaI - alphaL)/t )
  alpha <- (alphaI + alphaRatio)
  
  #cat("Prototypes: \n")
  #print(prototypes)
  #cat("\n")
  
  #We move this prototypes 't' times.
  for(i in 1:t)
  {
    #selRow <- ceiling(runif(1, 0, nrow(dataset)))
    selRow <- sample(1:nrow(dataset), size=1, replace=TRUE)
    
    dsX <- dataset[selRow, 1]
    dsY <- dataset[selRow, 2]
    
    #Select a point from 'x' dataset.
    dsPoint <- c(dsX, dsY)
    
    #cat("Selected x point is (",dsX,", ",dsY,"). The ",selRow," set point\n")
    
    alpha <- (alpha - alphaRatio)
    
    #cat("Alpha is now:", alpha,"percent\n")
    
    #Find closest prototype from selected point.
    indexBMU <- findBMU(dsPoint, prototypes)
    
    #cat("The",indexBMU,"prototype is the BMU (",prototypes[indexBMU, 1],",",prototypes[indexBMU, 2],")\n")
    
    #Check for a correct bmu
    if(indexBMU >= 0)
    {
      bmu <- c(prototypes[indexBMU, 1], prototypes[indexBMU, 2])
      nBMU <- updateRule(bmu, dsPoint, alpha)
      
      #Update the BMU prototype with the new coordinates.
      prototypes[indexBMU, 1] <- nBMU[1]
      prototypes[indexBMU, 2] <- nBMU[2]
      
      #cat("Updating the prototype, from (",bmu[1],",",bmu[2],") to (",nBMU[1],", ",nBMU[2],")\n\n\n")
    }
  }
  
  return(prototypes)
}

#Parameters
alphaInit <- 0.95
alphaLast <- 0
numLoops <- 10000
newIris <- data.frame(iris$Sepal.Length, iris$Sepal.Width)

#Saving the output into a txt file.
#out <- capture.output(  VQ(newIris, 3, alphaInit, alphaLast, numLoops)  )
#cat("Output", out, file="C:/output.txt", sep="\n", append=FALSE)

protos <- VQ(newIris, 3, alphaInit, alphaLast, numLoops)

cat("Prototypes: \n")
print(protos)
cat("\n")

###################################################################

#This is just for plotting purpose
z <- 0
w <- 0
s <- "prototypes"
newData <- data.frame(Sepal.Length = protos$x, Sepal.Width = protos$y, Petal.Length = z, Petal.Width = w, Species = s)

outIris <- rbind(iris, newData)

p <- ggplot(outIris, aes(Sepal.Length, Sepal.Width))
p + geom_point(aes(color = factor(Species)), size = 2) + theme(legend.position="top")


