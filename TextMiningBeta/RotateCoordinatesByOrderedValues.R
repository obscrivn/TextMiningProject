RotateCoordinatesByOrderedValues <- function(coordinates, data.values,
                                             method = "pearson"){
  #Assuming coordinates is a n x 2 matrix and so is the output.
  #Our coordinate is probably two columns of a data frame so there might be cleaner ways
  #set this up.
  
  mean.x <- mean(coordinates[, 1])
  mean.y <- mean(coordinates[, 2])
  coordinates[,1] <- coordinates[, 1] - mean.x
  coordinates[,2] <- coordinates[, 2] - mean.y
  
  f <- function(theta){
    rotated.x <- cos(theta) * coordinates[, 1] - sin(theta) * coordinates[, 2]
    cor(rotated.x,data.values, method = method)
  }
  theta = optimize(f, c(0, 2*pi), tol = 0.0001, maximum = T)$maximum
  
  output.coordintates <- coordinates
  output.coordintates[, 1] <- cos(theta)*coordinates[, 1] - sin(theta)*coordinates[, 2] +
    mean.x
  output.coordintates[, 2] <- sin(theta)*coordinates[, 1] + cos(theta)*coordinates[, 2] + 
    mean.y
  return(output.coordintates)
}