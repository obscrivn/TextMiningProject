MyPlot <- function(coordinates, grouped.by=NULL, rotated.to=NULL, labeled.by=NULL){

if (is.null(rotated.to)){
}else{
  coordinates <- RotateCoordinatesByOrderedValues(coordinates, rotated.to)
}

#Start to define the dataframe and ggplot object
df <- data.frame(x = coordinates[, 1], y = coordinates[, 2])
g <- ggplot(df, aes(x,y))


#Since points or labels might inherit group mappings, we define them now.
gp <- geom_point()
gt <- NULL


#If there are labels, transparent out the points and add the text
if (is.null(labeled.by)){
  gp$aes_params$alpha <- .4
}else{
  gp$aes_params$alpha <- 0
  gt <- geom_text(aes(label=labeled.by))
}


#If there are groups, add the grouping and adjust the ggplot object
if (is.null(grouped.by)){
}else{
  df.temp <- df
  df.temp$grouped.by <- grouped.by
  g <- ggplot(df.temp, aes(x,y,color=grouped.by)) + geom_density2d(alpha=.2) 
  rm(df.temp)
}


print(g + gp + gl) 
}
