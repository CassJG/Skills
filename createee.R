#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'

create_mandala <- function(num_circles, radius_increment) {
  angles <- seq(0, 2 * pi, length.out = num_circles)
  df <- data.frame(x = numeric(num_circles), y = numeric(num_circles), radius = numeric(num_circles))

  for (i in 1:num_circles) {
    radius <- i * radius_increment
    df[i, "x"] <- radius * cos(angles[i])
    df[i, "y"] <- radius * sin(angles[i])
    df[i, "radius"] <- radius
  }

  return(df)
}










#'
#'A function that creates a polygon
#'
#'
#'
#'
#'
#'@param radius
#'@param sides How many sides you want on the polygon
#'
#'@return coordinates
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
create_polygon <- function(radius, sides) {
  coordinates <- list()
  angle <- 2 * pi / sides  # Divide a circle into 8 equal parts for octagon

  for (i in 0:sides) {
    x <- radius * cos(i * angle)
    y <- radius * sin(i * angle)
    coordinates[[i+1]] <- c(x, y)
  }

  return(coordinates)
}

