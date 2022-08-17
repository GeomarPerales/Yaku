#' values extraction of PISCO daily data from a stations group.
#'
#' extraction of daily and monthly values from PISCO data, PISCO is Peruvian Interpolated Data of the SENAMHI Climatological and Hydrological Observations.
#' @param x a dataframe with PISCO file name, longitude and latitude from stations group.
#' @param type extraction type, daily is default value, other option is monthly
#' @importFrom raster brick
#' @importFrom raster projection
#' @importFrom raster extract
#' @importFrom sp coordinates
#' @import sp
#' @import raster
#'
#' @export
#'
#' @author Geomar Perales Apaico
#'
#' @name piscogroup

piscogroup <-function(x, ...) UseMethod("piscogroup")

piscogroup <- function(x, type = NULL){
  x <- x[,1:4]
  colnames(x) <- c("nc", "name","v1", "v2")
  if(x$v1[1] < x$v2[1]){
    colnames(x) <- c("nc", "name","lon", "lat")
  } else if(x$v1[1] > x$v2[1]){
    colnames(x) <- c("nc", "name","lat", "lon")
  }

  file.nc <- unique(as.character(x$nc))
  name <- as.character(x$name)
  longitude <- as.numeric(x$lon)
  latitude <- as.numeric(x$lat)

  if(is.numeric(longitude) & is.numeric(latitude)){
    coord <- data.frame(x = as.numeric(longitude), y = as.numeric(latitude))
  }  else {
    stop("coordinates not defined")
  }

  coord <- coord
  variable.raster <- raster::brick(file.nc)
  sp::coordinates(coord) <- ~ x + y
  raster::projection(coord) <- raster::projection(variable.raster)
  points <- raster::extract(variable.raster[[1]], coord, cellnumbers = T)[,1]
  pisco.data <- as.vector(t(variable.raster[points]))
  pisco.data <- round(pisco.data, digits = 2)
  row.names(pisco.data) <- seq(1, nrow(pisco.data), 1)

  if(is.null(type)){
    date <- seq(as.Date("1981-01-01"), by = "day", length = nrow(pisco.data))
    dt <- data.frame(date = date, pisco = pisco.data)
    return(dt)

  }else if(type == "day"){
    date <- seq(as.Date("1981-01-01"), by = "day", length = nrow(pisco.data))
    dt <- data.frame(date = date, pisco = pisco.data)
    return(dt)

  } else if(type == "month"){
    date <- seq(as.Date("1981-01-01"), by = "month", length = nrow(pisco.data))
    dt <- data.frame(date = date, pisco = pisco.data)
    return(dt)
  }
}

#' @rdname piscogroup
