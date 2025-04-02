# source("renv/activate.R")


# Define ports of interest -----------------------------------------------------
ports <- c(
  "ENSENADA",
  "SAN BLAS",
  "MAZATLAN",
  "EL SAUZAL",
  "PUERTO MADERO",
  "MANZANILLO",
  "CHIAPAS",
  "SAN CARLOS"
)

# Define lat-lon ranges of new Revilla polygon ---------------------------------
# Coordinates com from the DOF decree at:
# http://www.dof.gob.mx/nota_detalle.php?codigo=5505736&fecha=27/11/2017
lon_range <- c(-115.471415,-110.078093)
lat_range <- c(17.655231, 20.008631)


gini <- function(x) {
  X <- numeric(length(x))
  for(i in 1:length(x)){
    xi <- x[i]
    xj <- x[-i]
    X[i] <- sum(abs(xi - xj))
  }
  sum(X) / (2 * (length(x) ^ 2) * mean(x))
}
