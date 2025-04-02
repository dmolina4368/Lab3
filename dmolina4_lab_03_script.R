# Task 1
library(dplyr)
library(tmap)
library(sf)
library(ggplot2)
task1subset <- d.joined %>% 
  dplyr::filter(STATEFP %in% c("17", "55", "19", "29"))
# map it to verify
tmap::tm_shape(task1subset) + tm_polygons()

# Save the shapefile
st_write(task1subset, "task1subset.shp", delete_dsn = TRUE)

file.info("task1subset.shp")

# Task 2

task1subset$B01001e2_normalized <- (task1subset$B01001e2 / task1subset$B01001e1) * 100
head(task1subset$B01001e2_normalized) # By total population.

# Task 3
ggplot(task1subset, aes(x = B01001e2)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Histogram of B01001e2",
    x = "B01001e2 (Male Population Count)",
    y = "Frequency"
  )

# Task 4
tmap::tm_shape(task1subset) + tm_polygons(fill = "B01001e2")

# Task 5
# 5.1
sf::st_crs(task1subset) 

# then reproject to north american equidistant conic
task1subset.projected <- task1subset %>% sf::st_transform(., "ESRI:102010")

# plot it again to make sure nothing broke
tmap::tm_shape(task1subset.projected) + tm_polygons()

# make the neighborhood
nb <- spdep::poly2nb(task1subset.projected, queen = TRUE)

nb

# 5.2
nb[[1]]

task1subset.projected$NAMELSAD[1] # county in index 1

nb[[1]] %>% task1subset.projected$NAMELSAD[.]

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

neighbors <- attr(lw$weights,"comp")$d 

hist(neighbors)

# 5.3
F75.lag <- lag.listw(lw, task1subset.projected$B01001e47)
F75.lag

# 5.4
moran.plot(task1subset.projected$B01001e47, lw, zero.policy=TRUE, plot=TRUE)

# Task 6
library(sp)
library(gstat)
library(spdep)

dist_matrix <- spDists(coords, longlat = TRUE)

power <- 2
weight_matrix <- 1 / (dist_matrix^power)
diag(weight_matrix) <- 0

print(weight_matrix)

nb

nb[[1]]

task1subset.projected$NAMELSAD[1]

nb[[1]] %>% task1subset.projected$NAMELSAD[.]

lw <- nb2listw(nb, style="W", zero.policy=TRUE)
lw$weights[1]

neighbors <- attr(lw$weights,"comp")$d 

hist(neighbors)

F75.lag <- lag.listw(lw, task1subset.projected$B01001e47)
F75.lag

moran.plot(task1subset.projected$B01001e47, lw, zero.policy=TRUE, plot=TRUE)


