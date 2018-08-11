###required packages
library(dplyr)
library(ggplot2)
library(purrr)
library(raster)
library(sf)
library(smoothr)

laserplot <- function(p) {
  ggplot(p) +
    geom_sf() +
    coord_sf(xlim = c(116.5, 127), ylim = c(4.5, 19)) + 
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}
### Data
# data location
# from https://www.bodc.ac.uk/data/online_delivery/gebco/gebco_08_grid/
bathy_fname <- "data//GRIDONE_2D_116.5_4.5_127.0_21.0.nc" 

#load bathy data
#nc <- open.nc(bathy_fname)
#raster load
ras <- raster(bathy_fname)
brk <- seq(-10000, 0, 1000)
xtile <- .8
dat <- bathy_fname %>%
  raster() %>%
  rasterToContour(levels = brk) %>%
  st_as_sf() %>%
  st_cast("LINESTRING") %>%
  mutate(len = st_length(.)) %>%
  smooth(method = "chaikin")

plt <- dat %>%
  filter(len > quantile(len, xtile)) 

k <- plt$level %>%
  levels() %>%
  map(~filter(plt, level == .)) %>%
  map(laserplot)

#k %>%
#  seq_along() %>%
#  map(~ggsave(paste0("plot", ., ".pdf"), plot = k[[.]], device = "pdf")) %>%
#  invisible()