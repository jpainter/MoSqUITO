# R naturalearth 
# Original idea: https://downwithtime.wordpress.com/2013/12/04/naturalearthdata-and-r-in-ggplot2/
# JP modified to SF June 2019

library( rnaturalearth )
library(raster)
library(rgdal)
library(tidyverse)
library( sf )

dir = "Maps/NaturalEarth"

## Download needed layers
#  Assuming you have a path 'Maps' that you store your spatial files in.  This
#  is all downloaded from <a href="http://www.naturalearthdata.com/downloads/">http://www.naturalearthdata.com/downloads/</a> using the
#  1:50m "Medium" scale data.

# lakes
ne_50m_lakes <- ne_download(scale = 50, type = 'lakes', category = 'physical', 
                            destdir = dir , returnclass='sf' )

# rivers
ne_50m_rivers_lake_centerline <- ne_download(scale = 50, type = 'rivers_lake_centerlines', category = 'physical', 
                                             destdir = dir , returnclass='sf' )
# coast
ne_50m_coastline = ne_download(scale = 50, type = 'coastline', category = 'physical', 
                               destdir = dir , returnclass='sf' )

# relief 
NE2_50M_SR_W <- ne_download(scale = 50, category = 'raster', type = 'NE2_50M_SR_W', 
                            destdir = dir , returnclass='sf' )


nat.earth <- stack('./Maps/NaturalEarth/NE2_50M_SR_W/NE2_50M_SR_W.tif')
# nat.earth.sf <-  st_as_sf( nat.earth )

ne_lakes <-  st_read( './Maps/NaturalEarth/ne_50m_lakes.shp',
                         'ne_50m_lakes' )

ne_rivers <- st_read('./Maps/NaturalEarth/ne_50m_rivers_lake_centerlines.shp',
                     'ne_50m_rivers_lake_centerlines')

ne_coast <-st_read('./Maps/NaturalEarth/ne_50m_coastline.shp',
                    'ne_50m_coastline')

#  Subset
.country = "Kenya"
country = ne_countries( scale = 50 , country = .country , returnclass='sf' )
adm1 = ne_states( country = .country , returnclass='sf' )
domain <- st_bbox( country )

nat.earth.subset = crop( nat.earth , extent( country ) )
lakes.subset = st_crop( ne_lakes , domain)
river.subset <- st_crop(ne_rivers, domain)
coast.subset <- st_crop(ne_coast, domain)


rast.table <- data.frame(xyFromCell( nat.earth.subset , 1:ncell( nat.earth.subset )),
                         getValues( nat.earth.subset /255))

rast.table$rgb <- with(rast.table, rgb( NE2_50M_SR_W.1,
                                        NE2_50M_SR_W.2,
                                        NE2_50M_SR_W.3,
                                        1))
# et voila!

library(ggspatial)
ggplot() + 
  layer_spatial( nat.earth.subset ) +
  # geom_sf( data = country, alpha = 0 , color = 'brown' ) +
  geom_sf( data = adm1 , alpha = 0 , color = 'brown',  ) +
  geom_sf( data = lakes.subset , fill = 'blue', alpha = .3 )  +
  geom_sf( data = river.subset , color = 'blue' , alpha = .3 )  +
  geom_sf( data = coast.subset , color = 'brown')  +
  coord_sf(xlim = c(domain["xmin"], domain["xmax"]), ylim = c(domain["ymin"], domain["ymax"])) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab('') + ylab('')
  
