# sum rainfall in each month
# packages  ####

library(data.table)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(countrycode)
library(tidyverse)

# rainfall by adm, africa ####

adm_rainfall = function( adm = 3 , years = 2018:2018, admin.sf = ous , 
                         data.directory = NULL , source = NULL ){
   
  
   if ( is.numeric( adm) ) admin.sf = admin.sf %>% filter( level == adm )
   
   tmp = list()
   index = NA # marker for first record, used ot calculate map over function (slow)

   for ( year in years ){
      year = as.character(year)
      for (month in 1:12){
         
         cat( "adm-", adm , "month-" , year, month , "\n")
         
         index = sum( index, 1, na.rm = TRUE)
         
         period_file_name = paste0( data.directory , year, month, '/', 
                                   "africa_arc", year, month
                                   ) 
         
         if ( file.exists( period_file_name ) ){
            
           load( period_file_name ) # data loads as object d 
           
           arc_period = d 
   
         } else { next }
         
         # USe first rain map to determine relationship with adm 
         # subsequent maps should have same geometry
         if ( index ==  1 ){ 
           
           cat('Processing the first file takes a minute.  Subsequent files go much faster.\n')
           
           # convert rain data to SF before perofomring intersection with admin shapes
           
           # TODO use bounding box to reduce d before converting to SF
           # do conversion to sf and transform outside the function
           # separate function to create matrix of over...
            arc_period = d %>% st_as_sf %>% st_crop( . , st_bbox(admin.sf) )
            
            # d is SpatialGridDataFrame with 601,551 rows (1 per pixel) and 32 columuns,
            # one for each day of the month, and the monthly total
            
            # arc_period is an SF data.frame with rows = number of points with bbox created by admin.sf, 
            # and and 32 columuns,one for each day of the month, and the monthly total

            # convert to same crs as adm map
            # if ( is.na( projection( admin.sf ))){
            #   admin.sf = st_transform( admin.sf , crs = projection( arc_period ) )
            # } else {
            #   arc_period = st_transform( arc_period , crs = projection( admin.sf ) )
            # }
            
            # arc_over_admins = over(  ous, arc_period , returnList = TRUE)
            arc_over_admins = st_intersects( admin.sf , arc_period , sparse = FALSE )
            # result is matrix with rows = number of features in admin , and cols = 
            # number of pixels 
            # glimpse( arc_over_admins )
         }     
         
         # vector of which pixels are in country
         # glimpse( arc_over_admins )
         # pixel.in.country = which( colSums( arc_over_admins ) > 0 )
         # glimpse( x ); sum( x )
         
            # get daily data for each admin
              # convert arc_period to matrix
              # arc_period.matrix = arc_period %>% 
              #   st_set_geometry(NULL) %>%
              #   dplyr::select( -month ) %>%
              #   as.matrix()
              
            # convert arc data to matrix with rows for 601551 pixels, 1 monthly column
            # d.matrix = d@data[ pixel.in.country , ] %>% as.matrix()
            
            # TODO add function parameter for daily versus monthly
            daily = FALSE
            if ( daily ){ # daily
              d.matrix = arc_period %>% 
                dplyr::select( -month ) %>% st_set_geometry(NULL) %>% as.matrix()
            } else { # monthly
              d.matrix = arc_period %>% 
                dplyr::select( month ) %>% st_set_geometry(NULL) %>% as.matrix()
            }
            
            # matrix multiplication to get mean monthly value for all pixels in each admin
            # TODO speed up by reducing d before cross product
            arc_admins_period = arc_over_admins %*% d.matrix
 
         
         tmp[[ index ]] = tibble( 
            orgUnit = admin.sf$orgUnit ,
            orgUnit.name = admin.sf$orgUnit.name ,
            period = paste0( year, str_pad( month , 2, pad = "0")  ) ,
            rain_mean = arc_admins_period 
         ) 
      }
      
   }
   
   arc_mean_rainfall = rbindlist( tmp ) %>% as_tibble()
   
   print( paste0( "head arc_adm" , adm , "_rainfall.rds" ) )
   print( head( arc_mean_rainfall ) )

   return( arc_mean_rainfall )

}



