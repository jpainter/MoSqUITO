
# noaa ARC22 geotiff climate data from http://www.cpc.ncep.noaa.gov/products/fews/data.shtml
# see: http://www.cpc.ncep.noaa.gov/products/fews/AFR_CLIM/ARC22_201303_final.pdf
# example script from http://creativemorphometrics.co.vu/blog/2014/03/27/extracting-climate-data-in-r/

library(tidyverse)
library(RCurl)
library(rgdal)
library(lubridate)


# downloads list of daily files from source ####
get_file_list = function( source = c("ARC2", "CHIRPS" ), period = "daily",
                          year_range = 2000:year(now()) ){

# set internet source and download file locations  ####

if (toupper(source) == "ARC2" ){ 
   
   # ARC22 ####
   # download from ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/ARC22/geotiff/ 
   
   url <- "ftp://ftp.cpc.ncep.noaa.gov/fews/fewsdata/africa/ARC22/geotiff/" 

   filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
   filenames_split = unlist(strsplit(filenames, "\n" ))
   filenames_split = unlist(strsplit( filenames_split, "\r"))
   file_list = strsplit(filenames_split, "[.]")
   file_dates = ymd(sapply(file_list, "[[", 2)) 
   
} 

if (toupper(source) == "CHIRPS" ){ 
   
   # CHIRPS ####
   # monthly: ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/
   # decad: 
   # daily: ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05/
   
   if ( period == "monthly"){
      
      url <- "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_monthly/tifs/" 
      filenames <- getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 
      filenames_split = unlist(strsplit(filenames, "\n" ))
      file_list = strsplit(filenames_split, "[.]")
      file_dates = dmy(
         paste0( "01", sapply(file_list, "[[", 4), sapply(file_list, "[[", 3))
      )
   } else {
      # TODO: allow download from each--need to loop through folder directory
      if ( period == "daily"){ # 2015 only...
         
         fn = list()
         fl = list()
         fd = list()
         urls = list()

         pb = progress_estimated( length( year_range ) )
         for ( i in seq_along( year_range )) {
            pb$pause(0.1)$tick()$print()
            
            url_year <- paste0( "ftp://ftp.chg.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/africa_daily/tifs/p05/" ,
                           year_range[i], "/")
            
            filenames <- getURL(url_year, ftp.use.epsv = FALSE, dirlistonly = TRUE) 
            fn[ i ] = list( unlist( strsplit( filenames, "\n" ) ) ) 
            fl[ i ] = list( str_sub( fn[ i ][[1]] , 13, 16) )
            fd[ i ] = list( anytime::anydate( str_sub( fn[ i ][[1]] , 13, 22 ) )  )
            
            urls[ i ] = list( paste0( url_year ,  unlist( fn[i] ) ) )
         }
         
         # file_list = unlist( fl )
         file_dates =  do.call( 'c' , fd ) #unlist( fd )
         filenames_split  = unlist( fn ) 
         url = unlist( urls )
      }

   } 
  
}
  
  file_list_df = tibble( url = url , date = file_dates, file = filenames_split ) %>% filter( complete.cases(.)) 
  return( file_list_df  )
  
  }
# test
# arc_file_list = get_file_list( "ARC2")
# chirps_file_list = get_file_list( "CHIRPS" )


## download a month of daily geotiff files and aggregates them into one monthly fil

download_years_rain_geotiff = function(
   source = "ARC2" , 
   period = "daily" ,
   remove = FALSE ,  # TODO: option to remove daily files
   replace = FALSE , # TODO: option to overwrite; skip download if exists
   year = 2018 ,
   file_list = NULL
   ){


# packages  ####
library(tidyverse)
library(stringr)
library(Hmisc)  # for monthDays function
library(data.table)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(maptools)
require(rleafmap)
library(rworldmap) # this pkg has waaaay better world shapefiles
library(RCurl)
library(R.utils)

# file list ####

   if (is.null( file_list)) file_list = get_file_list( source = source, 
                                                       period = period, 
                                                       year_range = year )
   
   start_date = paste0( year, "01" , "01")
   
   end_date = paste0( year, '12' , '31')
   
   date_interval = interval( ymd(start_date), ymd(end_date) )
   
   files_to_download = file_list %>% filter( date %within% date_interval )
   
   num_files = nrow(files_to_download)
   
   url = files_to_download$url
   

# download files   #### 
   for (i in 1:num_files){
      
      file_date = files_to_download[i,]$date
      
      folder_name = paste0("datasets/", source, "/" , year(file_date), month(file_date))
      
      cat( paste(i, files_to_download[i, 'file']), sep = "\n") ;flush.console()
      
      url = files_to_download$url[i] 
      
      remote_file =  files_to_download[i, 'file']
      
      download_file = paste0(folder_name, "/", remote_file )
      
      if (!dir.exists(folder_name)) dir.create(folder_name) 
      
      # Redownload or not
      already_downloaded = file.exists( download_file )
      size = file.info( download_file )$size
      
      if ( already_downloaded & !replace & size > 100 ){ 
         print( 'file already downloaded')
         next } # do not redownload 
      
      
      con <- file( download_file, open = "wb")
      
      cat(" downloading file...\n") ;flush.console()
      
      repeat{
         success = TRUE
         
         bin <- tryCatch(
            {
               getBinaryURL( url , ssl.verifypeer=FALSE)
            },
            
            error = function(cond){
               success = FALSE
               print("download error")
               return(success)
            }
         )
         
         if (success == TRUE)   break
      }
      
      cat("  writing file...\n") ;flush.console()
      writeBin(bin, con)
      close(con)
      
      # cat("   unzipping file...\n") ;flush.console()
      # unzip(zip_file, exdir = folder_name)
      cat("done \n \n") ;flush.console()
      
   }

   }

# downloaded datsets ####
for ( year in 2018 ){
   
   download_years_rain_geotiff( year = year, source = "CHIRPS")
   
}


# make monthly summary file  ####

aggregate_geotiff_monthly = function( 
   source = "ARC2" , 
   period = "daily" ,
   remove = FALSE ,  # TODO: option to remove daily files
   replace = FALSE , # TODO: option to overwrite; skip download if exists
   year = NULL ,
   month = NULL ,
   file_list = NULL
){
   
   # file list ####
   
   if (is.null( file_list)) file_list = get_file_list( source = source, period = period )
   
   start_date = paste( year, month , 1, sep = '-')

   end_date = paste( year, month, days_in_month( ymd(start_date )) , sep = '-' )

   date_interval = interval( ymd(start_date), ymd(end_date) )

   files_to_download = file_list %>% filter( date %within% date_interval ) 

   num_files = nrow(files_to_download)

   file_date = files_to_download$date
   
   folder_name = unique( paste0("Datasets/", source, "/" , year, month ) )
   
   period_file_name = paste0( source, "_", year, "_", month)
   
   print( period_file_name )
   
   # Skip if aggregate already exists #### 
   if ( file.exists( paste0( folder_name, "/", period_file_name)  ) & replace == FALSE ){ return( "Aggregate file alread exists") }
   
   # matrix of monthly values ####
   
   if ( source == "CHIRPS"){
      rain = matrix(, 2400000, num_files)
   } else{
      if (source == "ARC2")
         rain = matrix(, 601551, num_files) 
   }
   
   
   colnames(rain) = paste0( "day", 1:num_files)
   
   # combine daily files into monthly ####
   if ( period == 'daily'){ 
      for (i in 1:num_files){  
         
         remote_file =  files_to_download[i, 'file']
         
         download_file = paste0( folder_name, "/", remote_file ) 
         
         cat("   unzipping", download_file, "...\n") ;flush.console()
         if ( source == "ARC2"){
            
            tiff_file = unzip( download_file , exdir = tempdir()) # don't write uncompressed file
            
         } else {
            
            if ( source == "CHIRPS"){
               tiff_file = gunzip( download_file , 
                                   temporary = TRUE, overwrite = TRUE,
                                   remove = remove) # keep original files while testing...
            }
         }
         
         
         d = try(
            readGDAL(tiff_file, silent = TRUE)
         ) 
         
         if ( class(d) == "try-error" ){
            
            cat("file not found, or something...\n" )
            next
         }
         
    
         # put values in matrix with column corresponding to day
         rain[, paste0("day", i)] = data
         
         
         
         cat(summary(rain[, paste0("day", i)])); cat("\n")
         
      }
   }
   
   #bind matrix of rain totals to spatial polygon
   d@data = cbind( d@data, rain )
   
   # remove original 'band1' column
   d@data = d@data[, -1]
   
   # remove these from CHIRPS datafile...
   d@data[ d@data == -9999 ] <- NA 
   
   rain_total = rowSums( d@data, na.rm = TRUE )
   cat( "MONTHLY TOTAL",  summary(rain_total), "\n" ); flush.console()
   
   # Add total rain data 
   
   d@data$month = rain_total # add in total
   
   # write summary to file ####
   cat("writing file"); flush.console()
   save( d, file = paste0( folder_name, "/", period_file_name) )
}

# ARC
for ( year in year_range ){
   for ( month in 1:12 ){
      
      aggregate_geotiff_monthly(source = 'ARC2', year = year, month = month)
   }
}

# CHIRPS
for ( year in 2018 ){
  for ( month in 1:12 ){
    
    aggregate_geotiff_monthly(source = 'CHIRPS', year = year, month = month)
  }
}

################### TEST
# aggregate_geotiff_monthly(source = 'ARC2', year = 2016, month = 2 )
# aggregate_geotiff_monthly(source = 'CHIRPS', year = 2018, month = 2 )



### Summarise by adm area  ######

# Agregate into areas defined by gadm maps, e.g. ARC2_adm0_rainfall.rds,
# by running script, arc_rainfall_by_adm.R
# Output of this file is used by rain apps like rain_module_flex.R in the shiny app
# rain_dashboard_flex.RMD 

source( "arc_rainfall_by_adm.R" )

# create files for adm0, adm1, and adm2 
map( 0:2 , ~adm_rainfall( adm = .x , years = year_range ) )

# Note this takes awhile to run.  The first month takes the longest because it assigns
# the area to the rain data; subsequent months run much faster.  
# Takes about 1 minutes per year


