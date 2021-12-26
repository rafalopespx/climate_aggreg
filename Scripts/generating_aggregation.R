#' @title generating_aggregation
#' @author Rafael Lopes Paixão da Silva, IFT - UNESP/ISGlobal
#' @description  Function to crop and weight climate variables from 'rasterBrick' objects to a specified administrative level, 
#' using the 'exact_extract' function from the 'exactextractr' package. 
#'
#' @param climate_file Climate file, normally a 'rasterBrick' object.
#' @param adm_limits 'Extent', 'ShapeFile' or 'sf' object to where to crop and weight by area to calculate the weighted_mean, 
#' other options from 'exact_extract' function can be given by '...' parameters.
#' @param year [Optional] Parameter to identify the year in which the weighted mean will be done.
#'
#' @return A data.frame object with the time series for each administrative area with the weighted mean for this area.
#' @export sf;raster;ncdf4;exactextractr;foreach;parallel;doParallel;transformr
#'
#' @examples
generating_aggregation<-function(climate_file, # Any Climate object, 'rasterBrick' objects.
                                 adm_limits, # Any administrative level 'extents' to crop and weight by the area, 
                                 # 'sf' objects.
                                 year, # [Optional] A character identifying the year of the file to crop.
                                 hourly = TRUE # [Default] If TRUE, the climate_file is hourly basis, 
                                 # if FALSE it assuming the climate_file is in daily basis
                                 ){
  # Loading probably necessary libraries
  require(sf)
  require(raster)
  require(ncdf4)
  require(exactextractr)
  require(foreach)
  require(parallel)
  require(doParallel)
  require(transformr)
  require(dplyr)
  require(tidyr)
  require(tidyverse)
  
  if(is.null(year)){
    year<-"No year specified"
  }
  
  # Parameter Checks
  if(missing(climate_file) | missing(adm_limits)){
    return(print("Missing Climate files, and/or Administrative level to crop!"))
  }
  
  # Same CRS, putting the objects at the Coordinate Reference System
  if(hourly == TRUE){
    adm_limits<-sf::st_transform(adm_limits, crs = sf::st_crs(climate_file))
  }
  
  
  # Guide index creation
  ## Hourly basis
  if(hourly == TRUE){
    indexes<-format(as.Date(names(climate_file), format = "X%Y.%m.%d")) # Extracting the indexes to run over
    unique_indexes<-unique(indexes) # Picking the unique of the indexes
    smart_indexes<-sort(rep(1:length(unique_indexes), 24)) # Has to repeat 24 times, 
    # because the data is suppose to be on hourly basis
  } else {
    indexes<- seq(as.Date(paste0(year, "-01-01")), 
                          as.Date(paste0(year, "-12-31")), 
                          "1 day")
    unique_indexes<-unique(indexes)
    smart_indexes<-sort(rep(1:length(unique_indexes)))
  }
  
  
  # deriving daily measures on the raster 
  ## Paralellizar!
  ### Hourly
  if(hourly == TRUE){
    print("Hourly basis for Climate File!")
    dmean<-raster::stackApply(climate_file, smart_indexes, fun = mean)
    print("Mean Finished!")
    dmax<-raster::stackApply(climate_file, smart_indexes, fun = max)
    print("Max Finished!")
    dmin<-raster::stackApply(climate_file, smart_indexes, fun = min)
    print("Min Finished!")
    layers<-raster::nlayers(dmean)
  } else {
    print("Daily basis assumed for Climate file")
    layers<-raster::nlayers(climate_file)
    dmean<-climate_file
    # print("Daily basis, no need for mean daily average!")
  }
  
  
  ## Parallel For
  cores<-parallel::detectCores()-1
  doParallel::registerDoParallel(cores)
  print("Ow loko meo! Paralellizou ae! Vai Kasinaooooo!")
  adm_limits_list_year<-
    foreach::foreach (i=1:layers, 
                      .combine = 'bind_rows',
                      .verbose = T
                      # .packages = c("exactextractr", "tibble"), 
                      # .export = "adm_limits"
                      ) %dopar% {
      # print(paste0("Starting layer ", i))
      # Area to be weighted
      # area_weights<-raster::area(dmean)
      date <- unique_indexes[i]
      mean <- exactextractr::exact_extract(
        subset(dmean, i), 
        adm_limits, 
        "weighted_mean",
        weights = raster::area(dmean),
        append_cols = TRUE) ## select columns if dont want all
      if(hourly == TRUE){
        max <- exactextractr::exact_extract(
          subset(dmax, i), 
          adm_limits, 
          "weighted_mean",  
          weights = raster::area(dmax),
          append_cols = TRUE) ## select columns if dont want all
        min <- exactextractr::exact_extract(
          subset(dmin, i), 
          adm_limits, 
          "weighted_mean",  
          weights = raster::area(dmin),
          append_cols = TRUE) ## select columns if dont want all
        c <- tibble::tibble(
          tibble::tibble(date), 
          mean,
          weighted_max = max$weighted_mean, 
          weighted_min = min$weighted_mean)
      } else {
        c <- tibble::tibble(
          tibble::tibble(date), 
          mean)
      }
      
      # print(paste0("Layer ", i, " Finished!"))
    }
  doParallel::stopImplicitCluster()
  # parallel::stopCluster()
  print(paste0("Finished all layers for the year ", year))
  
  return(adm_limits_list_year)
}