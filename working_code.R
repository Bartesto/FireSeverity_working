## code snippets and functions
library(sf)
library(terra)
library(tidyverse)

load("sev.RData")

## code to return firehistory data for a forest block

find_tenure <- function(like = NULL){
  if(is.null(like)){
    return(DBCA_tenure)
  } else {
    lc <- tolower(like)
    result <- DBCA_tenure[grepl(lc, DBCA_tenure, ignore.case = TRUE)]
    return(result)
  }
}


find_block <- function(like = NULL){
  if(is.null(like)){
    return(DBCA_blocks)
  } else {
    lc <- tolower(like)
    result <- DBCA_blocks[grepl(lc, DBCA_blocks, ignore.case = TRUE)]
    return(result)
  }
}

find_block()
location <- find_block(like = "leona")
location2 <- find_tenure(like = "wandoo")[1]

## NEW - update FireHistory package
DBCA_aoi <- function(choice, block = TRUE){
  if(block == TRUE){
    aoi <- blocks_shp |>
      dplyr::filter(SFB_BLOCK %in% choice)
  } else {
    aoi <- tenure_shp |>
      dplyr::filter(LEG_NAME %in% choice)
  }

  aoi_list <- list(aoi = aoi,
                   aoi_name = choice)
  if(dim(aoi_list$aoi)[1] == 0) stop("Choice is not a forest block")
  return(aoi_list)
}

l <- DBCA_aoi(choice = location, block = TRUE)

make_wkt <- function(aoi, fh_crs){
  aoi_wkt <- sf::st_transform(aoi[['aoi']], fh_crs) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_geometry() |>
    sf::st_as_text()
  return(aoi_wkt)
}
gdb_assembler <- function(fire_path, from, to, aoi){
  # messenging
  cli::cli_progress_message("Working with the Corporate geodatabase file")
  layer_list <- sf::st_layers(fire_path)
  fh_crs <- layer_list$crs[grepl(lyr, layer_list$name)][[1]]
  # match crs and proceed
  wkt_flt <- make_wkt(aoi, fh_crs)
  cli::cli_progress_message("Querying the fire history")
  fh <- sf::st_read(dsn = fire_path, layer= lyr, quiet = TRUE, wkt_filter = wkt_flt)
  # manually rename the geometry column
  attr(fh, "sf_column") <- "shape"
  names(fh) <- tolower(names(fh))
  if(dim(fh)[1] != 0){
    fh_alb <- fh |>
      dplyr::filter(fih_year1 >= from & fih_year1 <= to) |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    aoi_alb <- aoi[['aoi']] |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    dat_list <- list(fh_alb = fh_alb,
                     aoi_alb = aoi_alb,
                     aoi_name = aoi[['aoi_name']],
                     period = c(from,to))
    return(dat_list)
  } else {
    stop("There is no fire history data for that location")
  }

}

shp_assembler <- function(fire_path, from, to, aoi){
  # messenging
  cli::cli_progress_message("Working with a shape file")
  fname <- tools::file_path_sans_ext(basename(fire_path))
  fquery <- paste0('SELECT * from ', fname, ' LIMIT 1')
  fh_crs <- sf::st_crs(sf::read_sf(fire_path, query = fquery))
  # match crs and proceed
  wkt_flt <- make_wkt(aoi, fh_crs)
  cli::cli_progress_message("Querying the fire history")
  fh <- sf::st_read(dsn = fire_path, quiet = TRUE, wkt_filter = wkt_flt)
  names(fh) <- tolower(names(fh))
  if(dim(fh)[1] != 0){
    fh_alb <- fh |>
      dplyr::filter(fih_year1 >= from & fih_year1 <= to) |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    aoi_alb <- aoi[['aoi']] |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    dat_list <- list(fh_alb = fh_alb,
                     aoi_alb = aoi_alb,
                     aoi_name = aoi[['aoi_name']],
                     period = c(from,to))
    return(dat_list)
  } else {
    stop("There is no fire history data for that location")
  }
}

assemble_data2 <- function(fire_path, from, to, aoi){
  # Am I a corporate gdb?
  if(stringr::str_detect(fire_path, pattern = ".gdb")){
    dat_list <- gdb_assembler(fire_path, from, to, aoi)
  } else if(stringr::str_detect(fire_path, pattern = ".shp")){
    dat_list <- shp_assembler(fire_path, from, to, aoi)
  } else {
    stop("I can't work with that file type")
  }
}




fire_path <- "Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/thinning/DBCA_FireHistory_NJF_1987to2017.shp"
fire_path <- "V:/GIS1-Corporate/Data/GDB/Fire/Burn_data/Fire_Data_Burns.shp"
fire_path <- "V:/GIS1-Corporate/Data/GDB/Fire/Burn_data/Fire_Data_Burns.json"
stuff <- assemble_data2(fire_path = fire_path, from = 1987, to = 2017, aoi = l)

