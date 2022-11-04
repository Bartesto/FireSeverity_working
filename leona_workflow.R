## code snippets and functions
library(sf)
library(terra)
library(dplyr)
library(tidyterra)
library(lubridate)
library(here)

# data to be internal in package
load("sev.RData")

# block selection
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

## get block spatially
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
aoi_choice <- DBCA_aoi(choice = location, block = TRUE)

## filter fire history to block selection
## determines whether working with a local fh shape file or corporate data gdb file
make_wkt <- function(aoi, fh_crs){
  aoi_wkt <- sf::st_transform(aoi[['aoi']], fh_crs) |>
    sf::st_bbox() |>
    sf::st_as_sfc() |>
    sf::st_geometry() |>
    sf::st_as_text()
  return(aoi_wkt)
}
historical_ids <- function(histpath){
  allfolds <- list.dirs(histpath, recursive = FALSE, full.names = FALSE)
  folds <- list.dirs(histpath, recursive = FALSE)[stringr::str_starts(allfolds, "[[:digit:]]")]
  all_ids <- character()
  for(i in seq_along(folds)){
    shpn <- list.files(paste0(folds[i], "/inputs"), pattern = ".shp$",
                       full.names = TRUE)
    if(length(shpn != 0)){
      burn_ids <- sf::st_read(shpn, quiet = TRUE) |>
        dplyr::pull(BURNID)
      all_ids <- c(all_ids, burn_ids)
    }
    return(all_ids)
  }
}
gdb_assembler <- function(fire_path, hist_path, fromY, toY, preD, postD, aoi){
  # messenging
  cli::cli_progress_step("Working with the Corporate geodatabase file")
  layer_list <- sf::st_layers(fire_path)
  lyr <- "CPT_FIRE_HISTORY"
  fh_crs <- layer_list$crs[grepl(lyr, layer_list$name)][[1]]
  # match crs and proceed
  wkt_flt <- make_wkt(aoi, fh_crs)
  cli::cli_progress_step("Querying the fire history")
  fh <- sf::st_read(dsn = fire_path, layer= lyr, quiet = TRUE, wkt_filter = wkt_flt)
  # manually rename the geometry column
  attr(fh, "sf_column") <- "shape"
  names(fh) <- tolower(names(fh))
  if(dim(fh)[1] != 0){
    fh_alb <- fh |>
      dplyr::filter(fih_year1 >= fromY & fih_year1 <= toY) |>
      sf::st_make_valid() |>
      sf::st_transform(9473)
    ## make burn ID
    # centroid in GDA94 MGA50
    xyC <- suppressWarnings(sf::st_centroid(fh_alb)) |>
      sf::st_transform(28350) |>
      sf::st_coordinates() |>
      tibble::as_tibble() |>
      dplyr::mutate(across(where(is.numeric), round)) |>
      dplyr::mutate(across(where(is.numeric), as.character)) |>
      dplyr::rename_with(tolower)
    # enforce lower case names
    attr(fh_alb, "sf_column") <- "shape"
    names(fh_alb) <- tolower(names(fh_alb))
    # different var names if gdb or shp as input
    col_ind <- names(fh_alb)[stringr::str_detect(names(fh_alb),
                                                 c("fih_distr|fih_year1"))]
    #add burnid and date biz
    fh_alb <- fh_alb |>
      dplyr::bind_cols(xyC) |>
      dplyr::mutate(BURNID = paste0(!!sym(col_ind[2]), "-", !!sym(col_ind[1]), "-",
                                    stringr::str_sub(x, start = -4),
                                    stringr::str_sub(y, start = -4)),
                    date = as.Date(fih_date1),
                    date_end = date + 90,
                    im_start = date - preD,
                    im_end = date + postD)
    # check against historical work
    all_ids <- historical_ids(histpath = hist_path)
    fh_alb <- filter(fh_alb, (BURNID %in% all_ids)==FALSE)
    ## area of interest
    aoi_alb <- aoi[['aoi']] |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    dat_list <- list(fh_alb = fh_alb,
                     aoi_alb = aoi_alb,
                     aoi_name = aoi[['aoi_name']],
                     period = c(fromY,toY))
    cli::cli_progress_done()
    return(dat_list)
  } else {
    stop("There is no fire history data for that location")
  }

}
shp_assembler <- function(fire_path, hist_path, fromY, toY, preD, postD, aoi){
  # messenging
  cli::cli_progress_step("Working with a shape file")
  fname <- tools::file_path_sans_ext(basename(fire_path))
  fquery <- paste0('SELECT * from ', fname, ' LIMIT 1')
  fh_crs <- sf::st_crs(sf::read_sf(fire_path, query = fquery))
  # match crs and proceed
  wkt_flt <- make_wkt(aoi, fh_crs)
  cli::cli_progress_step("Querying the fire history")
  fh <- sf::st_read(dsn = fire_path, quiet = TRUE, wkt_filter = wkt_flt)
  names(fh) <- tolower(names(fh))
  if(dim(fh)[1] != 0){
    ## fire history collection
    fh_alb <- fh |>
      dplyr::filter(fih_year1 >= fromY & fih_year1 <= toY) |>
      sf::st_make_valid() |>
      sf::st_transform(9473)
    # make burn ID
    # centroid in GDA94 MGA50
    xyC <- suppressWarnings(sf::st_centroid(fh_alb)) |>
      sf::st_transform(28350) |>
      sf::st_coordinates() |>
      tibble::as_tibble() |>
      dplyr::mutate(across(where(is.numeric), round)) |>
      dplyr::mutate(across(where(is.numeric), as.character)) |>
      dplyr::rename_with(tolower)
    # enforce lower case names
    attr(fh_alb, "sf_column") <- "geometry"
    names(fh_alb) <- tolower(names(fh_alb))
    # different var names if gdb or shp as input
    col_ind <- names(fh_alb)[stringr::str_detect(names(fh_alb),
                                                 c("fih_distr|fih_year1"))]
    # add burnid and date biz
    fh_alb <- fh_alb |>
      dplyr::bind_cols(xyC) |>
      dplyr::mutate(BURNID = paste0(!!sym(col_ind[2]), "-", !!sym(col_ind[1]), "-",
                                    stringr::str_sub(x, start = -4),
                                    stringr::str_sub(y, start = -4)),
                    date = as.Date(lubridate::parse_date_time(fh_alb$fih_date1,
                                                              c("ymd", "dmy"))),
                    date_end = date + 90,
                    im_start = date - preD,
                    im_end = date + postD)
    # check against historical work
    all_ids <- historical_ids(histpath = hist_path)
    fh_alb <- filter(fh_alb, (BURNID %in% all_ids)==FALSE)
    ## area of interest
    aoi_alb <- aoi[['aoi']] |>
      sf::st_make_valid() |>
      sf::st_transform(9473) |>
      terra::vect()
    dat_list <- list(fh_alb = fh_alb,
                     aoi_alb = aoi_alb,
                     aoi_name = aoi[['aoi_name']],
                     period = c(fromY,toY))
    cli::cli_progress_done()
    return(dat_list)
  } else {
    stop("There is no fire history data for that location")
  }
}
assemble_data <- function(fire_path, hist_path , fromY, toY, preD, postD, aoi){
  # Am I a corporate gdb?
  if(stringr::str_detect(fire_path, pattern = ".gdb")){
    dat_list <- suppressWarnings(gdb_assembler(fire_path, hist_path, fromY, toY,
                                               preD, postD, aoi))
    if(length(unique(dat_list$fh_alb$BURNID)) - nrow(dat_list$fh_alb) != 0){
      cli::cli_alert_danger(cli::bg_red(style_bold(col_white("There are duplicate BURNIDs in this selection"))))
    }
    return(dat_list)
  } else if(stringr::str_detect(fire_path, pattern = ".shp")){
    dat_list <- shp_assembler(fire_path, hist_path, fromY, toY, preD, postD, aoi)
    if(length(unique(dat_list$fh_alb$BURNID)) - nrow(dat_list$fh_alb) != 0){
      cli::cli_alert_danger(cli::bg_red(style_bold(col_white("There are duplicate BURNIDs in this selection"))))
    }
    return(dat_list)
  } else {
    cli::cli_alert_danger("I'm sorry I can't work with that file")
  }
}

path <- "Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/thinning/DBCA_FireHistory_NJF_1987to2017.shp"
path <- "V:/GIS1-Corporate/Data/GDB/Fire/Burn_data/Fire_Data_Burns.gdb"
path <- "V:/GIS1-Corporate/Data/GDB/Fire/Burn_data/Fire_Data_Burns.json"

histpath <- "Z:/DEC/Prescribed_Bushfire_Outcomes_2018-134/DATA/Working/Historical"

fire_path = path
hist_path = histpath
fromY = 1987
toY = 2017
preD = 400
postD = 400
aoi = aoi_choice

fh_dat <- assemble_data(fire_path = path, hist_path = histpath, fromY = 1987, toY = 2017, preD = 400,
                        postD = 400, aoi = aoi_choice)

clean_export <- function(dat, location){
  make_dirs(location)


}
## last function
# make folders
# export clean shape
# export clean dates csv
# export shp by burn


dat = fh_dat


####
## folder creation
make_dirs <- function(dat, out_path){
  loc <- dat$aoi_name
  dir_name <- paste0(Sys.Date(), "_", loc)
  if (!dir.exists(dir_name)) {dir.create(dir_name)}
  fname <- paste0("./", dir_name, "/fireSelection")
  mname <- paste0("./", dir_name, "/models")
  iname <- paste0("./", dir_name, "/inputs/shpByBurn")
  if (!dir.exists(fname)) {dir.create(fname)}
  if (!dir.exists(mname)) {dir.create(mname)}
  if (!dir.exists(iname)) {dir.create(iname, recursive = TRUE)}
}
make_dirs(dat = fh_dat, out_path = ".")
