## Internal data

library(sf)
library(dplyr)

## Tenure boundaries
tenure_shp <- sf::st_read("V:/GIS1-Corporate/Data/GDB/SCDB_Tenure/State_Tenure/Tenure.gdb",
                          layer = "CPT_DBCA_LEGISLATED_TENURE") |>
  sf::st_make_valid() |>
  sf::st_transform(9473)

tenure <- unique(tenure_shp$LEG_NAME)

DBCA_tenure <- tenure[!grepl("^\\s+$", tenure)]

## Forest blocks
blocks_shp <- sf::st_read("V:/GIS1-Corporate/data/GDB/DBCA_Operations/DBCA_Operations.gdb",
                          layer = "CPT_FOREST_BLOCKS") |>
  sf::st_make_valid() |>
  sf::st_transform(9473)

blocks <- unique(blocks_shp$SFB_BLOCK)

DBCA_blocks <- blocks[!grepl("^\\s+$", blocks)]

save(tenure_shp, DBCA_tenure, blocks_shp, DBCA_blocks, file = "sev.RData")
# load("sev.RData")


# usethis::use_data(tenure_shp, DBCA_tenure, blocks_shp, DBCA_blocks, internal = TRUE,
#                   overwrite = TRUE)
