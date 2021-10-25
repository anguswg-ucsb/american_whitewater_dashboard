library(tidyverse)
library(sf)

res_pts         <- readRDS("reservoir_pts.rds")

barnes <- data.frame(structure_name = "barnes", wdid = "0303683", lat = 40.598309, lng = -105.833240) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)
res_pts2 <- bind_rows(res_pts, barnes)
saveRDS(res_pts2, "reservoir_pts2.rds")
mapview::mapview(res_pts2)
res_stage       <- readRDS("reservoir_stage3.rds")

res_diversions  <- readRDS("reservoir_diversions.rds") %>%
  dplyr::select(-structure_name) %>%
  rename("structure_name" = "name")

res_wdid <- res_diversions %>%
  group_by(structure_name) %>%
  slice(n = 3) %>%
  dplyr::select(wdid, structure_name)
rm(res_names)

res_pts2 <- left_join(res_pts, res_wdid, by = "structure_name")
saveRDS(res_pts2, "reservoir_pts.rds")
