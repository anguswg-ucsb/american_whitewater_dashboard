library(tidyverse)
library(sf)
library(highcharter)

struct_name <- "long_draw"

release <- res_diversions %>%
  filter(
    # structure_name == struct_name,
         water_type == "releases") %>%
  mutate(across(where(anyNA), ~ replace_na(., 0))) %>%
  arrange(date) %>%
  mutate(
    year         = lubridate::year(date),
    month        = lubridate::month(date),
    year_factor  = factor(year),
    release_vol  = vol
  ) %>%
  mutate(
    release_freq = case_when(
      release_vol == 0 ~ 0,
      release_vol > 0 ~ 1
    )
  )

rel_mean <- long_draw_release %>%
  group_by(month) %>%
  summarise(releases = sum(release_vol, na.rm = T))

storage <- res_storage %>%
  # filter(structure_name == struct_name) %>%
  mutate(
    date         = lubridate::mdy(date),
    year         = lubridate::year(date),
    month        = lubridate::month(date),
    year_factor  = factor(year),
    date_my      = as.Date(paste0(year, "-", month, "-01"))
  ) %>%
  filter(year >= 1973)

mod_rel <- long_draw_release

mod_data <- left_join(dplyr::select(release, -vol), dplyr::select(storage, structure_name, volume, date_my), by = c("date" = "date_my")) %>%
  dplyr::select(date, release_vol, storage_vol = volume)
mod_data2  <- mod_data %>%
  mutate(dstorage = storage_vol - lag(storage_vol),
         inflow   = dstorage + release_vol)
  # mutate(
  #   scale_release = scale(release_vol),
  #   scale_storage = scale(volume)
  #   )
ggplot() +
  geom_col(data = mod_data, aes(x = date, y = release_vol))
lm_mod <- lm(scale_storage~scale_release, data = mod_data)
summary(lm_mod)
# mutate(across(where(anyNA), ~ replace_na(., 0))) %>%
  # arrange(date)

long_draw_release %>%
  mutate(
    release_freq = case_when(
      vol == 0 ~ 0,
      vol > 0 ~ 1
    )
  )
library(dataRetrieval)
library(nwmTools)

gage <- nwmHistoric::readNWMdata(comid = 2902747)
library(nwmHistoric)
nwmHistoric::readNWMdata()
climateR::param_meta$loca








