library(tidyverse)
library(sf)

below_ld_pt <- data.frame(structure_name = "gage_below_long_draw", lat = 40.503744, lng = -105.770969) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

above_jw_pt <- data.frame(structure_name = "gage_above_joe_right", lat = 40.5399822, lng = -105.88279) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

below_jw_pt <- data.frame(structure_name = "gage_below_joe_right", lat = 40.5619264, lng = -105.8639) %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326)

rb <- bind_rows(below_ld_pt, above_jw_pt, below_jw_pt)
saveRDS(rb, "gage_pts.rds")
mapview::mapview(rb)
below_ld <- readRDS("daily_discharge_below_long_draw.rds")
below_jw <- readRDS("daily_discharge_below_joe_wright.rds")
above_jw <- readRDS("daily_discharge_above_joe_wright.rds")



highchart() %>%
  hc_plotOptions(
    line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3), area = list(marker = list(enabled = FALSE)))%>%
  hc_title(text = "Daily discharge", style = list(color = "black", fontSize = 20, fontWeight = "bold")) %>%
  hc_xAxis(
    tickInterval = 300, categories = unique(below_ld$date),
    title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  hc_yAxis(
    title = list(text = "Discharge (AF)", style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  hc_legend(itemStyle = list(fontSize = 14, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = below_jw,
    hcaes(x = date, y = discharge_af_total),
    type = 'line',
    name = "Gage above Joe Wright",
    yAxis = 0, fillOpacity = 0.6, showInLegend = T) %>%
  hc_add_series(
    data = above_jw,
    hcaes(x = date, y = discharge_af_total),
    type = 'line',
    name = "Gage below Joe Wright",
    yAxis = 0, fillOpacity = 0.6, showInLegend = T) %>%
  hc_add_series(
    data = below_ld,
    hcaes(x = date, y = discharge_af_total),
    type = 'line',
    name = "Gage below Long Draw",
    yAxis = 0, fillOpacity = 0.6, showInLegend = T) %>%
  hc_navigator(
    enabled = TRUE, outlineColor = "gray", outlineWidth = 2,
    series = list(color = "black",lineWidth = 2,type = "areaspline",  fillColor = "white"),
    handles = list( backgroundColor = "grey",  borderColor = "black")) %>%
  hc_colors(c("#EF798A", "#4F7CAC",  "#162521")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)

  viridisLite::turbo(n = 3)


