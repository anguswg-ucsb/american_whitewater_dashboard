library(tidyverse)
library(sf)
library(highcharter)
res_pts         <- readRDS("reservoir_pts.rds")

res_stage       <- readRDS("reservoir_stage.rds")

res_diversions  <- readRDS("reservoir_diversions.rds") %>%
  dplyr::select(-structure_name) %>%
  rename("structure_name" = "name")

struct_name <- "long_draw"

diversions <- res_diversions %>%
  filter(structure_name == struct_name, water_type == "diversions") %>%
  mutate(across(where(anyNA), ~ replace_na(., 0)))

tot_divers <- diversions %>%
  group_by(irr_year) %>%
  summarize(total_diversions = sum(vol, na.rm = T))

releases <- res_diversions %>%
  filter(structure_name == struct_name, water_type == "releases")

high
ggplot() +
  # geom_line(data = tot_divers, aes(x = irr_year, y = total_diversions), col = "red", size = 2) +
  geom_col(data = diversions, aes(x = date, y = vol))
library(xts)
library(dygraphs)

ts <- xts::xts(as.data.frame(diversions$vol), order.by = diversions$date, tz = "UTC")
dygraph(ts, ylab = "Water diversions")  %>%
  dyHighlight(highlightCircleSize = 4.5,
              highlightSeriesBackgroundAlpha = 0.5) %>%
  dyOptions(colors = c("darkcyan"),
            fillGraph = TRUE)

highchart() %>%
  hc_plotOptions(
    line = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3),
    scatter = list(marker = list(symbol = "circle", radius = 5))) %>%
  hc_title(text = "Monthly water releases", style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
  hc_yAxis(
    title = list(text = "Water volume", style = list(fontSize = 16, fontWeight = "bold", color = "black")),
    labels = list(style = list(fontSize = 14, color = "black", fontWeight = "bold"))) %>%
  hc_xAxis(
    title = list(text = "Date", style = list(fontSize = 16, fontWeight = "bold", color = "black")),
    labels = list(style = list(fontSize = 14, color = "black", fontWeight = "bold")),
    reversed = T) %>%
  hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = releases,
    hcaes(x = date, y = vol),
    type = 'line',
    name = "Water releases", # yAxis = 0,
    fillOpacity = 0.8) %>%
  hc_colors(c("darkred")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)



