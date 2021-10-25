
library(tidyverse)
library(lubridate)

# joe wright --> chambers
# barnes     --> chambers outflow
# long_draw  --> chambers outflow

# sum inflows & outflows for all reservoirs
total_flows <- water_balance %>%
  mutate(year = year(date)) %>%
  filter(year > 2000, year < 2005) %>%
  group_by(date) %>%
  summarize(
    inflow  = sum(inflow_abs),
    outflow = sum(releases)
  ) %>%
  pivot_longer(cols = c(inflow, outflow), names_to = "flow_type", values_to = "volume")

# plot sum inflows & outflows for all reservoirs
highchart() %>%
  hc_plotOptions(line   = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3)) %>%
  # hc_title(text = "Change in reservoir storage",
  #          style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
  # hc_xAxis(tickInterval = 60,
  #          categories = as.list(inflows$date),
  #          title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
  #          labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  # hc_yAxis(title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
  #          labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  hc_yAxis_multiples(create_yaxis(naxis = 2, heights = c(1, 1))) %>%
  hc_legend(itemStyle = list(fontSize = 14, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = total_flows,
    hcaes(x = date, y = inflow),
    type = 'area',
    name = "Inflows",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = total_flows,
    hcaes(x = date, y = outflow),
    type = 'area',
    name = "Outflows",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8)

# dstorage, inflows, outflows all reservoirs
sub_wb <- water_balance %>%
  mutate(
    year = year(date),
    date = as.character(date)) %>%
  # filter(year > 2000, year < 2005) %>%
  rename(Inflow = inflow, Outflow = releases, "Change in storage" = dstorage) %>%
  pivot_longer(cols = c(Inflow, Outflow, "Change in storage"), names_to = "flow_type", values_to = "volume")


highchart() %>%
  hc_add_series(
    data = c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6),
    type = "column"
  ) %>%
  hc_title(
    text = "This is a title with <i>margin</i> and <b>Strong or bold text</b>",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  )
highchart() %>%
  hc_plotOptions(line   = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 4),
                 area   = list(marker = list(enabled = FALSE)))%>%
  hc_title(text = "Water balance<br>(Change in storage = Inflows - Outflows)",
           style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
  hc_xAxis(tickInterval = 60,
           categories = unique(sub_wb$date),
           title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
           labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  hc_yAxis(title = list(text = "Water volume", style = list(fontSize = 14, fontWeight = "bold", color = "black")),
           labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  # hc_yAxis_multiples(create_yaxis(naxis = 4, heights = c(1, 1),
                                  # title = list(text = "Joe Wright", "Barnes", "Long Draw", "Chambers Lake"))) %>%
  # hc_legend(enabled = F) %>%
  hc_legend(itemStyle = list(fontSize = 14, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = filter(sub_wb, structure_name == "joe_wright"),
    hcaes(x = date, y = volume,
          group = flow_type
          ),
    type = 'line',
    # name = "Outflows",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.6, showInLegend = T) %>%
 # hc_navigator(
 #    enabled = TRUE,
 #    outlineColor = "gray",
 #    outlineWidth = 2,
 #    series = list(
 #      color = "black",
 #      lineWidth = 2,
 #      type = "areaspline", # you can change the type
 #      fillColor = "white"
 #    ),
 #    handles = list(
 #      backgroundColor = "grey",
 #      borderColor = "black")) %>%
  # hc_colors(c("#70a7ff", "#6ec439", "#ff708d")) %>%
  # hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
  hc_add_series(
    data = filter(sub_wb, structure_name == "barnes"),
    hcaes(x = date, y = volume,  group = flow_type),
    type = 'area',
    # name = "Outflows",
    yAxis = 1,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = filter(sub_wb, structure_name == "long_draw"),
    hcaes(x = date, y = volume,  group = flow_type),
    type = 'area',
    # name = "Outflows",
    yAxis = 2,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8, showInLegend = F) %>%
  hc_add_series(
    data = filter(sub_wb, structure_name == "chambers_lake"),
    hcaes(x = date, y = volume,  group = flow_type),
    type = 'area',
    # name = "Outflows",
    yAxis = 3,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8, showInLegend = F) %>%
  hc_colors(c("darkred", "forestgreen", "dodgerblue", "darkred", "forestgreen", "dodgerblue"))
  hc_add_series(
    data = filter(sub_wb, structure_name == "chambers_lake"),
    hcaes(x = date, y = inflow_abs),
    type = 'area',
    name = "Inflows",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = filter(sub_wb, structure_name == "chambers_lake"),
    hcaes(x = date, y = dstorage),
    type = 'area',
    name = "Change in storage",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8)
  #  hc_navigator(
  #     enabled = TRUE,
  #     outlineColor = "gray",
  #     outlineWidth = 2,
  #     series = list(
  #       # data = subdate,
  #       color = "black",
  #       lineWidth = 2,
  #       type = "areaspline", # you can change the type
  #       fillColor = "white"
  #     ),
  #     handles = list(
  #       backgroundColor = "grey",
  #       borderColor = "black")) %>%
  # hc_rangeSelector()
  # hc_colors(c("darkred", "forestgreen", "dodgerblue")) %>%
  # hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
  hc_add_series(
    data = filter(sub_wb, structure_name == "long_draw"),
    hcaes(x = date, y = releases),
    type = 'area',
    name = "Outflow",
    yAxis = 1,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = filter(sub_wb, structure_name == "long_draw"),
    hcaes(x = date, y = inflow_abs),
    type = 'area',
    name = "Inflows",
    yAxis = 1,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = filter(sub_wb, structure_name == "long_draw"),
    hcaes(x = date, y = dstorage),
    type = 'area',
    name = "Change in storage",
    yAxis = 1,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8)

inflows <- water_balance %>%
  mutate(
    id    = 1:n(),
    year  = year(date),
    date  = as.character(date)
    ) %>%
  group_by(date) %>%
  summarise(
    releases = sum(releases),
    inflow   = sum(inflow_abs),

  )
  # filter(year >2000, year <= 2006)
plot(inflows$releases)
dstorage_plot <-
  highchart() %>%
      hc_plotOptions(line   = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3)) %>%
      hc_title(text = "Change in reservoir storage",
               style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
      hc_xAxis(tickInterval = 60,
               categories = as.list(inflows$date),
               title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
               labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
      hc_yAxis(title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
               labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
      # hc_yAxis_multiples(create_yaxis(naxis = 2, heights = c(1, 1))) %>%
      hc_legend(itemStyle = list(fontSize = 14, color = "black", fontWeight = "bold")) %>%
      hc_add_series(
        data = inflows,
        hcaes(x = date, y = dstorage),
        type = 'area',
        name = "Storage volume",
        yAxis = 0,
        # pointRange =  24 * 3600 * 1000,
        fillOpacity = 0.8) %>%
      # hc_add_series(
      #   data = storage,
      #   hcaes(x = year, y = stage),
      #   type = 'line',
      #   name = "Stage",
      #   # pointRange =  24 * 3600 * 1000,
      #   yAxis = 1,
      #   fillOpacity = 0.8) %>%
      hc_colors(c("darkcyan")) %>%
      hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
hc <- highchart(type = "stock") %>%
  hc_add_series(AirPassengers)

hc

hc %>%
  hc_rangeSelector(enabled = FALSE)

hc %>%
  hc_rangeSelector(
    verticalAlign = "bottom",
    selected = 4
  )

highchart() %>%
  hc_plotOptions(
    # column   = list(stacking = "normal"),
    # area   = list(stacking = "normal"),
    line   = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3)) %>%
  hc_title(text = "Reservoir inflows & outflows",
           style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
  # hc_xAxis(
  #   # tickInterval = 60,
  #          categories = as.list(inflows$date),
  #          title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
  #          labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  hc_yAxis(title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
           labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  # hc_yAxis_multiples(create_yaxis(naxis = 2, heights = c(1, 1))) %>%
  hc_legend(itemStyle = list(fontSize = 14, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = inflows,
    hcaes(x = date, y = releases),
    type = 'column',
    name = "Outflows",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = inflows,
    hcaes(x = date, y = inflow),
    type = 'column',
    name = "Inflows",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_navigator(
    enabled = TRUE,
    outlineColor = "gray",
    outlineWidth = 2,
    series = list(
      color = "black",
      lineWidth = 2,
      type = "areaspline", # you can change the type
      fillColor = "white"
      ),
    handles = list(
      backgroundColor = "grey",
      borderColor = "black")) %>%
  hc_colors(c("darkcyan", "darkred")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
