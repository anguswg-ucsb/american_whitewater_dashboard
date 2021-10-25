
# fix dates
res_storage       <- readRDS("reservoir_stage.rds") %>%
  mutate(
    date         = lubridate::mdy(date),
    year         = lubridate::year(date),
    month        = lubridate::month(date),
    year_factor  = factor(year)
  )

storage <- res_storage %>%
  filter(structure_name == struct_name) %>%
  mutate(
    date_my = as.Date(paste0(year, "-", month, "-01")),
    timestamp = datetime_to_timestamp(date_my)
    )

monthly_storage    <- storage %>%
  group_by(year, month) %>%
  summarize(
    volume = mean(volume, na.rm = T),
    stage  = mean(stage, na.rm = T)
  )

annual_storage    <- storage %>%
  mutate(
    date = lubridate::mdy(date),
    year = lubridate::year(date)
  ) %>%
  group_by(year) %>%
  summarize(
    volume = mean(volume, na.rm = T),
    stage  = mean(stage, na.rm = T)
            ) %>%
  mutate(across(where(anyNA), ~ replace_na(., 0)))

storage2    <- res_storage2 %>%
  filter(structure_name == struct_name) %>%
  mutate(
    # date = lubridate::mdy(date),
    year = lubridate::year(date)
  ) %>%
  group_by(year) %>%
  summarize(
    volume = mean(volume, na.rm = T),
    stage  = mean(stage, na.rm = T)
  ) %>%
  mutate(across(where(anyNA), ~ replace_na(., 0)))
highchart() %>%
  hc_plotOptions(line   = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3)) %>%
  hc_title(text = "Reservoir storage & stage",
           style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
  hc_xAxis(
    title = list(style = list(fontSize = 14, fontWeight = "bold", color = "black")),
    labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"))) %>%
  # type = "datetime" reversed = T tickInterval =  50) %>%
  hc_yAxis_multiples(
    list(title = list(text = "Storage volume (AF)",
                      style = list(fontSize = 14, color = "black", fontWeight = "bold")),
         labels=list(style = list(fontSize = 11, color = "black", fontWeight = "bold"), format = '{value} AF'),
         min=0, max=max(storage2$volume),
         showFirstLabel = TRUE,showLastLabel=TRUE,
         opposite = FALSE),
    list(title = list(text = "Stage (ft)", style = list(fontSize = 14, color = "black", fontWeight = "bold")),
         min=0, max = max(storage2$stage),
         labels = list(style = list(fontSize = 11, color = "black", fontWeight = "bold"), format = "{value} ft"), showLastLabel = FALSE,
         opposite = TRUE)) %>%
  # hc_yAxis_multiples(create_yaxis(naxis = 2, heights = c(1, 1))) %>%
  hc_legend(itemStyle = list(fontSize = 14, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = storage2,
    hcaes(x = year, y = volume),
    type = 'column',
    name = "Storage volume",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = storage2,
    hcaes(x = year, y = stage),
    type = 'line',
    name = "Stage",
    # pointRange =  24 * 3600 * 1000,
    yAxis = 1,
    fillOpacity = 0.8) %>%
  hc_colors(c("darkcyan", "darkred")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
# Plot prices and volume with relative height.
datetime_to_timestamp(
     storage$date
      )
highchart() %>%
  hc_plotOptions(
    column = list(pointWidth = 0.1),
    line   = list(marker = list(enabled = FALSE, symbol = "circle"), lineWidth = 3)
    ) %>%
  hc_title(text = "Reservoir storage & stage",
           style = list(color = "black", fontSize = 18, fontWeight = "bold")) %>%
  hc_xAxis(
    type = "datetime"
           # reversed = T
           # tickInterval =  50
           ) %>%
  #   # title = list(style = list(fontSize = 16, fontWeight = "bold", color = "black")),
  # #   labels = list(style = list(fontSize = 14, color = "black", fontWeight = "bold"))) %>%
  # hc_yAxis_multiples(
  #   list(title = list(text = "Storage volume (AF)",
  #                     style = list(fontSize = 16, color = "black", fontWeight = "bold")),
  #        labels=list(style = list(fontSize = 14, color = "black", fontWeight = "bold"), format = '{value} AF'),
  #        min=0, max=max(storage$volume),
  #        showFirstLabel = TRUE,showLastLabel=TRUE,
  #        opposite = FALSE),
  #   list(title = list(text = "Stage (ft)", style = list(fontSize = 16, color = "black", fontWeight = "bold")),
  #        min=0, max = max(storage$stage),
  #        labels = list(style = list(fontSize = 14, color = "black", fontWeight = "bold"), format = "{value} ft"), showLastLabel = FALSE,
  #        opposite = TRUE)) %>%
  hc_yAxis_multiples(create_yaxis(naxis = 2, heights = c(1, 1))) %>%
  hc_legend(itemStyle = list(fontSize = 16, color = "black", fontWeight = "bold")) %>%
  hc_add_series(
    data = storage,
    hcaes(x = timestamp, y = volume),
    type = 'column',
    name = "Storage volume",
    yAxis = 0,
    # pointRange =  24 * 3600 * 1000,
    fillOpacity = 0.8) %>%
  hc_add_series(
    data = storage,
    hcaes(x = timestamp, y = stage),
    type = 'line',
    name = "Stage",
    # pointRange =  24 * 3600 * 1000,
    yAxis = 1,
    fillOpacity = 0.8) %>%
  hc_colors(c("darkcyan", "red")) %>%
  hc_chart(plotBorderWidth = 0.5, plotBorderColor = '#b4b4b4', height = NULL)
annual_storage
ggplot() +
  # geom_line(data = na.omit(monthly_storage), aes(x = year, y = stage), col = "red")
  geom_col(data = monthly_storage, aes(x = year, y = volume))
ggplot() +
  geom_line(data = annual_storage, aes(x = year, y = stage), col = "red")
  geom_col(data = annual_storage, aes(x = year, y = volume))
unique(res_storage$date)
