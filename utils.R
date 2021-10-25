# --- Shiny utils ---
basemap <- function(res_pts, gage_pts, pts = NULL) {

  # ==================================
  # ---- COLOR PALETTES & LABELS -----
  # ==================================

  # Color palette + labels
  # turbo_pal        <- viridisLite::turbo(n = fetch_categories, direction = -1)

  # hsi_cols         <- data.frame(numeric_cols = 0:1)
  # # hsi_pal          <- colorNumeric('YlGnBu', domain = values(hsi_sal3), na.color = NA, reverse = T)
  # hsi_pal          <- colorNumeric('magma', domain =values(hsi_sal3), na.color = NA, reverse = F)

  # ==================================
  # ----------- LEAFLET MAP ----------
  # ==================================
  leaflet() %>%
    # addProviderTiles(providers$Esri.WorldTopoMap, group = "Topographic") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap, group = "Nat. Geo. Topographic") %>%
    # addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addScaleBar("bottomleft") %>%
    addMeasure(position = "bottomright", primaryLengthUnit = "feet",
               primaryAreaUnit = "sqmiles", activeColor = "red", completedColor = "green") %>%
    leafem::addMouseCoordinates() %>%
    addCircleMarkers(
      data        = res_pts,
      weight      = 5,
      radius      = 12,
      fillColor   = "navy",
      opacity     = 1,
      label       = ~structure_name,
      layerId     = ~structure_name
      ) %>%
    addCircleMarkers(
      data        = gage_pts,
      weight      = 3,
      radius      = 10,
      fillColor   = "darkred",
      color       = "darkred",
      opacity     = 1,
      label       = ~structure_name,
      layerId     = ~structure_name
    )
}
