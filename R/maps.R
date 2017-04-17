
eq_map <- function(data, annot_col) {

     m <- leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                                    radius = data$EQ_PRIMARY, weight = 1,
                                    popup = data[[annot_col]])

     m
}


eq_create_label <- function(data) {
     popup_text <- with(data, {
          part1 <- ifelse(is.na(LOCATION_NAME), "",
                          paste("<strong>Location:</strong>",
                                LOCATION_NAME))
          part2 <- ifelse(is.na(EQ_PRIMARY), "",
                          paste("<br><strong>Magnitude</strong>",
                                EQ_PRIMARY))
          part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                          paste("<br><strong>Total deaths:</strong>",
                                TOTAL_DEATHS))
          paste0(part1, part2, part3)
     })
}
