
eq_clean_data <- function(data) {
     data <- data %>%
          dplyr::mutate(
               year_fix = stringr::str_pad(as.character(abs(YEAR)),
                                           width = 4, side = "left", pad = "0"),
               date_paste = paste(year_fix, MONTH, DAY, sep = "-"),
               DATE = lubridate::ymd(date_paste, truncated = 2)) %>%
          dplyr::select(-year_fix, -date_paste)

     lubridate::year(data$DATE) <- data$YEAR

     data <- data %>%
          dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                        LONGITUDE = as.numeric(LONGITUDE))

     data <- eq_location_clean(data)

     data
}

eq_location_clean <- function(data) {
     data <- data %>%
          dplyr::mutate(LOCATION_NAME = LOCATION_NAME %>%
                             stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                             stringr::str_trim("both") %>%
                             stringr::str_to_title())
     data
}
