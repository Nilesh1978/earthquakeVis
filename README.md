# earthquakeVis - NOAA earthquake visualization package

This is an R package created for the purpose of visualizing NOAA earthquake data. It processes data from [NOAA database](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1). 

## Example

After downloading data from the NOAA database, the package is able to process and visualize them using the following example:

```r
data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
data %>% eq_clean_data() %>%
     filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
     ggplot(aes(x = DATE,
                y = COUNTRY,
                color = as.numeric(TOTAL_DEATHS),
                size = as.numeric(EQ_PRIMARY)
     )) +
     geom_timeline() +
     geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
     theme(
          plot.background = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x = element_line(size = 1),
          axis.ticks.y = element_blank(),
          legend.position = "bottom"
     ) +
     labs(size = "Richter scale value", color = "# deaths")
```

This creates a `ggplot2` object with earthquake timelines and labels grouped by country, colored by number of casualties and sized by magnitude. 

Another example uses leaflet package:

```r
data %>% 
  eq_clean_data() %>% 
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>% 
  dplyr::mutate(popup_text = eq_create_label(.)) %>% 
  eq_map(annot_col = "popup_text")
```

The `leaflet` map includes circles for individual earthquakes with location name, magnitude and number of casualties annotations.

## Author

[Zdenek Kabat](https://github.com/zkabat)
