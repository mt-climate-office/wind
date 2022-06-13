library(rgee)
library(reticulate)
library(mcor)
library(magrittr)
library(tidyverse)

ee_Initialize()
# # 2. Install geemap in the same Python ENV that use rgee
# py_install("geemap")
# gm <- import("geemap")

flathead <- mcor::mt_tribal_land %>%
  dplyr::filter(Name == "Flathead") %>%
  sf::st_transform(4326) %>%
  dplyr::rename(geometry = SHAPE)

# gridmet_wind <- 
#   ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
#   ee$ImageCollection$filterDate("1979-01-01", "1980-12-31") %>%
#   ee$ImageCollection$map(function(x) x$select("vs"))


flathead_max_wind <-
  purrr::map(1979:2022,
             ~ee_extract(x = ee$ImageCollection("IDAHO_EPSCOR/GRIDMET") %>%
                          ee$ImageCollection$filterDate(paste0(.x,"-01-01"), paste0(.x,"-12-31")) %>%
                          ee$ImageCollection$map(function(x) x$select("vs")), 
                        y = flathead,
                        fun = ee$Reducer$max())
             )

flathead_max_wind_processed <- 
  flathead_max_wind %>%
  dplyr::bind_rows() %>%
  tidyr::pivot_longer(-Name,
                      names_to = "Date",
                      values_to = "Wind Speed") %>%
  dplyr::mutate(Date = 
                  stringr::str_remove(Date, "X") %>%
                  stringr::str_remove("_vs") %>%
                  lubridate::as_date(),
                `Wind Speed` = units::set_units(`Wind Speed`, "m/s") %>%
                  units::set_units("mi/hr")) 

flathead_max_wind_processed_monthly <-
  flathead_max_wind_processed %>%
  dplyr::group_by(Year = lubridate::year(Date),
                  Month = factor(month.abb[lubridate::month(Date)],
                                 ordered = TRUE,
                                 levels = month.abb)) %>%
  dplyr::summarise(`Mean Wind Speed` = mean(`Wind Speed`, na.rm = TRUE),
                   `Max Wind Speed` = max(`Wind Speed`, na.rm = TRUE),
                   `Windy Days` = sum(`Wind Speed` >= units::set_units(20,"mi/hr"), na.rm = TRUE))

flathead_max_wind_processed_monthly %>%
  dplyr::rename(`Mean Wind Speed (mi/hr)` = `Mean Wind Speed`,
                `Max Wind Speed (mi/hr)` = `Max Wind Speed`) %>%
  dplyr::mutate(dplyr::across(`Mean Wind Speed (mi/hr)`:`Max Wind Speed (mi/hr)`,
                              round,
                              digits = 2)) %>%
  readr::write_csv("flathead_wind.csv")

flathead_max_wind_processed_monthly %>%
  dplyr::mutate(dplyr::across(`Mean Wind Speed`:`Max Wind Speed`, units::drop_units)) %>%
  tidyr::pivot_longer(`Mean Wind Speed`:`Windy Days`,
                      names_to = "Variable") %>%
  dplyr::mutate(Variable = factor(Variable,
                                  ordered = TRUE,
                                  levels = c("Mean Wind Speed",
                                             "Max Wind Speed",
                                             "Windy Days"),
                                  labels = c("Mean Wind Speed (mph)",
                                             "Max Wind Speed (mph)",
                                             "Windy Days"))) %>%
  # dplyr::filter(Month == 2) %>%
  ggplot2::ggplot(aes(x = Year,
                      y = value,
                      group = Variable)) +
  geom_line() +
  geom_smooth(method=lm,
              se = FALSE) +
  ylab(NULL) +
  ggplot2::scale_x_continuous(breaks = seq(1980, 2020, 10)) +
  facet_grid(Variable ~ Month,
             scales = "free_y")
  facet_wrap("Variable",
             ncol = 1,
             scales = "free_y")
  
  ggsave("flathead_wind.pdf",
         width = 24,
         height = 12)
