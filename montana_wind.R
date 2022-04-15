library(tidyverse)
library(magrittr)
library(terra)

windy_thresh <-
  units::set_units(20, "mi/hr") %>% 
  units::set_units("m/s") %>% 
  units::drop_units()

count_windy <- 
  function(x){
    sum(x >= windy_thresh)
  }

process_year <- function(year){
  the_rast <- 
    terra::rast(paste0("~/Desktop/gridmet/vs/vs_",year,".nc")) %>%
    magrittr::set_names(.,
                        names(.) %>%
                          stringr::str_remove("wind_speed_day=") %>%
                          as.integer() %>%
                          magrittr::add(lubridate::date("1900-01-01"))
    ) %>%
    terra::crop(sf::st_transform(mcor::mt_state_simple, 4326))
  
  list(mean, max) %>%
    purrr::map(~terra::tapp(the_rast, 
                            index = paste0(year, "-", lubridate::month(names(the_rast))), 
                            fun = .x))  %>%
    c(
      (the_rast >= windy_thresh) %>%
        terra::tapp(index = paste0(year, "-", lubridate::month(names(the_rast))), 
                    fun = sum)
    ) %>%
    magrittr::set_names(c("mean", "max", "windy_days"))
}

montana_wind <-
  1979:2020 %>%
  magrittr::set_names(.,.) %>%
  purrr::map(process_year)




