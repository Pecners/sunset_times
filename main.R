library(tidyverse)
library(tigris)
library(sf)
library(httr)
library(glue)
library(lubridate)

counties <- map_df(state.name, function(s) {
  c <- counties(s)
  c |> 
    select(NAME, NAMELSAD) |> 
    mutate(state = s)
})

c_cent <- counties |> 
  mutate(cent = st_centroid(geometry))


cent_coords <- c_cent |> 
  as_tibble() |> 
  select(cent) |> 
  st_sf() |> 
  st_coordinates() |> 
  as_tibble()


sunset_times <- map_df(1:nrow(cent_coords), function(i) {
  x <- cent_coords$X[i]
  y <- cent_coords$Y[i]
  
  url <- glue("https://aa.usno.navy.mil/api/rstt/oneday?date=2023-03-30&coords={y},{x}&dst=true",
              "&tz=-6")
  res <- GET(url)
  # res_ls <- rawToChar(res$content) |> 
  #   jsonlite::fromJSON()
  cat(i, "of", nrow(cent_coords), "\n")
  x <- res$content |> 
    rawToChar() |> 
    jsonlite::fromJSON()
  x$properties$data$sundata |> 
    as_tibble()
})

these <- sunset_times |> 
  filter(phen == "Set") |> 
  transmute(time = as.numeric(hm(time))) |> 
  bind_cols(counties) 

p <- these |> 
  filter(!state %in% c("Alaska", "Hawaii")) |> 
  ggplot(aes(fill = time, geometry = geometry)) +
  geom_sf(color = NA)

ggsave(filename = "plot.png", plot = p)

tz <- read.delim("https://www.weather.gov/source/gis/Shapefiles/County/bp08mr23.dbx", 
                 sep = "|", header = FALSE)
tz_counties <- tz |> 
  select(1:2, 6)

comb <- tibble(
  abb = state.abb,
  name = state.name
)

joined <- left_join(tz_counties,
          comb, 
          by = c("V1" = "abb"))

all <- left_join(these, joined,
          by = c("state" = "name",
                 "NAME" = "V6"))

p <- all |> 
  filter(!state %in% c("Alaska", "Hawaii")) |> 
  ggplot(aes(fill = time, geometry = geometry)) +
  geom_sf(color = NA)

ggsave(filename = "plot.png", plot = p)
