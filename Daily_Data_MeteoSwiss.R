library(reprex)

library(tidyverse)

# Data Source
file.latest <- "https://data.geo.admin.ch/ch.meteoschweiz.klima/nbcn-tageswerte/VQEA34.csv"

# Read from URL and load into tibble
dat <- readr::read_delim(file.latest, delim = ";", skip = 2, col_names = T, na = "-", col_types = cols())

var.id <- c("gre000d0","hto000d0","nto000d0","prestad0","rre150d0","sre000d0","tre200d0","tre200dn","tre200dx","ure200d0")
var.names <- c("Globalstrahlung; Tagesmittel", "Gesamtschneehöhe; Morgenmessung von 6 UTC", "Gesamtbewölkung; Tagesmittel", 
  "Luftdruck auf Stationshöhe (QFE); Tagesmittel", "Niederschlag; Tagessumme 6 UTC - 6 UTC Folgetag", "Sonnenscheindauer; Tagessumme", 
  "Lufttemperatur 2 m über Boden; Tagesmittel", "Lufttemperatur 2 m über Boden; Tagesminimum",
  "Lufttemperatur 2 m über Boden; Tagesmaximum", "Relative Luftfeuchtigkeit 2 m über Boden; Tagesmittel")

# Process (rename, gather into long format, rename)
dat <- dat %>% 
  dplyr::mutate(time = lubridate::ymd(time)) %>%
  tidyr::gather("id_var", "value", -stn, -time) %>%
  mutate(var_name = var.names[match(id_var, var.id)])

# Filter data and plot timeseries
dat %>%
  filter(stn %in% c("SMA", "SAE"), id_var %in% c("tre200d0", "tre200dn", "tre200dx")) %>%
  ggplot(aes(x = time, y = value, group = var_name)) +
  facet_wrap(~stn, scales = "free_y", ncol = 1) +
  geom_path(aes(colour = var_name)) +
  labs(x = NULL, y = "Temperatur [°C]", title = "Zürich-Fluntern, Säntis", colour = "Variable") +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "bottom", legend.direction = "vertical")

reprex(tidyverse_quiet = TRUE)
