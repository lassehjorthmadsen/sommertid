library(tidyverse)
library(ggmap)
library(suncalc)
library(lubridate)
library(RColorBrewer) 
library(lutz)

theme_set(theme_minimal())

register_google(key = Sys.getenv("GL_API_KEY"))


# Get latitude and longitude for selected cities
file_name <- "capitals_geocoded.csv"

if (file.exists(file_name)) {
  capitals_geocoded <- read.csv(file_name) %>% as_tibble()
  
} else {
  
  # from https://en.wikipedia.org/wiki/List_of_national_capitals_by_latitude 
  # Note, some names are not uniquely geocoded and gets mixed up
  capitals <- 
    read.csv2("capitals.csv", header = FALSE) %>%
    pull(1)
  
  capitals_geocoded <-
    capitals %>% set_names() %>% 
    map_dfr(geocode, .id = "city") %>% 
    mutate(tz = tz_lookup_coords(lat = lat, lon = lon, method = "accurate"))
  
  capitals_geocoded %>% write.csv(file_name, row.names = FALSE)
}


# Values
year_start   <- as_date("2021-01-01")
year_end     <- as_date("2021-12-31")
get_up_at    <- as.POSIXct("06:30:00", format = "%H:%M:%S")
to_bed_at    <- as.POSIXct("23:00:00", format = "%H:%M:%S")
summer_start <- as_date("2021-03-28") 
summer_end   <- as_date("2021-10-31")
day_start    <- as.POSIXct("03:00:00", format = "%H:%M:%S")
day_end      <- as.POSIXct("23:30:00", format = "%H:%M:%S")
day_mid      <- as.POSIXct("12:00:00", format = "%H:%M:%S")

my_cities <- c("Nairobi", "Bangkok", "New Delhi", "Madrid", "Rome", "London", "Copenhagen", "Oslo", "Nuuk")

# All sun-time data for all days for all cities
df <- capitals_geocoded %>% 
  distinct() %>% 
  group_by(city) %>% 
  group_split() %>% 
  map(~ bind_cols(date = seq(year_start, year_end, by = "days"), .x)) %>% 
  map_dfr(~ getSunlightTimes(data = .)) %>% 
  as_tibble() %>% 
  left_join(distinct(capitals_geocoded), by = c("lon", "lat")) 

# To-do:
# 1) get dates for daylight saving times; depending on year and location   
# 2) get a nice sample of cities  
# 3) Visualize span of sunrise /golden hour/dusk /twilight, etc

# Selected subset             
sun <- df %>%
  select(city, date, sunrise, sunset) %>%
  filter(city %in% my_cities) %>% 
  mutate(across(
    where(is.POSIXct),
    ~ .x %>%
      strftime(format = "%H:%M:%S") %>%
      as.POSIXct(format = "%H:%M:%S")
  )) %>%
  mutate(across(
    where(is.POSIXct),
    ~
      if_else(
        date >= summer_start & date < summer_end,
        .x - hours(1),
        .x
      ),
    .names = "{.col}_raw"
  ))


# My colors
day_color   <- brewer.pal(8, name = "YlOrRd")[3]
diff_color  <- brewer.pal(8, name = "YlOrRd")[4]
sleep_color <- gray(level = 0, alpha = 0.1)
my_city     <- "Copenhagen"

# Calculate gain in awake-daylight-time
gain <- sun %>% 
  filter(city == my_city) %>% 
  rowwise() %>% 
  mutate(awake_daylight_dst = min(sunset, to_bed_at) - max(sunrise, get_up_at),
         awake_daylight_no_dst = min(sunset_raw, to_bed_at) - max(sunrise_raw, get_up_at),
         daylight_gain = awake_daylight_dst - awake_daylight_no_dst) %>% 
  ungroup() %>% 
  pull(daylight_gain) %>% 
  sum() %>% 
  as.numeric()

# Plot for Cph
pl <- sun %>%
  filter(city == my_city) %>%
  mutate(fill = day_color) %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = sunrise, ymax = sunset, fill = "Dagslys"), show.legend = T) +
  geom_ribbon(aes(ymin = sunrise_raw, ymax = sunrise, fill = "Flyttet dagslys")) +
  geom_ribbon(aes(ymin = sunset_raw, ymax = sunset), fill = diff_color) +
  scale_fill_manual(values = c(day_color, diff_color)) +
  scale_y_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = c(day_start, day_end),
    expand = c(0, 0)
  ) +
  scale_x_date(
    date_breaks = "months",
    date_minor_breaks = "month",
    date_labels = "%B",
    expand = c(0, 0)
  ) +
  annotate(
    "rect",
    xmin = year_start,
    xmax = year_end,
    ymin = day_start,
    ymax = get_up_at,
    fill = sleep_color
  ) +
  annotate(
    "rect",
    xmin = year_start,
    xmax = year_end,
    ymin = to_bed_at,
    ymax = day_end,
    fill = sleep_color
  ) +
  annotate(
    "text",
    x = year_start + days(5),
    y = day_start + minutes(15),
    label = "Mange sover\ni dette tidsrum",
    hjust = 0,
    vjust = 0,
    size = 3
  ) +
  annotate(
    "text",
    x = year_start + weeks(25),
    y = day_mid,
    label = "Sommetid flytter soltimer\nfra tidlig morgen til aften",
    hjust = 1,
    vjust = 0,
    size = 3
  ) +
  geom_segment(
    x = year_start + weeks(26),
    y = day_start + hours(1),
    xend = year_start + weeks(26),
    yend = day_end - hours(2),
    color = grey(0.3),
    linetype = 2,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8),  
    legend.key.size = unit(1, "line"),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t =  0, r = 10, b = 0, l = 0))
  ) +
  labs(title = "Sommertid flytter timer med dagslys til det tidsrum, hvor flest er vågne",
       subtitle = paste0("Graf for København. Sommertid flytter over året ", round(gain, 0), " timer fra nat/tidlig morgen til dagtimer"),
       caption = "Grafik: @lassehmadsen. github.com/lassehjorthmadsen/sommertid",
       y = "Klokken", x = "Måned")
  
pl

ggsave("sommertid_cph.png", plot = pl, width = 200, height = 160, units = "mm")


# Plot for selected cities
pl2 <- sun %>%
  mutate(fill = day_color) %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = sunrise, ymax = sunset, fill = "Dagslys"), show.legend = T) +
  geom_ribbon(aes(ymin = sunrise_raw, ymax = sunrise, fill = "Flyttet dagslys")) +
  geom_ribbon(aes(ymin = sunset_raw, ymax = sunset), fill = diff_color) +
  scale_fill_manual(values = c(day_color, diff_color)) +
  scale_y_datetime(
    date_breaks = "2 hours",
    date_labels = "%H:%M",
    limits = c(day_start, day_end),
    expand = c(0, 0)
  ) +
  scale_x_date(
    date_breaks = "months",
    date_minor_breaks = "month",
    date_labels = "%B",
    expand = c(0, 0)
  ) +
  facet_wrap(facets = vars(city))
  
pl2

  
  annotate(
    "rect",
    xmin = year_start,
    xmax = year_end,
    ymin = day_start,
    ymax = get_up_at,
    fill = sleep_color
  ) +
  annotate(
    "rect",
    xmin = year_start,
    xmax = year_end,
    ymin = to_bed_at,
    ymax = day_end,
    fill = sleep_color
  ) +
  annotate(
    "text",
    x = year_start + days(5),
    y = day_start + minutes(15),
    label = "Mange sover\ni dette tidsrum",
    hjust = 0,
    vjust = 0,
    size = 3
  ) +
  annotate(
    "text",
    x = year_start + weeks(25),
    y = day_mid,
    label = "Sommetid flytter soltimer\nfra tidlig morgen til aften",
    hjust = 1,
    vjust = 0,
    size = 3
  ) +
  geom_segment(
    x = year_start + weeks(26),
    y = day_start + hours(1),
    xend = year_start + weeks(26),
    yend = day_end - hours(2),
    color = grey(0.3),
    linetype = 2,
    arrow = arrow(length = unit(2, "mm"), type = "closed")
  ) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    plot.caption = element_text(size = 8),  
    legend.key.size = unit(1, "line"),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = margin(t =  0, r = 10, b = 0, l = 0))
  ) +
  labs(title = "Sommertid flytter timer med dagslys til det tidsrum, hvor flest er vågne",
       subtitle = paste0("Graf for København. Sommertid flytter over året ", round(gain, 0), " timer fra nat/tidlig morgen til dagtimer"),
       caption = "Grafik: @lassehmadsen. github.com/lassehjorthmadsen/sommertid",
       y = "Klokken", x = "Måned")
