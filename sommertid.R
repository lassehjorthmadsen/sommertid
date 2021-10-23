library(tidyverse)
library(ggmap)
library(suncalc)
library(lubridate)
library(RColorBrewer)

theme_set(theme_minimal())

register_google(key = Sys.getenv("GL_API_KEY"))

year_start   <- as_date("2021-01-01")
year_end     <- as_date("2021-12-31")
get_up_at    <- as.POSIXct("06:30:00", format = "%H:%M:%S")
to_bed_at    <- as.POSIXct("23:00:00", format = "%H:%M:%S")
summer_start <- as_date("2021-03-28") 
summer_end   <- as_date("2021-10-31")
day_start    <- as.POSIXct("03:00:00", format = "%H:%M:%S")
day_end      <- as.POSIXct("23:30:00", format = "%H:%M:%S")

cph <- geocode("Copenhagen")

df <- tibble(date = seq(year_start, year_end, by="days"),
             lat = cph$lat, lon = cph$lon, tz ="CET") 

# To-do:
# 1) get dates for daylight saving times; depending on year and location   
# 2) get a nice sample of cities  
# 3) Visualize span of sunrise /golden hour/dusk /twilight, etc
             
sun <- getSunlightTimes(data = df) %>%
  as_tibble() %>%
  select(date, sunrise, sunset) %>%
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


morning <- tibble(
  x = c(year_start, summer_start, summer_start, summer_end, summer_end),
  y = c(get_up_at, get_up_at, get_up_at - hours(1), get_up_at - hours(1), get_up_at),
  xend = c(summer_start, summer_start, summer_end, summer_end, year_end),
  yend = c(get_up_at, get_up_at - hours(1), get_up_at - hours(1), get_up_at, get_up_at)
)

evening <- tibble(
  x = c(year_start, summer_start, summer_start, summer_end, summer_end),
  y = c(to_bed_at, to_bed_at, to_bed_at - hours(1), to_bed_at - hours(1), to_bed_at),
  xend = c(summer_start, summer_start, summer_end, summer_end, year_end),
  yend = c(to_bed_at, to_bed_at - hours(1), to_bed_at - hours(1), to_bed_at, to_bed_at)
)

sun %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = sunrise_raw, ymax = sunset_raw), 
              fill = "orange", alpha = 0.5) +
  scale_y_datetime(date_breaks = "2 hours", date_labels = "%H:%M") +
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  geom_segment(data = morning, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = evening, aes(x = x, y = y, xend = xend, yend = yend)) +
  labs(y = "Normaltid")


day_color    <- brewer.pal(8, name = "YlOrRd")[3]
diff_color   <- brewer.pal(8, name = "YlOrRd")[4]


sun %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymin = sunrise, ymax = sunset), fill = day_color) +
  geom_ribbon(aes(ymin = sunrise_raw, ymax = sunrise), fill = diff_color) +
  geom_ribbon(aes(ymin = sunset_raw, ymax = sunset), fill = diff_color) +
  scale_y_datetime(date_breaks = "2 hours", date_labels = "%H:%M", 
                   limits = c(day_start, day_end)) +
  scale_x_date(date_breaks = "months", date_minor_breaks = "month", date_labels = "%B") +
  geom_segment(x = year_start, y = get_up_at, xend = year_end, yend = get_up_at, color = "darkblue") +
  geom_segment(x = year_start, y = to_bed_at, xend = year_end, yend = to_bed_at, color = "darkblue") +
  labs(y = "Klokkeslet", x = "Måned")
