library(tidyverse)
library(ggmap)
library(suncalc)
library(lubridate)
library(RColorBrewer) 

theme_set(theme_minimal())

register_google(key = Sys.getenv("GL_API_KEY"))
cph <- geocode("Copenhagen")

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


# My colors
day_color    <- brewer.pal(8, name = "YlOrRd")[3]
diff_color   <- brewer.pal(8, name = "YlOrRd")[4]
sleep_color <- gray(level = 0, alpha = 0.05)

pl <- sun %>%
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
  labs(y = "Klokken", x = "Måned") +
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
    x = year_start + days(5),
    y = day_mid,
    label = "Sommetid flytter soltimer\nfra tidlig morgen til aften",
    hjust = 0,
    vjust = 0,
    size = 3
  ) +
  geom_curve(
    x = year_start + weeks(11),
    y = day_mid - minutes(30),
    xend = summer_start,
    yend = day_start + minutes(200),
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) + 
  geom_curve(
    x = year_start + weeks(21),
    y = day_mid + minutes(15),
    xend = summer_end,
    yend = day_mid + minutes(260),
    arrow = arrow(length = unit(1.5, "mm"), type = "closed")
  ) +
theme(
  legend.position = c(0.95, 0.95),
  legend.justification = c(1, 1),
  legend.title = element_blank(),
  legend.text = element_text(size = 8),
  legend.key.size = unit(1, "line")
)


pl
pl + geom_curve(
  x = year_start + weeks(11),
  y = day_mid - minutes(30),
  xend = summer_start,
  yend = day_start + minutes(200),
  arrow = arrow(length = unit(1.5, "mm"), type = "closed")
)

pl + geom_curve(
  x = year_start + weeks(21),
  y = day_mid,
  xend = summer_end,
  yend = day_mid + minutes(250),
  arrow = arrow(length = unit(1.5, "mm"), type = "closed")
)

pl + ylim(day_start, day_end)
pl + coord_cartesian(ylim = c(day_start, day_end))

# Discarded versions
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
