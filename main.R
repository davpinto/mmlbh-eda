## Load required packages
library("readr")
library("ggplot2")
library("ggmap")
library("viridis")
library("lubridate")
library("magrittr")
library("tidyr")
library("dplyr")

## Import dataset
flights <- readr::read_csv("./data/flights.csv.zip")
dplyr::glimpse(flights)

# ============================================================================ #
# Combining functions with the pipe operator
# ------------------------------------------

## Average delays (3 ways to combine functions)
# 1. Many names
delays1 <- dplyr::group_by(flights, origin, dest)
delays2 <- dplyr::summarise(delays1, delay = mean(dep_delay), count = n())
delays3 <- dplyr::filter(delays2, origin == "EWR")
delays4 <- dplyr::top_n(delays3, n = 10, wt = delay)
delays  <- dplyr::arrange(delays4, desc(delay))
head(delays, n = 10)
# 2. Nested calls
delays <- dplyr::arrange(
   dplyr::top_n(
      dplyr::filter(
         dplyr::summarise(
            dplyr::group_by(
               flights, origin, dest
            ), delay = mean(dep_delay), count = n()
         ), origin == "EWR"
      ), n = 10, wt = delay
   ), desc(delay)
)
head(delays, n = 10)
# 3. Pipe operator %>%
delays <- flights %>% 
   dplyr::group_by(origin, dest) %>% 
   dplyr::summarise(delay = mean(dep_delay), count = n()) %>% 
   dplyr::filter(origin == "EWR") %>% 
   dplyr::top_n(n = 10, wt = delay) %>% 
   dplyr::arrange(desc(delay))
head(delays, n = 10)

# ============================================================================ #
# Data Visualization: The grammar of graphics and ggplot2
# -------------------------------------------------------

## Visualize delays
dt <- delays %>% 
   dplyr::mutate(delay = round(delay, 2)) %>% 
   tidyr::unite(col = "flight", origin, dest, sep = "-")
head(delays, n = 5)
# 1. Simple bar plot
g <- ggplot(aes(x = flight, y = delay), data = dt) +
   geom_bar(stat = "identity") +
   labs(x = "Flight", y = "Delay (min)", title = "Average Delays", 
        subtitle = "Flights from EWR")
plot(g)
# 2. Beautiful bar plot
g <- ggplot(aes(x = reorder(flight, delay), y = delay, 
                fill = reorder(flight, delay)), data = dt) +
   geom_bar(width = 0.7, stat = "identity", color = "white", size = 0.5) +
   coord_flip() +
   geom_text(aes(label = delay), nudge_y = -3, color = "white", size = 4, 
             fontface = "bold") +
   viridis::scale_fill_viridis(direction = -1, discrete = TRUE, guide = "none") +
   labs(x = "Flight", y = "Delay (min)", title = "Average Delays", 
        subtitle = "Flights from EWR")
plot(g)

## Distribution of delays
dt <- flights %>% 
   dplyr::filter(origin == "EWR") %>% 
   dplyr::group_by(dest) %>% 
   dplyr::summarise(count = n()) %>% 
   dplyr::top_n(n = 10, wt = count)
dt <- flights %>% 
   dplyr::filter(origin == "EWR", dest %in% dt$dest) %>%
   dplyr::filter(dep_delay > 0) %>% 
   dplyr::filter(dep_delay > quantile(dep_delay, 0.1), 
          dep_delay < quantile(dep_delay, 0.9)) %>% 
   dplyr::select(origin, dest, dep_delay)
head(dt, n = 5)
# 1. Simple boxplot
g <- ggplot(aes(x = dest, y = dep_delay), data = dt) +
   geom_boxplot(outlier.shape = NA) +
   labs(x = "Destination", y = "Delay (min)", title = "Distribution of Delays", 
        subtitle = "Flights from EWR")
plot(g)
# 2. Beautiful boxplot
g <- ggplot(aes(x = dest, y = dep_delay, fill = dest), data = dt) +
   geom_jitter(width = 0.2, color = "gray40", alpha = 0.6, size = 1) +
   geom_boxplot(outlier.shape = NA, alpha = 0.6, size = 0.6) +
   viridis::scale_fill_viridis(discrete = TRUE, guide = "none") +
   viridis::scale_color_viridis(discrete = TRUE, guide = "none") +
   labs(x = "Destination", y = "Delay (min)", title = "Distribution of Delays", 
        subtitle = "Flights from EWR")
plot(g)

# ============================================================================ #
# Add new variables
# -----------------

## Average delays along year
table(flights$origin, flights$dest)
dt <- flights %>% 
   dplyr::filter(dep_delay > 0) %>% 
   tidyr::unite("flight", origin, dest, sep = "-") %>% 
   dplyr::group_by(flight) %>% 
   dplyr::summarise(count = n()) %>% 
   dplyr::top_n(n = 10, wt = count)
dt <- flights %>% 
   na.omit() %>% 
   dplyr::filter(dep_delay > 0) %>% 
   tidyr::unite("flight", origin, dest, sep = "-") %>% 
   dplyr::filter(flight == "EWR-LAX") %>% 
   dplyr::mutate(dep_month = lubridate::month(dep_time)) %>% 
   dplyr::group_by(dep_month) %>% 
   dplyr::summarize(delay_mean = median(dep_delay), delay_sd = IQR(dep_delay), 
                    count = n())
g <- ggplot(aes(x = dep_month, y = delay_mean), data = dt) +
   geom_ribbon(aes(ymin = delay_mean - delay_sd, ymax = delay_mean + delay_sd), 
               alpha = 0.2) +
   geom_line(size = 1, color = "#482878FF", alpha = 0.6) +
   geom_point(size = 3, color = "#482878FF", alpha = 0.8) +
   scale_x_continuous(breaks = unique(dt$dep_month)) +
   labs(x = "Month", y = "Delay (min)", title = "Delay Trends", 
        subtitle = "Flight EWR to LAX") +
   theme_bw()
plot(g)

## Number of delays by weekday and month
dt <- flights %>% 
   na.omit() %>% 
   tidyr::unite("flight", origin, dest, sep = "-") %>% 
   dplyr::filter(flight == "EWR-LAX") %>% 
   dplyr::mutate(dep_weekday = weekdays(dep_time, abbreviate = TRUE)) %>%
   dplyr::mutate(dep_month = lubridate::month(dep_time)) %>% 
   dplyr::group_by(dep_month, dep_weekday) %>% 
   dplyr::summarise(rate = sum(dep_delay > 0) / n()) %>% 
   dplyr::ungroup() %>% 
   dplyr::mutate(dep_weekday = factor(dep_weekday, c("Dom","Seg","Ter","Qua",
                                                     "Qui","Sex","Sáb"))) %>% 
   dplyr::mutate(dep_month = factor(dep_month, sort(unique(dep_month))))
g <- ggplot(dt, aes(x = dep_month, y = dep_weekday, fill = rate)) +
   geom_tile(color = "gray93", size = 1, alpha = 0.8) +
   geom_text(aes(label = round(rate, 2)), color = "white") +
   viridis::scale_fill_viridis(direction = -1, guide = "none") +
   scale_x_discrete(expand = c(.01, .01)) +
   scale_y_discrete(expand = c(.01, .01)) +
   theme_gray(base_family = "Helvetica", base_size = 14) +
   labs(x = "Month", y = "Weekday")
plot(g)

## Plot airports
airports <- readr::read_csv("./data/airports.csv.zip")
orig <- airports %>% 
   dplyr::filter(faa %in% unique(flights$origin))
dest <- airports %>% 
   dplyr::filter(faa %in% unique(flights$dest))

map <- get_map("United States", zoom = 4, maptype = "terrain", scale = 2, color = "bw")
ggmap(map, extent = "device", darken = 0.1) +
   stat_density2d(data = dest, aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), 
                  size = 0.01, bins = 10, geom = "polygon") + 
   geom_density2d(data = dest, aes(x = lon, y = lat, color = ..level..), 
                  size = 0.5, bins = 15, alpha = 0.8) + 
   geom_point(aes(lon, lat), data = dest, size = 2, shape = 18, alpha = 0.8, color = "black") +
   geom_point(aes(lon, lat), data = orig, size = 2, shape = 13, alpha = 0.8, color = "black") +
   scale_color_gradientn(colors = heat.colors(100)[80:5], guide = "none") + 
   scale_fill_gradientn(colors = heat.colors(100)[80:5], guide = "none") + 
   scale_alpha(range = c(0.1, 0.4), guide = "none")
