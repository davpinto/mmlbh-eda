## Load required packages
library("readr")
library("dplyr")
library("lubridate")

## Read and format data
flights <- read_csv(file = "data/flights2014.csv.zip") %>% 
   na.omit() %>% 
   mutate(dep_time = sprintf("%d-%02d-%02d %02d:%02d", year, month, day, hour, min)) %>% 
   mutate(dep_time = ymd_hm(dep_time, tz = "UTC")) %>% 
   mutate(arr_time = dep_time + minutes(air_time)) %>% 
   select(dep_time, dep_delay, arr_time, arr_delay, origin, dest, distance)

## Save data
write_csv(flights, path = "data/flights.csv")
system("cd data; rm -f flights.csv.zip")
system("cd data; zip flights.csv.zip flights.csv")
system("cd data; rm -f flights.csv")
