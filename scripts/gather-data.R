library(tidyverse)
library(sf)
library(rjson)
library(lubridate)
library(zoo)
library(stringr)
library(xml2)


#the code below pulls in all the data

storm_data = data.frame()

for (year in 1970:2023) {
  print(year)
  
  linkname = paste(sep="", "https://www.spc.noaa.gov/climo/summary/", year,"/smooth/NAT/NAT.json")
  filename = paste(sep="", "data/raw/", year ,".json")
  
  print(linkname)
  print(filename)
  
  download.file(linkname, filename)
  
  current_json <- rjson::fromJSON(file = filename)
  
  data_current_df <- as.data.frame(current_json)
  
  data_current_df_clean <- data_current_df %>%
    select(-hail, -wind, -torn) %>%
    mutate(time = "daily") %>%
    pivot_longer(!time, names_to = "date_weather", values_to = "count") %>%
    select(-time) %>%
    separate(col = date_weather, into = c("time", "date", "measure"), sep = "\\.") %>%
    filter(time == "daily") %>%
    pivot_wider(names_from = measure, values_from = count) %>%
    mutate(year = year) %>%
    mutate(full_date = paste(sep="", date, year)) %>%
    mutate(full_date_clean = as.Date(full_date, format = "%m%d%Y")) %>%
    select(full_date_clean, torn, wind, hail)
  
  storm_data <- bind_rows(storm_data, data_current_df_clean)
  
}

for (year in 2024:2025) {
  print(year)
  
  linkname = paste(sep="", "https://www.spc.noaa.gov/climo/summary/", year, "/ruf/NAT/NAT.json")
  filename = paste(sep="", "data/raw/", year ,".json")
  
  print(linkname)
  print(filename)
  
  download.file(linkname, filename)
  
  current_json <- rjson::fromJSON(file = filename)
  
  data_current_df <- as.data.frame(current_json)
  
  data_current_df_clean <- data_current_df %>%
    select(-hail, -wind, -torn) %>%
    mutate(time = "daily") %>%
    pivot_longer(!time, names_to = "date_weather", values_to = "count") %>%
    select(-time) %>%
    separate(col = date_weather, into = c("time", "date", "measure"), sep = "\\.") %>%
    filter(time == "daily") %>%
    pivot_wider(names_from = measure, values_from = count) %>%
    mutate(year = year) %>%
    mutate(full_date = paste(sep="", date, year)) %>%
    mutate(full_date_clean = as.Date(full_date, format = "%m%d%Y")) %>%
    select(full_date_clean, torn, wind, hail)
  
  storm_data <- bind_rows(storm_data, data_current_df_clean)
  
}

max_date = max(storm_data$full_date_clean)
max_date_day_month <- format(max_date, "%m/%d") #just has the month and day (not the year)

storm_data_clean_all <- storm_data %>% 
  mutate(year = year(full_date_clean))

storm_data_by_year_all <- storm_data_clean_all %>% 
  group_by(year) %>% 
  summarise(torn = sum(torn),
            wind = sum(wind),
            hail = sum(hail))

storm_data_by_5yr_all <- storm_data_clean_all %>% 
  filter(year != "1970") %>% 
  mutate(year_group = paste0(
    ((year - 1971) %/% 5) * 5 + 1971,     # Start of bin
    "-", 
    (((year - 1971) %/% 5) * 5 + 1975) # End of bin
  )) %>% 
  group_by(year_group) %>% 
  summarise(torn = sum(torn)/5,
            wind = sum(wind)/5,
            hail = sum(hail)/5)
  

storm_data_by_5yr_all_plot <- ggplot(storm_data_by_5yr_all, aes(x = year_group, y = torn)) +
  geom_bar(stat = "identity")
storm_data_by_5yr_all_plot

storm_data_clean_current <- storm_data %>% 
  mutate(year = year(full_date_clean)) %>% 
  mutate(day_month = format(full_date_clean, "%m/%d")) %>% #makes a column that just has the month and day (not the year)
  mutate(day_month = as.Date(day_month, format = "%m/%d")) %>% #converts the day_month column to a date column (the year doesn't matter because we want to filter for the same dates for every year)
  filter(day_month <= max_date)

storm_data_by_year_current <- storm_data_clean_current %>% 
  group_by(year) %>% 
  summarise(torn = sum(torn),
            wind = sum(wind),
            hail = sum(hail))

storm_data_by_5yr_current <- storm_data_clean_current %>% 
  filter(year != "1970") %>% 
  mutate(year_group = paste0(
    ((year - 1971) %/% 5) * 5 + 1971,     # Start of bin
    "-", 
    (((year - 1971) %/% 5) * 5 + 1975) # End of bin
  )) %>% 
  group_by(year_group) %>% 
  summarise(torn = sum(torn)/5,
            wind = sum(wind)/5,
            hail = sum(hail)/5)

storm_data_by_5yr_current_plot <- ggplot(storm_data_by_5yr_current, aes(x = year_group, y = torn)) +
  geom_bar(stat = "identity")
storm_data_by_5yr_current_plot

write.csv(storm_data_by_5yr_current, "data/csv/storm_data_by_5yr_current.csv", row.names = FALSE)


#TORNADOES XML EXPORT

max_date_pretty <- format(max_date, "%B %d")
max_date_pretty <- str_replace(max_date_pretty, " 0", "")

tornado_data_by_5yr_current <- storm_data_by_5yr_current %>% 
  select(year_group, torn) %>% 
  rename(label = year_group,
         value = torn) %>% 
  mutate(showLabel = 1) %>% 
  mutate(showValue = 1) %>% 
  mutate(valueToShow = paste0(round(as.numeric(value), digits=0)))

#get labels for x axis
years <- as.character(tornado_data_by_5yr_current$label)
torn_labels = ""
for (year in years) {torn_labels = paste0(torn_labels, "|", year)}
torn_labels = str_replace(torn_labels, "\\|","")

value_max <- max(tornado_data_by_5yr_current$value)

#convert data to XML

#variables 
xml_title <- paste0("Tornado reports Jan 1 - ", max_date_pretty)
xml_subtitle <- "Five-year average"
xml_xaxis <- torn_labels #labels for x axis, only fill out in necessary
xml_yaxis <- " " #labels for y axis, only fill out in necessary
xml_ymax <-  value_max #float value for max value
xml_source <- "NOAA/National Weather Service"
xml_date <- " "
xml_type <- "bar" #line, bar, pie, etc
xml_qualifier <- "2024 and 2025 data is provisional" #one line note, if needed


# Create chart node
torn_chart <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(torn_chart, "title", xml_title)
xml_add_child(torn_chart, "subtitle", xml_subtitle)
xml_add_child(torn_chart, "type", xml_type)
xml_add_child(torn_chart, "x-axis", xml_xaxis)
xml_add_child(torn_chart, "y-axis", xml_yaxis)
xml_add_child(torn_chart, "y-max", xml_ymax)

# Add data rows
for (i in 1:nrow(tornado_data_by_5yr_current)) {
  row_node <- xml_add_child(torn_chart, "dataPoint")
  for (col_name in names(tornado_data_by_5yr_current)) {
    xml_add_child(row_node, col_name, as.character(tornado_data_by_5yr_current[i, col_name]))
  }
}


xml_add_child(torn_chart, "source", xml_source)
xml_add_child(torn_chart, "date", xml_date)
xml_add_child(torn_chart, "qualifier", xml_qualifier)


# Write XML to file
write_xml(torn_chart, "data/xml/tornadoes_chart.xml")