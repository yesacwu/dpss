### Mask mandate date by state ###
# loading and combining our shape and data files 
statemap <- st_read("cb_2018_us_state_500k.shp")
mandatedates <- State_Mask_Mandate %>% select(1:3) #creating a new dataframe with just state names and mask mandate 'begin' dates
map_mandatedates_combined <- full_join(statemap, mandatedates, by = c("STATEFP" = "state_fips")) #ran into an issue where Nebraska's FIPS code was mislabled

## cleaning ##
# dropping State names from the left table to remove redundant column. Also to make it easier to remove US territories from our data.
map_mandatedates_combined <- subset(map_mandatedates_combined, select = -c(NAME)) 

# dropping US territories
map_mandatedates_combined <- drop_na(map_mandatedates_combined, state_name)

# converting our dates from chr to date format 
mask_policy_start_date <- mdy(map_mandatedates_combined$mask_policy_start)
map_mandatedates_combined <- map_mandatedates_combined %>%                  
  mutate(mask_policy_start = mask_policy_start_date) 

# plot
plot(map_mandatedates_combined["mask_policy_start"], xlim=c(-179.14891, 0), ylim=c(18.91036, 71.36516)) # adjusting bounds

### covid-19 cases by day per state ###
# importing data 
library(viridis)
library(readr)
covid_us_counties <- read_csv("~/Documents/DPSS /Capstone/shapefiles/NYT-COVID-us-counties.csv")

# aggregating county data by state and date
cases_by_day <- covid_us_counties %>% 
  group_by(state, date) %>%
  summarize(case_count = sum(cases)) %>%
  mutate(date = as.Date(date))

# creating the base plot 
case_plot <- cases_by_day %>%
  ggplot(aes(x = date, y = case_count, group = state, color = state)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Covid-19 Cases by Day") +
  xlab("Date") +
  ylab("Number of Cases") + 
  labs(color = "States")

# adjusting axes and scale, adding markers g
case_plot <- case_plot + 
  scale_x_date(date_breaks = "months", date_minor_breaks = "days", date_labels = "%b", limits = as.Date(c('2020-01-01', '2020-08-01'))) +
  scale_y_continuous(trans = "log2") +
  geom_vline(xintercept = as.Date("2020-07-26"), col = "black") +
  annotate("segment", x = as.Date("2020-01-21"), xend = as.Date("2020-01-21"), y = 1, yend = 16, colour = "black") +
  annotate("text", x = as.Date("2020-01-21"), y = 18, label = "January 21, 2020", size = 3) + 
  annotate("text", x = as.Date("2020-08-01"), y = 16384, label = "July 26, 2020", size = 3)
case_plot






