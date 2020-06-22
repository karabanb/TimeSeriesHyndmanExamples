
#### 2 TIME SERIES GRAPHICS ############################################################################################

library(fpp3)
library(GGally)

#### 2.1. tsibble objects ##############################################################################################

y <- tsibble(Year = 2015:2019, Observation = c(123,39,78,52,110), index = Year)


### Example data sets ### ----------------------------------------------------------------------------------------------

olympic_running

PBS


### Filtering data ### -------------------------------------------------------------------------------------------------

PBS %>% 
  filter(ATC2=='A10') %>% 
  select(Month, Concession, Type, Cost) 


### Grouping data ### --------------------------------------------------------------------------------------------------

a10 <- PBS %>% 
  filter(ATC2=='A10') %>% 
  select(Month, Concession, Type, Cost) %>% 
  summarise(TotalC = sum(Cost)) %>% 
  mutate(Cost = TotalC/1e6)

save(a10, file = 'data/a10.Rdata')


### Read a csv file and convert to a tsibble -----------------------------------------------------------------------

prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")

glimpse(prison)

prison <- prison %>% 
  mutate(quarter = yearquarter(date)) %>% 
  select(-date) %>% 
  as_tsibble(key = c('state', 'gender', 'legal', 'indigenous'), index = quarter)


#### 2.2 TIME SERIES PLOTS #############################################################################################

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class=="Economy")


#### Basic Time Series Plot  -------------------------------------------------------------------------------------------

melsyd_economy %>%
  autoplot(Passengers) +
  labs(title = "Ansett economy class passengers", subtitle = "Melbourne-Sydney") +
  xlab("Year")

load(file = 'data/a10.Rdata')

a10 %>% autoplot(Cost) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") + xlab("Year")


#### Seasonal plots ----------------------------------------------------------------------------------------------------

a10 %>% 
  gg_season(Cost, labels = 'both') +
  ylab('$ milion') +
  ggtitle('Seasonal plot: antidiabetic drug sales')
  

#### Multiple Seasonal Periods -----------------------------------------------------------------------------------------

vic_elec %>% 
  gg_season(Demand, period = "day") + 
  theme(legend.position = "none")

vic_elec %>%
  gg_season(Demand, period = "week") +
  theme(legend.position = "none")

vic_elec %>% 
  gg_season(Demand, period = "year")


#### Seasonal subseries plots ------------------------------------------------------------------------------------------

a10 %>%
  gg_subseries(Cost) +
  ylab("$ million") +
  xlab("Year") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

# holidays example 

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

holidays %>% 
  autoplot(Trips) +
  ylab("thousands of trips") + 
  xlab("Year") +
  ggtitle("Australian domestic holiday nights")

holidays %>% gg_season(Trips) +
  ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")

holidays %>%
  gg_subseries(Trips) + ylab("thousands of trips") +
  ggtitle("Australian domestic holiday nights")


#### Scatterplots ------------------------------------------------------------------------------------------------------

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  xlab("Year: 2014") + 
  ylab(NULL) +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  xlab("Year: 2014") + 
  ylab(NULL) +
  ggtitle("Half-hourly temperatures: Melbourne, Australia")

vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  ylab("Demand (GW)") + 
  xlab("Temperature (Celsius)")


visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  ylab("Number of visitor nights each quarter (millions)")

visitors %>%
  spread(State, Trips) %>%
  GGally::ggpairs(columns = 2:9)


#### Lag plots ---------------------------------------------------------------------------------------------------------

recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)

recent_production %>% 
  gg_lag(Beer, geom="point")


#### Autocorrelation ---------------------------------------------------------------------------------------------------

recent_production %>% 
  ACF(Beer, lag_max = 9)

recent_production %>%
  ACF(Beer) %>%
  autoplot()

a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot()


#### White noise -------------------------------------------------------------------------------------------------------

set.seed(30)

y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)

y %>%
  autoplot(wn) + 
  ggtitle("White noise")

y %>% 
  ACF(wn) %>%
  autoplot()


