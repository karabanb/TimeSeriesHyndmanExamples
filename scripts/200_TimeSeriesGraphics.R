
#### 2 TIME SERIES GRAPHICS ############################################################################################

library(fpp3)


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


### Read a csv file and convert to a tsibble ### -----------------------------------------------------------------------

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
