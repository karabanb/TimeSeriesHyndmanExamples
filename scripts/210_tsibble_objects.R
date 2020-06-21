
library(fpp3)

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
