
#### LIBRARIES #########################################################################################################

library(fpp3)


#### 1 #################################################################################################################

#### a) ----------------------------------------------------------------------------------------------------------------

gafa_stock %>% 
  filter(Symbol == 'AAPL') %>% 
  autoplot(Close)

PBS %>% 
  filter(ATC2 == 'A01',
         Concession == 'Concessional',
         Type == 'Co-payments') %>% 
  autoplot(Cost)

vic_elec %>% 
  autoplot(Temperature)

pelt %>% 
  autoplot(Hare)

#### b) ----------------------------------------------------------------------------------------------------------------

interval(gafa_stock)
interval(PBS)
interval(vic_elec)
interval(pelt)

#### c) ----------------------------------------------------------------------------------------------------------------

gafa_stock %>% 
  filter(Symbol == 'AAPL') %>% 
  filter(Close == max(Close))

gafa_stock %>% 
  filter(Symbol == 'AMZN') %>% 
  filter(Close == max(Close))

gafa_stock %>% 
  filter(Symbol == 'FB') %>% 
  filter(Close == max(Close))

gafa_stock %>% 
  filter(Symbol == 'GOOG') %>% 
  filter(Close == max(Close))


#### 2 #################################################################################################################

tute1 <- readr::read_csv('data/tute1.csv')
View(tute1)

mytimeseries <- tute1 %>%
  mutate(Quarter = yearmonth(Quarter)) %>%
  as_tsibble(index = Quarter)

mytimeseries %>%
  pivot_longer(-Quarter, names_to="Key", values_to="Value") %>% 
  ggplot(aes(x = Quarter, y = Value, colour = Key)) +
  geom_line() +
  facet_grid(vars(Key), scales = "free_y")


#### 3 #################################################################################################################

#### a) ----------------------------------------------------------------------------------------------------------------

tourism_raw <- readxl::read_excel('data/tourism.xlsx')

#### b) ----------------------------------------------------------------------------------------------------------------

tourism <- tourism_raw %>% 
  mutate(Quarter = yearquarter(Quarter)) %>% 
  as_tsibble(index = Quarter, key = c('Region', 'State', 'Purpose'))

#### c) ----------------------------------------------------------------------------------------------------------------

tourism %>% 
  select(Region, Purpose, Trips) %>% 
  as.data.frame() %>% 
  group_by(Region, Purpose) %>% 
  summarise(avg_trips = mean(Trips, na.rm = TRUE)) %>%
  arrange(-avg_trips)

#### d) ----------------------------------------------------------------------------------------------------------------

tourism_state <- tourism %>% 
  group_by(State) %>% 
  summarise(SumOfTrips = sum(Trips))


#### 4 #################################################################################################################


