
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

aus_production %>%  autoplot(Bricks)

pelt %>% autoplot(Lynx)

gafa_stock %>% 
  ggplot(aes(x = Date, y = Close, col = Symbol)) +
  geom_line() +
  facet_grid(Symbol~., scales = 'free_y') +
  theme_bw()

vic_elec %>% autoplot(Demand)


#### 5 #################################################################################################################

aus_arrivals

aus_arrivals %>% autoplot(Arrivals) + theme_bw()

aus_arrivals %>% gg_season(labels = 'both')

aus_arrivals %>% gg_subseries()


#### 6 #################################################################################################################

set.seed(42)

myseries <- aus_retail %>%
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))

myseries %>% autoplot(Turnover)

myseries %>% gg_season(Turnover,labels = 'both')

myseries %>% gg_subseries(Turnover)

myseries %>% gg_lag(Turnover, geom = 'point', lags = 1:12)

myseries %>% ACF(Turnover) %>% autoplot()


#### 7 #################################################################################################################

#### US Employment -----------------------------------------------------------------------------------------------------

us_employment

total_private_emp <- us_employment %>% 
  filter(Title == 'Total Private') 

total_private_emp %>% autoplot(Employed)

total_private_emp %>% gg_season(Employed, labels = 'both')

total_private_emp %>% gg_subseries(Employed)

total_private_emp %>% gg_lag(Employed, geom = 'point', lags = 1:12)

total_private_emp %>% ACF(Employed) %>% autoplot()


#### AUS Production ----------------------------------------------------------------------------------------------------

aus_production

aus_production %>% autoplot(Bricks)

aus_production %>% gg_season(Bricks)

aus_production %>% gg_subseries(Bricks)

aus_production %>% gg_lag(Bricks, geom = 'point')

aus_production %>% ACF(Bricks) %>% autoplot()


#### Pelt --------------------------------------------------------------------------------------------------------------

pelt

pelt %>% autoplot(Hare)

pelt %>% gg_season(Hare)

pelt %>% gg_subseries(Hare)

pelt %>% gg_lag(Hare, geom = 'point', lags = 1:16)

pelt %>% ACF(Hare) %>% autoplot()

####  PBS --------------------------------------------------------------------------------------------------------------

PBS

PBS_H02 <- PBS %>% 
  filter(ATC2 == 'H02') %>% 
  group_by(ATC2) %>% 
  summarise(Cost = sum(Cost))

PBS_H02 %>% autoplot(Cost)

PBS_H02 %>% gg_season(Cost, labels = 'right')

PBS_H02 %>% gg_subseries(Cost)

PBS_H02 %>% gg_lag(Cost, geom = 'point', lags = 1:12)

PBS_H02 %>% ACF(Cost) %>% autoplot()

#### US gasoline -------------------------------------------------------------------------------------------------------

us_gasoline

us_gasoline %>% autoplot()

us_gasoline %>% gg_season(Barrels)

us_gasoline %>% 
  gg_lag(Barrels, geom = 'point') + 
  theme(legend.position = "none")

us_gasoline %>% ACF(Barrels) %>% autoplot()


#### 9 #################################################################################################################

aus_livestock

aus_livestock_pigs <- aus_livestock %>% 
  filter(Animal == 'Pigs', State == 'Victoria', year(Month) >= 1990, year(Month) <= 1995) 

aus_livestock_pigs %>% autoplot(Count)

aus_livestock_pigs %>% ACF(Count) %>% autoplot()


#### 10 ################################################################################################################

dgoog <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2018) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE) %>%
  mutate(diff = difference(Close))

dgoog %>% autoplot(diff)

dgoog %>% ACF(diff) %>% autoplot()




