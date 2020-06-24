
#### LIBRARIES #########################################################################################################

library(fpp3)


#### TRANSFORMATIONS AND ADJUSTMENTS ###################################################################################

#### Population adjustment ---------------------------------------------------------------------------------------------

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP / Population)

#### Inflation adjustment ----------------------------------------------------------------------------------------------

print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))

aus_economy <- global_economy %>%
  filter(Code == "AUS")

print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI) %>%
  gather("Type", "Turnover", Turnover, Adjusted_turnover, factor_key = TRUE) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  xlab("Years") + ylab(NULL) +
  ggtitle("Turnover for the Australian print media industry")


#### Mathematical transformations --------------------------------------------------------------------------------------

aus_production %>% autoplot(Gas)

lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%    # Guerrero function choose the best lambda value for Box-Cox transformation 
  pull(lambda_guerrero)

aus_production %>% autoplot(box_cox(Gas, lambda))


#### TIME SERIES COMPONENTS ############################################################################################

us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)

us_retail_employment %>%
  autoplot(Employed) +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

dcmp <- us_retail_employment %>%
  model(STL(Employed))

components(dcmp)

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")

components(dcmp) %>% autoplot() + xlab("Year")


#### Seasonally adjusted data ------------------------------------------------------------------------------------------

us_retail_employment %>%
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), season_adjust, color='blue') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")


#### MOVING AVERAGES ###################################################################################################

global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  xlab("Year") + ylab("% of GDP") +
  ggtitle("Total Australian exports")








