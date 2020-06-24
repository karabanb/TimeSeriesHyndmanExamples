
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

aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slide_dbl(Exports, mean, .size = 5, .align = "center")
  )

aus_exports %>%                                   #
  autoplot(Exports) +
  autolayer(aus_exports, `5-MA`, color='red') +
  xlab("Year") + ylab("Exports (% of GDP)") +
  ggtitle("Total Australian exports") +
  guides(colour=guide_legend(title="series"))

beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)

beer_ma <- beer %>%
  mutate(
    `4-MA` = slide_dbl(Beer, mean, .size = 4, .align = "center-left"),
    `2x4-MA` = slide_dbl(`4-MA`, mean, .size = 2, .align = "center-right")
  )

## TODO COMMIT ISSUE - need to filter data to reproduce results

us_retail_employment <- us_employment %>%
  filter(Title == 'Retail Trade') %>%
  filter_index('1990'~.)

us_retail_employment_ma <- us_retail_employment %>%
  filter(Title == 'Retail Trade') %>%
  filter_index('1990'~.) %>% 
  mutate(
    `12-MA` = slide_dbl(Employed, mean, .size = 12, .align = "cr"),
    `2x12-MA` = slide_dbl(`12-MA`, mean, .size = 2, .align = "cl"),
  )

us_retail_employment_ma %>%
  autoplot(Employed, color='gray') +
  autolayer(us_retail_employment_ma, vars(`2x12-MA`), color='red') +
  xlab("Year") + ylab("Persons (thousands)") +
  ggtitle("Total employment in US retail")


#### CLASSICAL DECOMPOSITION ###########################################################################################

#### Additive ----------------------------------------------------------------------------------------------------------

us_employment %>%
  filter(Title == 'Retail Trade') %>%
  filter_index('1990'~.) %>% 
  model(classical_decomposition(Employed, type = "additive")) -> m
  components() %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical additive decomposition of total US retail employment")

#### Multiplicative ----------------------------------------------------------------------------------------------------  
  
us_employment %>%
  filter(Title == 'Retail Trade') %>%
  filter_index('1990'~.) %>% 
  model(classical_decomposition(Employed, type = "mult")) %>%
  components() %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition of total US retail employment")


#### X11 Decomposition #################################################################################################

  
# TODO X11 function isn't implemented in feasts 0.1.4
  
# x11_dcmp <- us_retail_employment %>%
#   model(x11 = feasts::X11(Employed, type = "additive")) %>%
#   components()
#   
# autoplot(x11_dcmp) + xlab("Year") +
#   ggtitle("Additive X11 decomposition of US retail employment in the US")
  
  
#### SEATS Decomposition ###############################################################################################

# TODO SEATS function isn't implemented in feasts 0.1.4

  
#### STL Decomposition #################################################################################################

us_retail_employment %>%
  model(STL(Employed ~ trend(window=7) + season(window='periodic'),
            robust = TRUE)) %>%
  components() %>%
  autoplot()




