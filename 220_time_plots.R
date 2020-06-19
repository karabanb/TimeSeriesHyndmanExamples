
#### LIBRARIES #########################################################################################################

library(fpp3)


#### PLOTS #############################################################################################################

melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class=="Economy")


#### Basic Time Series Plot  -------------------------------------------------------------------------------------------

melsyd_economy %>%
  autoplot(Passengers) +
  labs(title = "Ansett economy class passengers", subtitle = "Melbourne-Sydney") +
  xlab("Year")

