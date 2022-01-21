library("fpp2")
library("fpp3")

# US Consumption Expenditure does not appear to be seasonal, but does not look likle pure white noise either
us_change %>% autoplot(Consumption) +
  xlab("Year") + ylab("Quarterly percentage change")

# We can do a stepwise search for the best fitting ARIMA model
fit <- us_change %>%
  model(ARIMA(Consumption ~ PDQ(0,0,0)))
report(fit)

# and use this ARIMA(1,0,3) model to generate a forecast up to 10 steps ahead
fit %>% forecast(h=10) %>% autoplot(slice(us_change, (n()-80):n()))

# Is this the model we would choose from the ACF and PACF?
us_change %>%
  features(Consumption, unitroot_nsdiffs)

us_change %>%
  features(Consumption, unitroot_ndiffs)

us_change %>% ACF(Consumption) %>% autoplot()

us_change %>% PACF(Consumption) %>% autoplot()

us_change %>% gg_tsdisplay(Consumption, plot_type='partial')

# These patterns suggest an ARIMA(3,0,0) or an ARIMA(0,0,3)
fit2 <- us_change %>%
  model(
    arima = ARIMA(Consumption ~ pdq(3,0,0) + PDQ(0,0,0))
  )
report(fit2)

fit2 %>% gg_tsresiduals()

fit3 <- us_change %>%
  model(
    arima = ARIMA(Consumption ~ pdq(0,0,3) + PDQ(0,0,0))
  )
report(fit3)

fit3 %>% gg_tsresiduals()

# The AIC is lower for the ARIMA(3,0,0) than the ARIMA(0,0,3) and the ARIMA(1,0,3) by automated search
fit2 %>% forecast(h=10) %>% autoplot(slice(us_change, (n()-80):n()))

# Let's look at the workflow for a new time series
elec_equip <- as_tsibble(fpp2::elecequip)

elec_dcmp <- elec_equip %>%
  model(STL(value ~ season(window="periodic"))) %>%
  components() %>%
  select(-.model) %>%
  as_tsibble()

elec_dcmp %>%
  autoplot(season_adjust)

# First check the order of differencing required
elec_dcmp %>%
  features(season_adjust, unitroot_ndiffs)

# Now check the ACF and PACF of the differenced series
elec_dcmp %>%
  gg_tsdisplay(difference(season_adjust), plot_type='partial')

# This suggests one of three models
fit1 <- elec_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(3,1,0) + PDQ(0,0,0))
  )
report(fit1)

fit2 <- elec_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(0,1,3) + PDQ(0,0,0))
  )
report(fit2)

fit3 <- elec_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(3,1,3) + PDQ(0,0,0))
  )
report(fit3)

# ARIMA(3,1,0) model achieves the lowest AIC
# But it is worth checking models with one more or less order of AR and MA terms
fit4 <- elec_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(2,1,0) + PDQ(0,0,0))
  )
report(fit4)

fit5 <- elec_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(4,1,0) + PDQ(0,0,0))
  )
report(fit5)

fit6 <- elec_dcmp %>%
  model(
    arima = ARIMA(season_adjust ~ pdq(3,1,1) + PDQ(0,0,0))
  )
report(fit6)

# The ARIMA(3,1,1) has the lowest AIC, so let's check its residuals
fit6 %>% gg_tsresiduals()

augment(fit6) %>%
  features(.resid, ljung_box, lag = 24, dof = 4)

# and generate its forecast
fit6 %>% forecast() %>% autoplot(elec_dcmp)

# What about series with some seasonality
eu_retail <- as_tsibble(fpp2::euretail)
eu_retail %>% autoplot(value) + ylab("Retail index") + xlab("Year")

# The data is quarterly and needs seasonal differencing and first order differencing
eu_retail %>%
  features(value, unitroot_nsdiffs)

eu_retail %>%
  features(difference(value, 4), unitroot_ndiffs)

# The ACF and PACF show the effect of the differencing
eu_retail %>% gg_tsdisplay(value,
                           plot_type='partial')

eu_retail %>% gg_tsdisplay(value %>% difference(4),
                           plot_type='partial')

eu_retail %>% gg_tsdisplay(value %>% difference(4) %>% difference(),
                           plot_type='partial')

# The spikes at lag 1 on the ACF and PACF suggest an AR(1) or MA(1), while the spikes at lag 4 on each plot suggest a seasonal AR or MA model
fit1 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(1,1,0) + PDQ(1,1,0)))
report(fit1)

fit1 %>% gg_tsresiduals()

fit2 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(1,1,0) + PDQ(0,1,1)))
report(fit2)

fit2 %>% gg_tsresiduals()

fit3 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(0,1,1) + PDQ(1,1,0)))
report(fit3)

fit3 %>% gg_tsresiduals()

fit4 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(0,1,2) + PDQ(1,1,0)))
report(fit4)

fit4 %>% gg_tsresiduals()

fit5 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(0,1,3) + PDQ(1,1,0)))
report(fit5)

fit5 %>% gg_tsresiduals()

fit6 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(0,1,1) + PDQ(0,1,1)))
report(fit6)

fit6 %>% gg_tsresiduals()

fit7 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(0,1,2) + PDQ(0,1,1)))
report(fit7)

fit7 %>% gg_tsresiduals()

fit8 <- eu_retail %>%
  model(arima = ARIMA(value ~ pdq(0,1,3) + PDQ(0,1,1)))
report(fit8)

fit8 %>% gg_tsresiduals()

# The ARIMA(0,1,3)(0,1,1)4 model has the lowest AIC
fit8 %>% forecast(h=12) %>% autoplot(eu_retail)

# Automated search will look for the best seasonal model, but again it is best to turn stepwise off
fit9 <- eu_retail %>%
  model(ARIMA(value))
report(fit9)

# Let's compare the ARIMA and ETS models
aus_economy <- global_economy %>% filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy)

aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy)

# Consider the cement data beginning in 1988
cement <- aus_production %>%
  filter(year(Quarter) >= 1988)

cement %>% autoplot(Cement)

# Use 20 years of the data as the training set
train <- cement %>%
  filter(year(Quarter) <= 2007)

# Fit and test an ARIMA model
fit_arima <- train %>% model(ARIMA(Cement))
report(fit_arima)
gg_tsresiduals(fit_arima, lag_max = 16)
augment(fit_arima) %>%
  features(.resid, ljung_box, lag = 16, dof = 6)

# Fit and test an ETS model
fit_ets <- train %>% model(ETS(Cement))
report(fit_ets)
fit_ets %>% gg_tsresiduals(lag_max = 16)
augment(fit_ets) %>%
  features(.resid, ljung_box, lag = 16, dof = 6)

# Generate forecasts and compare accuracy over the test set
bind_rows(
  fit_arima %>% accuracy(),
  fit_ets %>% accuracy(),
  fit_arima %>% forecast(h = "2 years 6 months") %>%
    accuracy(cement),
  fit_ets %>% forecast(h = "2 years 6 months") %>%
    accuracy(cement)
)

# Generate forecasts from an ARIMA model
cement %>% model(ARIMA(Cement)) %>% forecast(h="3 years") %>% autoplot(cement)

# We can also build ARIMA models that include cross-correlations with other time series
us_change %>%
  gather("var", "value", Consumption, Income) %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  xlab("Year") + ylab(NULL) +
  ggtitle("Quarterly changes in US consumption and personal income")

# We do not specify pdq or PDQ to allow an automatic search for the best combination ARIMA model and regression model
fit1 <- us_change %>%
  model(ARIMA(Consumption ~ lag(Income)))
        
report(fit1)

fit1 %>% gg_tsresiduals()

augment(fit1) %>%
  features(.resid, ljung_box, dof = 5, lag = 8)