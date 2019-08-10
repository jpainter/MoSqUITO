# nasapower  

# document not very detailed, but suggests that data is at 1/2 degree (~35sqmi)
# *Maximum area processed is 4.5 x 4.5 degrees (100 points).

# see also nasadata and nasaweather, and rnoaa,  and rnaturalearth, rnaturalearthdata

library(nasapower)
daily_ag <- get_power(community = "AG",
                      lonlat = c(151.81, -27.48),
                      pars = c("RH2M", "T2M", "PRECTOT"),
                      dates = "1985-01-01",
                      temporal_average = "DAILY"
)

daily_ag

get_power
