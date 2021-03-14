library(GGally)

# create a scatterplot matrix
ggscatmat(spy.us, color = "direction")

# understandably, strong correlations are observed among the volumes of a given day 
# and the days immediately preceding it. For instance, volume.lag2 and volume.lag3 have a correlation
# of 0.85

# moreover, volume.lag1 and lag2 have weak correlations. The same goes for volume.lag2 and lag3.
# And the same goes for volume.lag3 and lag4. They hint that smaller returns in the market on a
# given day are associated with higher volume of trading on the next day. 

# lets take a closer look at the relationship between volume and returns.

ggplot(spy.us, aes(lag2, volume.lag1)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# this graph makes it clear why there is a negative associated between volume and the previous day's returns.
# However, it does not necessarily seem to be the case that lower return on a given day is associated with more activity 
# the next day. What more seems to be the case is that both high very low and very high returns on a given day
# are associated with high volumes of exchange the next day. However, the skew is more towards the low returns.