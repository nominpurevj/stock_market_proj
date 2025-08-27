library(GGally)

# Create a scatterplot matrix
ggscatmat(spy.us, color = "direction")

# Observations:
# Strong correlations are evident among trading volumes on consecutive days. 
# For example, volume.lag2 and volume.lag3 exhibit a correlation of 0.85.
# In contrast, volume.lag1 and volume.lag2 show only a weak correlation. 
# While weak, these correlations are noteworthy, as most other variables are largely uncorrelated.
# Similarly, volume.lag2 with lag3 and volume.lag3 with lag4 indicate that smaller returns on a given day 
# may be associated with higher trading volumes on subsequent days.

# Next, examine the relationship between volume and returns for further insights
ggplot(spy.us, aes(lag2, volume.lag1)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# Interpretation:
# The plot illustrates a negative association between trading volume and the previous day's returns.
# However, it does not consistently support the notion that lower returns on a given day 
# directly result in higher trading activity the following day. 
# Instead, both very low and very high returns appear to be associated with increased volumes 
# on subsequent days, with the effect slightly more pronounced for lower returns.
