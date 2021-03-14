library(GGally)

# create a scatterplot matrix
ggscatmat(spy.us, color = "direction")

# no correlations have been observed except between volume and the percentage
# return today, which shows a moderate positive correlation

ggplot(spy.us, aes(lag1, volume.today)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# whats interesting from here is that returns that are either very high
# or very low are associated with high volumes of exchange 