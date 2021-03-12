# obtain the dataset
spy.us <- read.csv("~/Projects/stock market project/spy.us.txt")

# Use open and close to create percentage returns for the present day.
spy.us['today'] <- round(spy.us$Close / spy.us$Open * 100 - 100, 3)

# Create lags that indicate the previous days' percentage return.
# When this is done, the first 5 days of the dataset will not be used as
# response variables.

spy.us['lag1'] <- c(NA, spy.us$today[-3201]) 
spy.us['lag2'] <- c(NA, spy.us$lag1[-3201]) 
spy.us['lag3'] <- c(NA, spy.us$lag2[-3201]) 
spy.us['lag4'] <- c(NA, spy.us$lag3[-3201]) 
spy.us['lag5'] <- c(NA, spy.us$lag4[-3201]) 

spy.us['volume.lag1'] <- c(NA, spy.us$Volume[-3201]) 
spy.us['volume.lag2'] <- c(NA, spy.us$volume.lag1[-3201]) 
spy.us['volume.lag3'] <- c(NA, spy.us$volume.lag2[-3201]) 
spy.us['volume.lag4'] <- c(NA, spy.us$volume.lag3[-3201]) 
spy.us['volume.lag5'] <- c(NA, spy.us$volume.lag4[-3201]) 

# add year as a variable rather than specific
spy.us['year'] <- as.numeric(substring(spy.us$Date, 1, 4))

# reorder the columns
col_order <- c("year", "lag1", "lag2", "lag3", "lag4", "lag5", 
               "volume.lag1", "volume.lag2", "volume.lag3", "volume.lag4", "volume.lag5", 
               "Volume", "today", "Date", "Open", "High", "Low", "Close", "OpenInt")
spy.us <- spy.us[, col_order]
names(spy.us)[names(spy.us) == "Volume"] <- "volume.today"

# get rid of irrelevant variables
spy.us <- spy.us[6:3201, 1:13]
rownames(spy.us) <- NULL 

# add outcome variable
spy.us['direction'] <- rep("down", nrow(spy.us))
spy.us$direction[spy.us$today >= 0] <- "up"

# create outcome variable
spy.us$up <- 0
spy.us$up[spy.us$direction == 'up'] <- 1

View(spy.us)

