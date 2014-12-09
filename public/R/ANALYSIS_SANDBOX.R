# Libraries
library(ggplot2)

# Load all the data toghether
load("all_data.Rda")

# look at structure
str(all_data)

# look at distribution of sharpe ratios subjects saw
ggplot(all_data, aes(x = sharpe)) + geom_histogram()# + geom_density(aes(fill=group),alpha=0.25)

# simple regression suggested by Davids
summary(lm(up~sharpe, all_data))
summary(lm(down~sharpe, all_data))

# look at kernel regression of up / down given sharpe
kreg_up = ksmooth(all_data$sharpe, all_data$up, bandwidth = 1)
kreg_down = ksmooth(all_data$sharpe, all_data$down, bandwidth = 1)

kreg_df = data.frame(sharpe = kreg_up$x, prob = kreg_up$y, type = "Up")
kreg_df = rbind(kreg_df,
                data.frame(sharpe = kreg_down$x, prob = kreg_down$y, type = "Down")
                )

ggplot(kreg_df, aes(x = sharpe, y = prob, color = type)) + geom_line()
