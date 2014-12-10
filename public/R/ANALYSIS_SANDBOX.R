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

#covariate balance check

all_data_cov <- all_data[, !names(all_data) %in% c("up", "down", "time", 
                                                   "group", "chart", "ret", "vol"
                                                   ,"direction", "conviction")]
all_data_cov_complete <- na.omit(all_data_cov)
# find and remove categorical variables with only one value
lapply(na.omit(all_data_cov_complete), unique)
all_data_cov_complete <- all_data_cov_complete[, !names(all_data_cov_complete) %in% c("personal",
                                                                                      "professional"
                                                                                      )] 
#omit variables perfectly predicted by otherse
all_data_cov_complete <- all_data_cov_complete[, !names(all_data_cov_complete) %in% c("other")] 

#regress on all and subsets of covariates
summary(lm(sharpe~.,data=all_data_cov_complete))
summary(lm(sharpe~user,data=all_data_cov_complete))
summary(lm(sharpe~name,data=all_data_cov_complete))
summary(lm(sharpe~year,data=all_data_cov_complete))
summary(lm(sharpe~stocks + bonds + cash,data=all_data_cov_complete))
summary(lm(sharpe~user,data=all_data))
summary(lm(sharpe~name,data=all_data))
summary(lm(sharpe~time,data=all_data))
summary(lm(sharpe~position * user,data=all_data))
#no significant p-values from any of these tests except for the last,
#which found that several users have statistically significant negative
#effects of order they saw the chart on Sharpe ratio. However, the large 
#number of coefficients in this regression suggests that these p-values
#merely arose by chance, as confirmed by the F-statistic of the regression

#tests of within subjects assumptions
install.packages("DataCombine")
install.packages("reshape")
library(reshape)
library(DataCombine)
lag_tests <- all_data[,c("sharpe", "up", "position", "user")]
lag_tests <- lag_tests[with(lag_tests, order(user, position)),]

# no persistence
lag_tests <- slide(lag_tests, Var = "sharpe", slideBy = -1)
lag_tests <- rename(lag_tests, c("sharpe-1"="sharpe_last"))
lag_tests$sharpe_last <- ifelse(lag_tests$position==1, NA, lag_tests$sharpe_last)
lag_tests_per <- lag_tests[complete.cases(lag_tests[,c("up", "sharpe_last")]),]
summary(lm(up~sharpe_last, lag_tests_per))
#p-value of .30, persistence unlikely

#no anticipation
lag_tests <- slide(lag_tests, Var = "sharpe", slideBy = 1)
lag_tests <- rename(lag_tests, c("sharpe1"="sharpe_next"))
lag_tests$sharpe_next <- ifelse(lag_tests$position==10, NA, lag_tests$sharpe_next)
lag_tests_ant <- lag_tests[complete.cases(lag_tests[,c("up", "sharpe_next")]),]
summary(lm(up~sharpe_last, lag_tests_ant))
#p-value of .37, persistence unlikely




