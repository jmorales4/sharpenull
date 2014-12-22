### Libraries
library(ggplot2)

# Load all the data together
load("all_data.Rda")

# look at structure
str(all_data)

# rename return field b/c it's an R reserved word
all_data$return_performance = all_data[,"return"]

# look at distribution of sharpe ratios subjects saw
ggplot(all_data, aes(x = sharpe)) + geom_histogram()# + geom_density(aes(fill=group),alpha=0.25)
ggplot(all_data, aes(x = sharpe)) + geom_density(fill='black', alpha=0.25) + geom_density(aes(fill=group),alpha=0.25)

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

# look at kernel regression of strong / weak given sharpe
kreg_strong = ksmooth(all_data$sharpe, all_data$confident_strong, bandwidth = 1)
kreg_weak = ksmooth(all_data$sharpe, all_data$confident_weak, bandwidth = 1)

kreg_df_conf = data.frame(sharpe = kreg_strong$x, prob = kreg_strong$y, type = "Strong")
kreg_df_conf = rbind(kreg_df_conf,
                data.frame(sharpe = kreg_weak$x, prob = kreg_weak$y, type = "Down")
                )

ggplot(kreg_df_conf, aes(x = sharpe, y = prob, color = type)) + geom_line()

#covariate balance check

all_data_cov <- all_data[, !names(all_data) %in% c("up", "down", "time", 
                                                   "group", "chart", "ret", "vol"
                                                   ,"direction", "conviction", "retq1",
                                                   "retq2","retq3","retq4")]
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
summary(lm(sharpe~professional,data=all_data))
summary(lm(sharpe~knowledge,data=all_data))
summary(lm(sharpe~experience,data=all_data))
summary(lm(sharpe~literacy,data=all_data))
summary(lm(sharpe~personal,data=all_data))
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
if(!require(reshape)){
  install.packages("reshape")
  library(reshape)
}
if(!require(reshape)){
  install.packages("DataCombine")
  library(reshape)
}
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
#p-value of .37, anticipation unlikely

#Hypothesis tests
#look at distribution of return and risk (the treatment variables)
ggplot(all_data, aes(x = ret)) + geom_histogram()
ggplot(all_data, aes(x = vol)) + geom_histogram()

#Set 1: Does Sharpe affect prediction?
reduced <- lm(up ~ name + literacy + knowledge + experience + professional + personal + user, all_data)

# Sharpe Only
sharpe_up =  lm(up ~ sharpe + name + literacy + knowledge + experience + professional + personal + user, all_data)
sharpe_down =  lm(down ~ sharpe + name + literacy + knowledge + experience + professional + personal + user, all_data)
summary(sharpe_up)
summary(sharpe_down)

# Fully Saturated Sharpe (ret + vol + ret*vol)
summary(lm(up ~ ret * vol + name + literacy + knowledge + experience + professional + personal + user, all_data))
summary(lm(down ~ ret * vol + name + literacy + knowledge + experience + professional + personal + user, all_data))

# Joint significance test of Sharpe ratio
reduced <- lm(up ~ name + literacy + knowledge + experience + professional + personal + user, all_data)
full <- lm(up ~ ret * vol + name + literacy + knowledge + experience + professional + personal + user, all_data)
anova(reduced, full)

#Set 2: Does Sharpe affect confidence?

#create binary conviction variables and perform regression
all_data$confident_strong <- ifelse(all_data$conviction!="Strong", 0, 1)
all_data$confident_weak <- ifelse(all_data$conviction!="Weak", 0, 1)
summary(lm(confident_strong ~ ret * vol + name + literacy + knowledge + experience + professional + personal + user, all_data))
summary(lm(confident_weak ~ ret * vol + name + literacy + knowledge + experience + professional + personal + user, all_data))

# Joint significance test of Sharpe ratio
reduced <- lm(confident_strong ~ name + literacy + knowledge + experience + professional + personal + user, all_data)
full <- lm(confident_strong ~ ret * vol + name + literacy + knowledge + experience + professional + personal + user, all_data)
anova(reduced, full)

#regression with high volatility as binary variable (just for kicks)
nrow(subset(all_data, vol > .3))
all_data$high_vol <- ifelse(all_data$vol < .3, 0, 1)
summary(lm(confident_strong ~ ret + high_vol + ret * high_vol + name + literacy + knowledge + experience + professional + personal + user, all_data))

#Set 3: Does perceived asset performance affect confidence

# check out how many of each category people said
table(subset(all_data, position == 1)$return_performance)

return_subset = complete.cases(all_data$return_performance)

strong3 = lm(confident_strong ~ sharpe * return_performance + name + literacy + knowledge + experience + professional + user, all_data[return_subset,])
weak3 = lm(confident_weak ~ sharpe * return_performance + name + literacy + knowledge + experience + professional + user, all_data[return_subset,])
summary(strong3)
summary(weak3)

# Joint significance test
reduced_strong3 = lm(confident_strong ~ name + literacy + knowledge + experience + professional + user, all_data[return_subset,])
anova(reduced_strong3, strong3)

reduced_weak3 = lm(confident_weak ~ name + literacy + knowledge + experience + professional + user, all_data[return_subset,])
anova(reduced_weak3, weak3)

#Set 4: Does Sharpe affect prediction differently for finance professionals?
up4 = lm(up ~ sharpe * professional + name + literacy + knowledge + experience + personal + user, all_data)
down4 = lm(down ~ sharpe * professional + name + literacy + knowledge + experience + personal + user, all_data)
summary(up4)
summary(down4)

# Joint significance test
reduced_up4 = lm(up ~ name + literacy + knowledge + experience + personal + user, all_data)
anova(reduced_up4, up4)

reduced_down4 = lm(down ~ name + literacy + knowledge + experience + personal + user, all_data)
anova(reduced_down4, down4)


#Set 5: Does it matter where the return happens?

# check out return by quarter
lm5up = lm(up ~ sq1 + sq2 + sq3 + sq4 + name + literacy + knowledge + experience + professional + personal + user, all_data)
lm5down = lm(down ~ sq1 + sq2 + sq3 + sq4 + name + literacy + knowledge + experience + professional + personal + user, all_data)

summary(lm5up)
summary(lm5down)

up_coef = data.frame(summary(lm5up)$coef[c("sq1","sq2","sq3","sq4"),])
colnames(up_coef) = c("est","se","t","p")
up_coef$q = 1:4

down_coef = data.frame(summary(lm5down)$coef[c("sq1","sq2","sq3","sq4"),])
colnames(down_coef) = c("est","se","t","p")
down_coef$q = 1:4

ggplot(up_coef, aes(x=q, y=est, ymin=est-se*1.96, ymax=est+se*1.96)) + geom_point() + geom_errorbar()
ggplot(down_coef, aes(x=q, y=est, ymin=est-se*1.96, ymax=est+se*1.96)) + geom_point() + geom_errorbar()






