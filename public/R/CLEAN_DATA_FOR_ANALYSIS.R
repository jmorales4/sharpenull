library(ggplot2)

# Data URls
test_url = "http://54.148.91.193:8080/8214ba9ab15f49/test.dat"
survey_url = "http://54.148.91.193:8080/8214ba9ab15f49/survey.dat"
valid_user_url = "https://raw.githubusercontent.com/jmorales4/sharpenull/master/public/ID_MASTER.csv"
chart_series_url = "https://raw.githubusercontent.com/jmorales4/sharpenull/master/public/CHART_SERIES.csv"


test_drt = read.csv(test_url, stringsAsFactors = F)
survey_drt = read.csv(survey_url, stringsAsFactors = F, strip.white = T, sep = ",")
valid_user_drt = read.csv(valid_user_url, stringsAsFactors = F)
chart_series_drt = read.csv(chart_series_url, stringsAsFactors = F)

str(test_drt)
str(survey_drt)
str(valid_user_drt)
str(chart_series_drt)

##############################
### Clean valid user data
valid_user_df = valid_user_drt

# Change id to string
valid_user_df = data.frame(user = sprintf("%05d",valid_user_drt$id), name = valid_user_drt$names, stringsAsFactors = F)

##############################
### Clean chart data
chart_series_cln = chart_series_drt

# make date actual date
chart_series_cln$date = as.Date(chart_series_cln$date)

###############################
### Make chart_stats_df data frame of sharpe, return, and vol for each chart

# function to calculate stats from a series of prices
chart_stats_fn <- function(px){
  n = length(px)
  ret = (px[n]/px[1])^(252/(n-1)) - 1
  ret = (px[n]/px[1]) - 1
  chg = px[2:n]/px[1:(n-1)] - 1
  vol = sqrt(sum(chg ^ 2)/(n-2) * 252)
  nq = rev(seq(from = n, by=-1*round(n/4), length.out=4))
  qs = sort(rep_len(1:4,length.out=n))
  pxq = split(px,qs)
  retq = unname(sapply(pxq, function(x){x[length(x)]/x[1] - 1}))
  volq = unname(sapply(pxq, function(x){
    nq = length(x)
    chgq = x[2:nq]/x[1:(nq-1)] - 1
    return(sqrt(sum(chgq ^ 2)/(nq-2) * 252))
    }))
  sq = retq/volq
  
  return(c(
    sharpe = ret/vol,
    ret = ret,
    vol = vol,
    retq1 = retq[1],
    retq2 = retq[2],
    retq3 = retq[3],
    retq4 = retq[4],
    sq1 = sq[1],
    sq2 = sq[2],
    sq3 = sq[3],
    sq4 = sq[4]
    ))
  
}

# create stats for each chart
chart_stats_df_temp = aggregate(value~groupid + group, chart_series_cln, chart_stats_fn)

# create clean chart stats data
chart_stats_df = data.frame(chart = chart_stats_df_temp$groupid, group = chart_stats_df_temp$group, stringsAsFactors = F)
for(stati in colnames(chart_stats_df_temp$value)){
  chart_stats_df[stati] = chart_stats_df_temp$value[,stati]
}

# check out sharpe ratios by group to check blocking
ggplot(chart_stats_df, aes(x = sharpe, fill = group)) + geom_density(alpha = 0.25)

###############################
### Clean test data
str(test_drt)
test_df = test_drt

# Remove repeat flag rows and rows preceding repeat flag
rep_flags = which(grepl("repeat", test_drt$time))
rep_rows = rep_flags - 1
test_df = test_drt[-1*c(rep_flags,rep_rows),]

# transform user id to string
test_df$user = sprintf("%05d",test_df$user)

# Only keep valid user id's from the master list
test_df = subset(test_df, user %in% valid_user_df$user)

# Remove '11' position
test_df = test_df[test_df$position != 11,]

# Transform time date string to valid
test_df$time = unlist(
  lapply(
    strsplit(test_df$time,"T"),
    function(x){paste(gsub("Z","",x),collapse=" ")}
    )
  )
test_df$time = as.POSIXct(test_df$time)


# Trim spaces of group, direction conviction
test_df = transform(test_df,
                     group = gsub(" ","",group),
                     direction = gsub(" ","",direction),
                     conviction = gsub(" ","",conviction)
                     )

# Add x thousand to chart id depending on block if ID < 1000

test_df$chart[test_df$chart<1000] = with(
  subset(test_df, chart < 1000),chart + 1000*
    ifelse(
      group=='VNEG',1,ifelse(
        group=='NEG',2,ifelse(
          group=='FLAT',3,ifelse(
            group=='POS',4,5)
          )
        )
      )
  )

# User 30079 had their 6th chart entered twice 
# The entries were identical so am removing the second one
index_30079_6 = which(with(test_df, position == 6 & user == "30079"))
if(length(index_30079_6) > 1){
   test_df = test_df[-index_30079_6[2:length(index_30079_6)],]
}

# Check table of user_id's and positions
# There should only be one position per user
table(test_df$position, test_df$user)


############################
### Clean Survey Data
str(survey_drt)
survey_df = survey_drt
names(survey_df)[names(survey_df)=='id'] = 'user'

# transform user id to string
survey_df$user = sprintf("%05d",survey_df$user)

# Only keep valid user id's from the master list
survey_df = subset(survey_df, user %in% valid_user_df$user)


############################
### Merge Data in to One

str(test_df)
str(survey_df)
str(valid_user_df)
str(chart_stats_df)

all_data = merge(test_df, chart_stats_df, by=c("chart","group"))
all_data = merge(all_data, valid_user_df, by="user")
all_data = merge(all_data, survey_df, by="user")

all_data$up = (all_data$direction == "Up")
all_data$down = (all_data$direction == "Down")

#############################
### Save data frames as .Rda

save(test_df,file="test.Rda")
save(survey_df,file="survey.Rda")
save(valid_user_df,file="valid_user.Rda")
save(chart_stats_df,file="chart_stats.Rda")
save(all_data,file="all_data.Rda")

