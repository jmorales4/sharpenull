require(reshape2)
require(ggplot2)
require(RJSONIO)

###########################################################
###########################################################
path <- ""#"F:/Docs/Personal/MIDS/2014Fall/SHARPE_RATIOS/"
chart_master <- read.csv(paste0(path, 'CHART_MASTER.csv'), stringsAsFactors = F)
chart_master$start_date <- as.Date(chart_master$start_date)
chart_master$end_date <- as.Date(chart_master$end_date)
chart_master$future_date <- as.Date(chart_master$end_date) + 356

pxDF <- read.csv(paste0(path, 'SERIES_PRICES.csv'), stringsAsFactors = F)
pxDF$date <- as.Date(pxDF$date)
pxDF <- pxDF[complete.cases(pxDF),]

#############

match_charts <- function(df, chartlist){
  index <- c()
  group <- c()
  groupid <- c()
  for(i in 1:nrow(chartlist)){
    charti <- chartlist[i,]
    idxi <- which(
              df$underlyer == charti$underlyer &
                df$date < charti$end_date &
                df$date > charti$start_date,
              )
    index <- c(index, idxi)
    group <- c(group, rep(charti$group, length(idxi)))
    groupid <- c(groupid, rep(charti$groupid, length(idxi)))
  }
  
  return(list(index=index, group=group, groupid=groupid))
}


match_charts <- function(df, chartlist){
  index <- c()
  type <- c()
  group <- c()
  groupid <- c()
  underlyer <- c()
  for(i in 1:nrow(chartlist)){
    charti <- chartlist[i,]
    idxpast <- which(
              df$underlyer == charti$underlyer &
                df$date <= charti$end_date &
                df$date > charti$start_date,
              )
    idxfuture <- which(
              df$underlyer == charti$underlyer &
                df$date > charti$end_date &
                df$date <= charti$future_date,
              )
    index <- c(index, c(idxpast, idxfuture))
    type <- c(type, c(rep('past',length(idxpast)), rep('future',length(idxfuture))))
    group <- c(group, rep(charti$group, length(idxpast) + length(idxfuture)))
    groupid <- c(groupid, rep(charti$groupid, length(idxpast) + length(idxfuture)))
    underlyer <- c(underlyer, rep(charti$underlyer, length(idxpast) + length(idxfuture)))
  }
  
  return(list(index=index, group=group, groupid=groupid, type=type, underlyer=underlyer))
}



rowsample <- function(df, n, each = NULL){
  if(! is.null(each)){
    if(each %in% colnames(df)){
      #each = 'group'
      rows = c()
      for(i in unique(df[,each])){
        rows = c(rows, sample(which(df[,each] == i), n))
      }
    } else{rows = sample(1:nrow(df),min(n,nrow(df)))}
  } else{
    rows = sample(1:nrow(df),min(n,nrow(df)))
  }
  return(chart_master[rows,])
  }
nsample <- 20
chartindex <- match_charts(pxDF, rowsample(chart_master, nsample, each = 'group'))
#chartindex <- match_charts(pxDF, chart_master)

chartsDF <- pxDF[,c('date','value')][chartindex$index,]
chartsDF$type <- chartindex$type
chartsDF$groupid <- chartindex$groupid
chartsDF$group <- chartindex$group
chartsDF$underlyer <- chartindex$underlyer

#ggplot(subset(chartsDF, group == 'VNEG'), aes(x = date, y = value)) +
#  geom_line() + facet_wrap(~groupid, scale = "free")

multby <- 1.24
shiftby <- -500

chart_series_df <- transform(subset(chartsDF, type == 'past'), value = value*multby, date = as.character(date + shiftby))[,c("date","value","groupid","group")]

write.csv(
  chart_series_df,
  paste0(path,'CHART_SERIES.csv'),
  row.names=F)

chart_result_series_df <- transform(chartsDF, date = as.character(date))

write.csv(
  chart_result_series_df,
  paste0(path,'CHART_RESULT_SERIES.csv'),
  row.names=F)


