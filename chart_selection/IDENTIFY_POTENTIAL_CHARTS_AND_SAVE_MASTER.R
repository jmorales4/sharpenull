require(reshape2)
require(ggplot2)

zscore.from.price <- function(x, vol.n, pct = TRUE, return.n = 1, mean.sub = FALSE) {
  movsd <- function(series,lag, mean.sub = FALSE, return.n=1){
    n.row <- length(series)
    movingsd <- rep(NA, n.row)    
    for( i in lag:n.row){
      obs <- rev(seq(from=i, to=(i-lag+1), by=-1*return.n))
      movingsd[i] <- sqrt(sum((series[obs] - ifelse(mean.sub,mean(series[obs]),0))^2)/(length(obs)-1))

    }
    movingsd
  }

  nr <- length(x)
  
  returns <- rep(NA,nr)            
  period_returns <- returns
    
  vol.n <- min(c(vol.n,nr-return.n))
  
  if (pct){
    returns[(1+return.n):nr] <- x[(1+return.n):nr] / x[1:(nr-return.n)] - 1
    period_returns[(1+vol.n):nr] <- x[(1+vol.n):nr] / x[1:(nr-vol.n)] - 1
  } else {
    returns[(1+return.n):nr] <- x[(1+return.n):nr] - x[1:(nr-return.n)]
    period_returns[(1+vol.n):nr] <- x[(1+vol.n):nr] - x[1:(nr-vol.n)]
  }

  vols <- movsd(returns,vol.n, mean.sub = mean.sub, return.n=return.n) * sqrt(vol.n)
  
  zscore <- period_returns / vols
  
  return(list(zscore=zscore, vol=vols, ret=period_returns))
}

#zscore.from.price(c(1,2,3,4,5,6,7,8,9,10) + 100 + rnorm(10)*2, 5)
#zscore.from.price(temp, 30)

#zscore.from.price(temp, 30)$zscore
#zscore.from.price(temp, 252) *100

############################################
############################################

path <- "" #"F:/Docs/Personal/MIDS/2014Fall/SHARPE_RATIOS/"


allDF <- read.csv(paste0(path, "SERIES_PRICES.csv"), stringsAsFactors=F)
allDF$date <- as.Date(allDF$date)
allDF$tenor <- "0"
allDF$type <- 'LEVEL'
allArray <- acast(data=allDF, underlyer ~ type ~ tenor ~ date)

seriesData <- read.csv(paste0(path, 'SERIES_DATA.csv'), stringsAsFactors=F)

ndate <- dim(allArray)[4]
dateNames <- dimnames(allArray)[[4]]

nund <- dim(allArray)[1]
undNames <- dimnames(allArray)[[1]]

## Calc Sharpe Ratios ##
months <- c(3, 6, 12) #c(1,2,3) #c(6,12,24)
asset.return.type <- TRUE #use the same return type for all assets, define by "pct"
pct <- TRUE #use percent return in vol if TRUE, otherwise uses diff return

#For calculating realized on different tenors, especially for rates
rlzd_calc_tenors <- c("0") #c("0","6","12","24") 

date_vec <- c()
value_vec <- c()
und_vec <- c()
type_vec <- c()
tenor_vec <- c()

for(i in 1:nund){
  
  if(asset.return.type){
    pct <- !(subset(seriesData,
                    underlyer == undNames[i]
                  )$asset_class %in% c("RATES"))
  }
  
  
  
  for(j in 1:length(months)){
    for(k in 1:length(rlzd_calc_tenors)){
      date_vec <- c(date_vec, data.frame(date = dateNames))
      value_vec <- c(value_vec,
                     zscore.from.price(
                         allArray[i,"LEVEL",rlzd_calc_tenors[k],],
                         ifelse(months[j] == 12, 261, months[j]*21),
                         pct = pct
                       )$zscore
                     )
      und_vec <- c(und_vec,rep(undNames[i], ndate))
      if(length(rlzd_calc_tenors)==1){
        type_vec <- c(type_vec, rep("ZSCORE", ndate))
      }else{
        type_vec <- c(type_vec, rep(paste("ZSCORE",rlzd_calc_tenors[k],sep=""), ndate))
      }
      tenor_vec <- c(tenor_vec, as.numeric(rep(months[j], ndate)))
    }
  }
}

addDF <- data.frame(
  date = as.Date(unlist(date_vec)),
  value = value_vec,
  underlyer = und_vec,
  type = type_vec,
  tenor = tenor_vec,
  stringsAsFactors = F
  )

allDF <- rbind(allDF, addDF)

allArray <- acast(data=allDF, underlyer ~ type ~ tenor ~ date)

allDF$asset_class <- seriesData$asset_class[match(allDF$underlyer,  seriesData$underlyer)]
addDF$asset_class <- seriesData$asset_class[match(addDF$underlyer,  seriesData$underlyer)]

# Look at distribution of Sharpe Ratios
ggplot(addDF, aes(x = value, color = factor(tenor))) + geom_line(stat='density') + facet_wrap(~asset_class)


#######
## Try to choose charts

find_charts <- function(df, gthan = -0.5, lthan = 0.5){
    
  first_true <- function(bool_vec){
    i = 1
    n = length(bool_vec)
    while(i < n){
      if(bool_vec[i]){return(i)}
      i = i + 1
    }
    return(0)
    }
  
  ndf <- nrow(df)
  safetyi <- 1
  i <- 1
  
  imatchdiff <- c()
  
  iright <- 1:ndf
  date_vec <- df$date
  und_vec <- df$underlyer
  val_vec <- df$value
  date_check <- -100000
  und_check <- "ABCDEFG"
  
  ########
  while(i < ndf & safetyi < 100000){
    icheck <- first_true((date_vec > (date_check + 365) | und_vec != und_check) & val_vec > gthan & val_vec < lthan)
    if(icheck > 0){
      imatchdiff <- c(imatchdiff, icheck)
      date_check <- date_vec[icheck]
      und_check <- und_vec[icheck]
      
      idrop <- (1:icheck)
      date_vec <- date_vec[-idrop]
      und_vec <- und_vec[-idrop]
      val_vec <- val_vec[-idrop]
      i = i + icheck
      safetyi = safetyi + 1
    }else{
      break
    }
  }
  
  imatch <- cumsum(imatchdiff)
  
  return(df[imatch,])
}


df12 <- subset(allDF, type == 'ZSCORE' & tenor == 12 & !is.na(value))

chartgroups <- c('VPOS','POS','FLAT','NEG','VNEG')
chartlists <- list(
  find_charts(df12, gthan = 1.5, lthan = 1000000),
  find_charts(df12, gthan = 0.5, lthan = 1.5),
  find_charts(df12, gthan = -0.5, lthan = 0.5),
  find_charts(df12, gthan = -1.5, lthan = -0.5),
  find_charts(df12, gthan = -100000, lthan = -1.5)
  )
names(chartlists) <- chartgroups

for(groupi in chartgroups){
  chartlists[[groupi]] <- transform(chartlists[[groupi]], group = groupi, groupid = 1:length(date))
}
chart_master <- do.call(rbind, chartlists)
chart_master$start_date <- as.character(chart_master$date - 356 + 1)
chart_master$end_date <- as.character(chart_master$date)
chart_master$sharpe <- chart_master$value

write.csv(
  chart_master[c('underlyer', 'start_date','end_date','group','groupid','sharpe')],
  paste0(path,'CHART_MASTER.csv'),
  row.names=F)

###########################################################
###########################################################

match_charts <- function(df, chartlist){
  index <- c()
  label <- c()
  for(i in 1:nrow(chartlist)){
    charti <- chartlist[i,]
    idxi <- which(
              df$underlyer == charti$underlyer &
                df$date < charti$date &
                df$date > (charti$date - 365),
              )
    index <- c(index, idxi)
    label <- c(label, rep(i, length(idxi)))
  }
  
  return(list(index=index, label=label))
}


pxDF <- subset(allDF, type == 'LEVEL' & !is.na(value))

#rowsample <- function(df, n){return(df[sample(1:nrow(df),n),])}
#nsample <- 20
#chartindex <- lapply(chartlists, function(x){match_charts(pxDF, rowsample(x, nsample))})
chartindex <- lapply(chartlists, function(x){match_charts(pxDF, x)})

index <- c()
label <- c()
group <- c()
for(i in 1:length(chartindex)){
  index <- c(index, chartindex[[i]]$index)
  label <- c(label, chartindex[[i]]$label)
  group <- c(group, rep(names(chartindex)[i], length(chartindex[[i]]$label)))
}

chartsDF <- pxDF[index,]
chartsDF$label <- label
chartsDF$group <- group

temp <- subset(chartsDF, group == 'VNEG' & label == 1)
head(temp)
tail(temp)

ggplot(subset(chartsDF, group == 'VNEG'), aes(x = date, y = value)) +
  geom_line() + facet_wrap(~label, scale = "free")
