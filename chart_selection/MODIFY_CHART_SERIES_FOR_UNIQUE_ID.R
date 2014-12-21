
transform_csv = function(fn, path, new_name = paste0('test_',fn)){
  
  csvin = read.csv(paste0(path,'/',fn), stringsAsFactors = F)
  
  csvout = csvin
  
  csvout$groupid = with(csvin,groupid + 1000*
                        ifelse(
                          group=='VNEG',1,ifelse(
                            group=='NEG',2,ifelse(
                              group=='FLAT',3,ifelse(
                                group=='POS',4,5)
                              )
                            )
                          )
                        )
  
  write.csv(csvout, paste0(path,'/',new_name), row.names=F)

}

# Data path
path = "" #"F:/Docs/Personal/MIDS/2014Fall/SHARPE_RATIOS/sharpenull/public"

# File name
fn = 'old_CHART_RESULT_SERIES.csv'

transform_csv(fn, path, new_name = 'CHART_RESULT_SERIES.csv')

fn = 'old_CHART_SERIES.csv'

transform_csv(fn, path, new_name = 'CHART_SERIES.csv')



