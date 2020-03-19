

# time series



## Data from here:
## https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide 
# source("project.R")

ts$DateRep <- as.Date(ts$DateRep,format = '%d/%m/%Y')
country <- sort(as.character(unique(ts$Countries.and.territories)))

time_frame <- range(ts$DateRep)
n_days <- diff(time_frame) +1

I <- as.data.frame(matrix(NA,n_days,length(country)+1))
names(I) <- c('dates',country)

I[,1] <- seq(time_frame[1],time_frame[2],1)

for (i in 1:length(country)){
  f <- which(ts$Countries.and.territories %in% country[i])
  temp <- ts[f,]
  for (j in 1:nrow(temp)){
    f <- which( I$dates %in% temp$DateRep[j])
    if(length(f)>1){
      print('warning')
      break
    }
    if(length(f)==1){
      I[f,i+1] <- temp$Cases[j]
    }
  }
}

I[is.na(I)] <- 0
I_full <- I






# format up to week of interest



f<- which(I_full$dates <= date_week_finishing)

I_full <- I_full[f,]



# eliminate country with less than 100 cases for previous month (4 weeks)
# AND country with less than 10 cases in the last 7 days


Threshold_criterion_4weeks <- 100
d_limit_4weeks <-  4*7

Threshold_criterion_7days <- 10
d_limit_7days <-  7

I_last_4weeks <- tail(I_full,d_limit_4weeks)
I_last_7days <-  tail(I_full,d_limit_7days)
f <- which((colSums(I_last_4weeks[,-1]) >= Threshold_criterion_4weeks) &
             (colSums(I_last_7days[,-1]) >= Threshold_criterion_7days))

I_active_transmission <- I_full[,c(1,f+1)]

# save

data <- list(date_week_finishing = date_week_finishing,
             Threshold_criterion_4weeks = Threshold_criterion_4weeks,
             Threshold_criterion_7days = Threshold_criterion_7days,
             I_active_transmission = I_active_transmission,
             Country = colnames(I_active_transmission)[-1],
             si_mean = si_mean,
             si_std = si_std)

saveRDS(data, file = paste0('data_',date_week_finishing,'.rds'))


