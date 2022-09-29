#### forecast nations
options(java.parameters = "-Xmx64g")
library(tidyverse)
library(ggplot2)
library(readxl)
library(RJDBC)
library(RColorBrewer)
library(wesanderson)
library(formattable)
library(lubridate)
library(scales)
library(ggrepel)
library(forecast)
library(zoo)
theme_set(theme_classic())
###### functions ######
### round up/down to nearest value for the axes limits
round_any <- function(x, accuracy, func){func(x/ accuracy) * accuracy}

#### read in data
data <-
  read.csv("../tv_news_data/England 000 reach_TV_Graph_data.csv") %>%
  rbind(read.csv("../tv_news_data/Wales 000 reach_TV_Graph_data.csv")) %>%
  rbind(read.csv("../tv_news_data/Scotland 000 reach_TV_Graph_data.csv")) %>%
  rbind(read.csv("../tv_news_data/NI 000 reach_TV_Graph_data.csv")) %>% 
  rbind("../tv_news_data/2022/England 000 reach_TV_Graph_data.csv") %>%
  rbind(read.csv("../tv_news_data/2022/Wales 000 reach_TV_Graph_data.csv")) %>%
  rbind(read.csv("../tv_news_data/2022/Scotland 000 reach_TV_Graph_data.csv")) %>%
  rbind(read.csv("../tv_news_data/2022/NI 000 reach_TV_Graph_data.csv")) %>% 
  distinct()
data$Calc.Weighted.Metrics<-as.numeric(gsub(",","", data$Calc.Weighted.Metrics))


clean_data <- data %>% mutate(
  viewers = 1000 * Calc.Weighted.Metrics,
  year = Year,
  week = Calc.Period,
  month = sub('-.*', '', Month.Year.Format),
  region = Region
) %>%
  filter(Channel == 'BBC') %>%
  select(region,year, month, week, viewers) %>% 
  mutate(week_commencing = ymd(strptime(paste0(year, "Monday", week), "%Y%A%U"))) %>% ## add in week commencing
  mutate(quarter = quarter(week_commencing))



#######  EDA #########

###### function to make graph
make_plot <- function(data, y_lab, y_value, split) {
  ## get simple axes, with min and max dates
  temp <- data %>%
    group_by(year, quarter) %>%
    summarise(quarter_min = min(week_commencing)) %>%
    ungroup() %>%
    select(quarter_min) %>%
    rbind(data %>% summarise(quarter_min = max(week_commencing)))%>% as.list()
  
  x_axis_dates <<- temp$quarter_min 
  rm(temp)
  
  ## get lines for new year
  new_year<<- data %>% group_by(year) %>% summarise(week_commencing = min(week_commencing))
  
  avg_traffic <<-
    data %>% #head() %>%
    group_by(year, quarter, region) %>%
    mutate(median_viewers = median(!!sym(y_value)),
           mean_viewers = mean(!!sym(y_value))) %>%
    select(year,
           quarter,
           week_commencing,
           region,
           median_viewers,
           mean_viewers)

  print(avg_traffic %>%
          select(year, quarter,median_viewers, mean_viewers) %>% unique() %>%
          mutate(median_viewers = signif(median_viewers,2),
                 mean_viewers = signif(mean_viewers,2))
  )
  
  ggplot(data = data, aes(x = week_commencing, colour  = region)) +
    geom_point(aes(y = viewers)) +
    ylab("viewers") +
    xlab("Date") +
    labs(title = y_lab) +
    {if(split == TRUE) 
      scale_y_continuous(label = comma,
                         limits = ~ c(min(.x), max(.x)*1.2),
                         n.breaks = 10)
      else scale_y_continuous(label = comma,
                              limits = ~ c(min(.x), max(.x)*1.1),
                              n.breaks = 20)} +
    geom_vline(xintercept = new_year$week_commencing,
               linetype = "dashed",
               color = "grey") +
    scale_x_date(
      labels = date_format("%Y-%m-%d"),
      breaks = x_axis_dates,
      sec.axis = sec_axis(
        name = NULL,
        trans = ~ .,
        labels = function(x)
          format(as.yearqtr(x), "%y Q%q")
      )
    ) +
    facet_wrap(~ region, nrow = 4, scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid.major.y = element_line(size = .1, color = "grey") ,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )+
    geom_line(data = avg_traffic ,
              aes(x = week_commencing, y = median_viewers),
              linetype = "dashed", colour = "black"
    )

  # return(plot)
}



### Basic plot
make_plot(
  data = clean_data %>% filter(region =='England'),
  y_lab = paste0("Viewers to BBC News ",clean_data$region," Bulletin (w/c ",clean_data$week_commencing %>% min()," to ",clean_data$week_commencing %>% max(),")"),
  y_value = 'viewers',
  split = TRUE
)



############# Time Series ##################
library(tseries)
england <- clean_data %>% filter(region =='England') %>% 
  mutate(viewers = viewers/1000000) ## convert to millions

### remove covid ###
england<-england %>% filter(week_commencing<= '2020-03-02' | week_commencing>= '2021-03-01' ) %>% 
  filter(paste0(year,quarter)!='20223') ##remove the half quarter
# write.csv(england, file = 'england_no_covid.csv', row.names = FALSE)
# england<- read.csv("england_no_covid.csv") %>% 
#   mutate(week_commencing = ymd(strptime(paste0(year, "Monday", week), "%Y%A%U")))

england %>% head()


make_plot(
  data = england, 
  y_lab = paste0("Viewers to BBC News ",clean_data$region," Bulletin (w/c ",clean_data$week_commencing %>% min()," to ",clean_data$week_commencing %>% max(),")"),
  y_value = 'viewers',
  split = TRUE
)

england %>% head()
england$week_commencing %>% min()
# Convert to time series
data_ts<-ts(england$viewers, 
            freq= 365.25/7, #to get weeks in a year (2020 has 53 weeks rather than 52)
            start=decimal_date(ymd(england$week_commencing %>% min()))
)
data_ts

######## try some simple models ##########

plot(data_ts)
abline(reg=lm(data_ts~time(data_ts))) 


### mean -> this just predicts the average value
plot(meanf(data_ts, h = 150), main = "Forecasting based on mean value", )
data_meanf<-meanf(data_ts, h = 150)

## naive - uses the last observation to predict the future
plot(naive(data_ts, h = 150), main = "Forecasting based on last observations (naive)")
data_naive<-naive(data_ts, h = 150)


## seasonal naive forecast - there is not really  seasonal trend
plot(snaive(data_ts,h = 150), main = "Forecasting based on last seasonal observations (snaive)")
data_snaive<-snaive(data_ts, h = 150)


## drift forecast - draws a line between the first and last observations and uses that
plot(rwf(data_ts, h = 150, drift = T), main = "Forecasting based on fisrt and last observations (rwf)")
data_rwf<-rwf(data_ts, h = 150, drift = T)


### plot together
plot(data_meanf,  main = "Simple Forecasting Models")
lines(data_naive$mean, col = 5, lty =1)
lines(data_snaive$mean, col = 2, lty =1)
lines(data_rwf$mean, col = 6, lty =1)
legend("bottomleft",  lty = 1, col = c(1,5,2,6), 
       legend = c(
         "Mean method", "Naive method", "Seasonal Naive method", "Drift method"
       ) )



############## Time series ##############
## generally the value of y at time t is given by at least 3 factors
# the previous value of y (t-1) multiplied by some scale factor 'a' 
# a value 'b' dependent on time t  i.e. 'bt'
# a constant value 'c'
## y(t) = c + bt + a y(t-1)

#https://rpubs.com/neharaut05/TimeSeries_AirPassangerForecast

########### Decompose ###########
#Decomposition break data into trend, seasonal, regular and random
plot(decompose(data_ts)) # time series decomposition


################# Make the series stationary #################
library(urca)
plot(data_ts)
##Is the sequence stationary already?
## use kpss test
data_ts %>% ur.kpss() %>% summary()
## the value is 0.7447 slightly higher than the 1% significance level (england covid incl)
## suggests that the null hypothesis is rejected -> so the sequence is NOT stationary

# Use the log value to help stablise the mean
data_ts %>% log() %>% ur.kpss() %>% summary() ##  value  = 0.8551 (england covid incl)


##How much differencing is suggested? 
ndiffs(data_ts) #1 
data_ts %>% log() %>% ndiffs() #1

#log(data_ts[1])-log(data_ts[2]) #the difference between the log of first two values (this is what diff(log(ts_data)) does)

## what does the differenced data look like?
plot(data_ts %>% log() %>% diff()) # this shows the sequence to be quite stable around a mean of 0 now.
abline(h=0, col="red")

### check how stationary it is now.
data_ts %>% log() %>% diff() %>% ur.kpss() %>% summary() ## 0.04, well below the critical value (so stationary)
#Because we only needed to do the difference of y(t)-y(t-1) to get a stable series then the difference (d-value) = 1.



############ Is there a seasonal difference? ###########
plot(data_ts %>% log()) #viewing is higher in the first months of the year according to the more detailed plot fros line 128
data_ts %>% log() %>% diff()%>% nsdiffs() ## this suggest no seasonal component (when covid is included AND when it's not included)

## but the plot per quarter visually shows higher viewing in the winter
## use diff(lag = 52) to account for this
plot(data_ts %>% log() %>% diff(lag = 52) %>% diff() )
abline(h=0, col="red")

################# ACF & PACF plot to find 'q' and 'p' #################

#ACF (autocorrelation function) sees how different the current value is the the previous value
# lag 1 means the value vs one step different (i.e previous)
# lag 2 means the value vs two steps different (i.e one before the previous)
#PACF is a partial autocorrelation so it takes account of intermediary numbers when lag >1

#Using the stationary sequence gained from taking the difference
acf(data_ts %>% log() %>% diff()) #ACF value cuts off after the second lag. So p = 2 (as a starting point)
pacf(data_ts %>% log() %>% diff())#PACF There is one main significant spike, then one just above threshold. so p = 2 (as a starting point)

##within the blue lines statistically we can't determine as being different from zero so we use anything outside the blue lines
acf(data_ts %>% log() %>% diff(lag = 52)%>% diff()) # this still suggests 2
pacf(data_ts %>% log() %>% diff(lag = 52)%>% diff()) ## this gives 8 lags outside the threshold


## this function can automatically suggest dpq
#auto.arima()## gives you an automatic value


################# Fit the model #################
#p=2,d=1,q=0

fit <- forecast::Arima(log(data_ts), order  = c(2,1,1),
             seasonal = list(order = c(2, 1, 1), period = 52.17857),
             method = 'CSS'
             )
fit
checkresiduals(fit)## this ACF plot shows no residuals outside the threshold - so white noise 
### trying different seasonal components, 0,0,0 actually looks best

pred <- predict(fit, n.ahead = 442)

ts.plot(2.718^pred$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts, 2.718^pred$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")
#### this just plots flat. 


## automatic model
fit_auto <- auto.arima(log(data_ts)) ## sets ARIMA(0,1,1)
fit_auto ##suggests 0,1,1
checkresiduals(fit_auto) ## residuals are white noise.

pred_auto <- predict(fit_auto, n.ahead = 442) #442 gives until the end of 2030

ts.plot(2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts, 2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")
                                            
#plot(data_ts)

####### summary ########
## without a seasonal component the model predicts flat, which is highly unlikely
## to the eye there is a seasonal component!


#Get the predicted values and make into a data frame to plot with ggplot
pred_df<- data.frame(pred)
pred_df<-pred_df %>% mutate(viewers = 2.718^pred)

## get a list of w/c dates
week_commencing <- seq(x_axis_dates[length(x_axis_dates)]+7, by = "week", length.out = nrow(pred_df))
week_commencing %>% min()
week_commencing %>% max()

england %>% head()

## make a df to plot with ggplot
covid<-
  clean_data %>% 
  filter(region == 'England') %>%
  mutate(viewers = viewers / 1000000) %>%
  filter(week_commencing > ymd('2020-03-02')) %>%
  filter(week_commencing < ymd('2021-03-01')) %>% 
  select(year, quarter, week_commencing, viewers) %>% 
  mutate(actual_pred = 'actual' )

forecast_data <-
  data.frame(week_commencing) %>%
  cbind(pred_df %>% select(viewers)) %>%
  mutate(
    quarter = quarter(week_commencing),
    year = year(week_commencing),
    week_commencing = ymd(week_commencing),
    viewers = as.numeric(viewers),
    actual_pred = 'predicted'
  ) %>%
  select(year, quarter, week_commencing, viewers, actual_pred) %>%
  rbind(
    england %>% select(year, quarter, week_commencing, viewers) %>% mutate(actual_pred = 'actual')
  ) %>%
  rbind(covid) %>% ## add this back in for the plot
  rbind(
    clean_data %>%
      filter(region == 'England') %>%
      mutate(viewers = viewers / 1000000) %>%
      filter(paste0(year, quarter) == '20223') %>% 
      select(year, quarter, week_commencing, viewers) %>% mutate(actual_pred = 'actual')
  ) %>% ##add in the few dates that were removed to make plotting easier
  arrange(week_commencing)
  



########### make forecast plot ##########
make_forecast_plot <- function(data, y_lab,title, y_value, split, colour_scheme) {
  ## get simple axes, with min and max dates
  temp <- data %>%
    group_by(year) %>%
    summarise(year_min = min(week_commencing)) %>%
    ungroup() %>%
    select(year_min) %>%
    rbind(data %>% summarise(year_min = max(week_commencing)))%>% as.list()
  
  x_axis_dates <- temp$year_min 
  rm(temp)
  
  ## get lines for new year
  new_year<<- data %>% group_by(year) %>% summarise(week_commencing = min(week_commencing))
  
  avg_traffic <<-
    data %>% #head() %>%
    group_by(year, region) %>%
    mutate(median_viewers = median(!!sym(y_value))) %>%
    select(year,
           week_commencing,
           region,
           median_viewers)
  
  print(
    avg_traffic %>%
      select(year, median_viewers) %>% unique() %>%
      mutate(median_viewers = signif(median_viewers, 2)) %>%
      ungroup() %>%
      mutate(previous = round(
        (median_viewers - lag(median_viewers, order_by = year)) / median_viewers, 3
      )
      )
  )
  
  ggplot(data = data, aes(x = week_commencing)) +
    geom_point(aes(y = viewers,  colour = actual_pred)) +
    ylab(y_lab) +
    xlab("Date") +
    labs(title = title) +
    scale_y_continuous(
      label = comma,
      limits = ~ c(min(.x), max(.x) * 1.1),
      n.breaks = 20
    ) +
    scale_color_manual(values=c(colour_scheme$actual,colour_scheme$pred ))+
    geom_vline(
      xintercept = new_year$week_commencing,
      linetype = "dashed",
      color = "grey"
    ) +
    scale_x_date(
      labels = date_format("%Y-%m-%d"),
      breaks = x_axis_dates,
      sec.axis = sec_axis(
        name = NULL,
        breaks = x_axis_dates,
        trans = ~ .,
        labels = date_format("%Y")
      )
    ) +
    facet_wrap( ~ region, nrow = 4, scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid.major.y = element_line(size = .1, color = "grey") ,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title=element_blank()
    ) +
    geom_line(
      data = avg_traffic ,
      aes(x = week_commencing, y = median_viewers),
      linetype = "dashed",
      colour = "black"
    )
  
  # return(plot)
}


# #get default colour codes already used and find a lightened version for the predicted value
colours <- hue_pal()(4)

nation_colours <-
  sapply(colours, function(x) {
    colorRampPalette(c(x, "#FFFFFF"))(10)
  }) %>%
  as.data.frame() %>%
  filter(row_number() == 5) %>%
  gather(key = actual.color, value = pred.color) %>%
  cbind(data.frame(nation = c('England', "NI", "Scotland", "Wales")))

x <- nation_colours %>% filter(nation == 'England')
x$actual.color
x$pred.color

## plot forecast
make_forecast_plot(
  data = forecast_data %>% mutate(region = 'England'),
  title = paste0("Predicted viewers to local BBC News from ",
                 week_commencing %>% min(),
                 " to ",
                 week_commencing %>% max()
                 ),
  y_lab = "Weekly viewers (million)",
  y_value = 'viewers',
  split = TRUE,
  colour_scheme = nation_colours %>% filter(nation =='England')
)





############### try using quarterly data ###############
england_qrtr<- england %>% 
  filter(week_commencing<= '2020-03-02' | week_commencing>= '2021-03-01' ) %>% 
  filter(paste0(year,quarter)!='20223' ) %>% 
  group_by(year, quarter) %>% 
  summarise(viewers = median(viewers))

data_ts_qrtr<-ts(england_qrtr$viewers, 
                 freq= 4, #to get weeks in a year (2020 has 53 weeks rather than 52)
                 start=decimal_date(ymd(england$week_commencing %>% min()))
)

##plot
plot(data_ts_qrtr)
abline(reg=lm(data_ts_qrtr~time(data_ts_qrtr))) 

##decompose
plot(decompose(data_ts))

## stationary?
data_ts_qrtr %>% ur.kpss() %>% summary()
data_ts_qrtr %>% log() %>% ur.kpss() %>% summary() ##basically the same

##differencing?
ndiffs(data_ts_qrtr%>% log()) #1 

plot(data_ts_qrtr %>% log()%>% diff())
abline(h = 0, col = "red")

## stationary?
data_ts_qrtr %>% log() %>% diff() %>% ur.kpss() %>% summary()

## Seasonality?
data_ts_qrtr %>% nsdiffs() ##0

## but the plot per quarter visually shows higher viewing in the winter
## use diff(lag = 4) to account for this
plot(data_ts_qrtr %>%log() %>% diff(lag = 4) %>% diff() ) ## not good
abline(h=0, col="red")

##ACF and PACF
acf(data_ts_qrtr%>% log() %>% diff()) # 1 and almost at 3
pacf(data_ts_qrtr %>% log() %>% diff())# at 2 only

acf(data_ts_qrtr %>% log() %>% diff(lag = 4)%>% diff()) # 1
pacf(data_ts_qrtr %>% log() %>% diff(lag = 4)%>% diff()) ## t0

#pdq = 1,1,0
#PDQ = 1,1,0

fit <- forecast::Arima(log(data_ts_qrtr), order  = c(1,1,0),
                       seasonal = list(order = c(1, 1, 0), period =4),
                       method = 'CSS')
fit
checkresiduals(fit)
pred <- predict(fit, n.ahead = 34)

ts.plot(2.718^pred$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts_qrtr, 2.718^pred$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")

## automatic model
fit_auto <- auto.arima(log(data_ts_qrtr))
fit_auto ##suggests 0,1,0
checkresiduals(fit_auto) ## residuals are white noise.

pred_auto <- predict(fit_auto, n.ahead = 34) #438 gives until the end of 2030

ts.plot(2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts_qrtr, 2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")
##just plots flat


#### sort the data set to plot
#Get the predicted values and make into a data frame to plot with ggplot
pred_df<- data.frame(pred)
pred_df<-pred_df %>% mutate(viewers = 2.718^pred)

## get a list of w/c dates
week_commencing <- seq(x_axis_dates[length(x_axis_dates)]+7, by = "week", length.out = 438)
week_commencing %>% min()
week_commencing %>% max()

england_qrtr %>% head()

weeks <-  data.frame(week_commencing) %>%
  mutate(year = year(week_commencing),
    quarter = quarter(week_commencing))
weeks


## make a df to plot with the ggplot fucntion from before
forecast_data <-
  weeks %>% 
  left_join(pred_df %>% select(viewers) %>% 
              cbind(weeks %>% select(year, quarter) %>% unique()),
            by = c("year", "quarter")
            ) %>%
  mutate(
    viewers = as.numeric(viewers),
    actual_pred = 'predicted'
  ) %>%
  select(year, quarter, week_commencing, viewers, actual_pred) %>%
  rbind(england %>% select(year, quarter, week_commencing, viewers) %>% mutate(actual_pred = 'actual')) %>%
  arrange(week_commencing)

## plot forecast
make_forecast_plot(
  data = forecast_data %>% mutate(region = 'England'),
  title = paste0("Predicted viewers to local BBC News from ",
                 week_commencing %>% min(),
                 " to ",
                 week_commencing %>% max()
  ),
  y_lab = "Weekly viewers (million)",
  y_value = 'viewers',
  split = TRUE,
  colour_scheme = nation_colours %>% filter(nation =='England')
)


