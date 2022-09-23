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
theme_set(theme_classic())
###### functions ######
### round up/down to nearest value for the axes limits
round_any <- function(x, accuracy, func){func(x/ accuracy) * accuracy}


#### read in data
data<-
  read.csv("../tv_news_data/England 000 reach_TV_Graph_data.csv") %>% 
  rbind(read.csv("../tv_news_data/Wales 000 reach_TV_Graph_data.csv")) %>% 
  rbind(read.csv("../tv_news_data/Scotland 000 reach_TV_Graph_data.csv")) %>% 
  rbind(read.csv("../tv_news_data/NI 000 reach_TV_Graph_data.csv"))
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
## get simple axes, with min and max dates
temp <- clean_data %>%
  group_by(year, quarter) %>%
  summarise(quarter_min = min(week_commencing)) %>%
  ungroup() %>%
  select(quarter_min) %>%
  rbind(clean_data %>% summarise(quarter_min = max(week_commencing)))%>% as.list()

x_axis_dates <- temp$quarter_min 
x_axis_dates %>% length()
rm(temp)

## get lines for Christmas/new year
new_year<-ymd(c("2017-01-01","2018-01-01","2019-01-01",
  "2020-01-01","2021-01-01","2022-01-01"))

###### function to make graph
make_plot <- function(data, y_lab, y_value, split) {
  
  avg_traffic <<-
    data %>% #head() %>%
    group_by(year, region) %>%
    mutate(median_viewers = median(!!sym(y_value)),
           mean_viewers = mean(!!sym(y_value))) %>%
    select(year,
           week_commencing,
           region,
           median_viewers,
           mean_viewers)

  print(avg_traffic %>%
          select(year, median_viewers, mean_viewers) %>% unique() %>%
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
    geom_vline(xintercept = new_year,
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
  data = clean_data,
  y_lab = paste0("Viewers to BBC News ",england$region," Bulletin (w/c ",clean_data$week_commencing %>% min()," to ",clean_data$week_commencing %>% max(),")"),
  y_value = 'viewers',
  split = TRUE
)



############# Time Series ##################
library(tseries)
england <- clean_data %>% filter(region =='England') %>% 
  mutate(viewers = viewers/1000000) ## convert to millions
england %>% head()
england$week_commencing %>% min()
# Convert to time series
data_ts<-ts(england$viewers, 
            freq=365.25/7, #to get weeks in a year (2020 has 53 weeks rather than 52)
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
england %>% group_by(year) %>% summarise(min = round(min(viewers),2), 
                                         max = round(max(viewers),2),
                                         var = max-min)

#1. Remove the unequal variance 
# To do this use log of values.
plot(data_ts)
plot(log(data_ts))

#2. Need to do 'differencing' or making the sequence stationary and find the d value
log(data_ts[1])-log(data_ts[2]) #the difference between the log of first two values (this is what diff(log(ts_data)) does)
plot(diff(log(data_ts))) # this shows the sequence to be quite stable around a mean of 0 now.
abline(h=0, col="red")

## means
mean(data_ts) ##10.3
mean(log(data_ts)) ##2.3
mean(diff(log(data_ts))) # -0.0012
## variance
var(data_ts) ##1.8
var(log(data_ts)) ##0.015
var(diff(log(data_ts))) # -0.005

#This function checks how stationary it is mathematically
adf.test(diff(log(data_ts)),alternative="stationary",k=0 ) #the result of this shows that the series is stationary

## this dicky-fuller test looks to see if the scale factor for the previous value of y is 1
## i.e ## y(t) = c + bt + a y(t-1) with a=1

#Because we only needed to do the difference of y(t)-y(t-1) to get a stable series then the difference (d-value) = 1.




################# ACF & PACF plot to find 'q' and 'p' #################

#ACF (autocorrelation function) sees how different the current value is the the previous value
# lag 1 means the value vs one step different (i.e previous)
# lag 2 means the value vs two steps different (i.e one before the previous)
#PACF is a partial autocorrelation so it takes account of intermediary numbers when lag >1

# using the logged series
acf(log(data_ts)) #Where the ACF value fluctuates a lot
pacf(log(data_ts))

#Using the stationary sequence gained from taking the difference
acf(diff(log(data_ts))) #ACF value cuts off after the second lag. So q = 2 (as a starting point)
pacf(diff(log(data_ts)))#PACF There is one main significant spike, then one just above threshold. so p = 2 (as a starting point)

##within the blue lines statistically we can't determine as being different from zero so we use anything outside the blue lines

## this function can automatically suggest dpq
auto.arima()## gives you an automatic valur


################# Fit the model #################
#p=2,d=1,q=2
#The data is weekly so for a year period = 52 or 53
fit <- arima(log(data_ts), c(2, 1, 2),
             seasonal = list(order = c(2, 1, 2), 
                             period = 52.17857),
             method="CSS")
pred <- predict(fit, n.ahead = 300) #300 gives us the next 6 years 

ts.plot(data_ts,2.718^pred$pred, log = "y", lty = c(1,3), xlab="time")
ts.plot(2.718^pred$pred, log = "y", lty = c(1,3), xlab="time")
plot(data_ts)

#Get the predicted values and make into a data frame to plot with ggplot
pred_df<- data.frame(pred)
pred_df<-pred_df %>% mutate(num_clicks = 2.718^pred)

feb_march_dates<- data %>% filter(dt >= 20200201 & dt < 20200401 & attribute != 'rec_content') %>%
  select(dt,date,month_name) %>% unique()

dataPrediction <- 
  feb_march_dates %>% 
  cbind(pred_df %>% select(num_clicks))

dataPrediction<-dataPrediction %>% bind_rows(data %>% 
                                               filter(dt >= 20200101 & dt < 20200201 & attribute != 'rec_content')%>%
                                               select(dt,date,month_name,num_clicks)) 
dataPrediction$month_name<- factor(dataPrediction$month_name, levels = c("January", "February", "March"))
dataPrediction$date<- as.Date(dataPrediction$date)







