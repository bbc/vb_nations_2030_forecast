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


### locally
driver <-JDBC("com.amazon.redshift.jdbc41.Driver","~/.redshiftTools/redshift-driver.jar",identifier.quote = "`")
my_aws_creds <-read.csv("~/Documents/Projects/DS/redshift_creds.csv",header = TRUE,stringsAsFactors = FALSE)
url <-paste0("jdbc:redshift://localhost:5439/redshiftdb?user=",my_aws_creds$user,"&password=",my_aws_creds$password)
conn <- dbConnect(driver, url)


######### Get Redshift creds  MAP #########
# get_redshift_connection <- function() {
#   driver <-JDBC(driverClass = "com.amazon.redshift.jdbc.Driver",classPath = "/usr/lib/drivers/RedshiftJDBC42-no-awssdk-1.2.41.1065.jar",identifier.quote = "`")
#   url <-str_glue("jdbc:redshift://live-idl-prod-redshift-component-redshiftcluster-1q6vyltqf8lth.ctm1v7db0ubd.eu-west-1.redshift.amazonaws.com:5439/redshiftdb?user={Sys.getenv('REDSHIFT_USERNAME')}&password={Sys.getenv('REDSHIFT_PASSWORD')}")
#   conn <- dbConnect(driver, url)
#   return(conn)
# }
# #Variable to hold the connection info
# conn <- get_redshift_connection()

dbGetQuery(conn,"select distinct brand_title, series_title from prez.scv_vmb ORDER BY RANDOM() limit 10;")

## get news regions data
sql<- "SELECT week_commencing, page_producer, visitors_raw as visitors FROM central_insights_sandbox.vb_news_regions_historic_users;"
nations_traffic<-dbGetQuery(conn, sql)
nations_traffic$week_commencing<- ymd(nations_traffic$week_commencing)
nations_traffic$visitors <- as.numeric(nations_traffic$visitors)

nations_traffic %>% head()

## get just england data
england<-nations_traffic %>% filter(page_producer =='England')
england %>% class()
## set up some dates to go on the axis
new_year<-ymd(c("2020-01-01","2021-01-01","2022-01-01"))
## get simple axes, with min and max dates
library(zoo)
nations_traffic$year_month <- as.Date(as.yearmon(nations_traffic$week_commencing))
nations_traffic$quarter<-quarter(nations_traffic$week_commencing)
nations_traffic$year<-year(nations_traffic$week_commencing)

nations_traffic %>%  
  group_by(year, quarter) %>% 
  summarise(quarter_min = min(year_month))

#### Make time series graph
nations_traffic %>% tail()
## get simple axes, with min and max dates
temp <- nations_traffic %>%
  group_by(year, quarter) %>%
  summarise(quarter_min = min(year_month)) %>%
  ungroup() %>%
  select(quarter_min) %>%
  rbind(nations_traffic %>% summarise(quarter_min = max(week_commencing)))%>% as.list()

x_axis_dates <- temp$quarter_min 
x_axis_dates %>% length()
rm(temp)


###############  plot ###############
make_plot <- function(data, y_lab, y_value) {
  data$year <- year(data$week_commencing)
  
  avg_traffic <-
    data %>% #head() %>%
    group_by(page_producer, year) %>%
    mutate(median_visitors = median(!!sym(y_value)),
           mean_visitors = mean(!!sym(y_value))) %>%
    select(year,
      week_commencing,
           page_producer,
           median_visitors,
           mean_visitors)
  
  print(avg_traffic %>% 
          select(year, median_visitors, mean_visitors) %>% unique() %>% 
          mutate(median_visitors = signif(median_visitors,3),
                 mean_visitors = signif(mean_visitors,3))
        )
  
plot<-ggplot(data = data, aes(x = week_commencing)) +
  geom_line(aes(y = visitors)) +
  ylab("Visitors") +
  xlab("Date") +
  labs(title = y_lab) +
  scale_y_continuous(
    label = comma,
    limits = ~ c(min(.x), max(.x) * 1),
    n.breaks = 20
  ) +
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
  facet_wrap(~ page_producer, nrow = 4, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = .1, color = "grey") ,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  geom_line(data = avg_traffic,
            aes(x =week_commencing, y = median_visitors),
            linetype = "dashed"
  )

return(plot)
}


### Basic plot
make_plot(
  data = england,
  y_lab = paste0("Visitors to BBC News England Nations pages (w/c 2019-03-04 to ",england$week_commencing %>% max(),")"),
  y_value = 'visitors'
  )

### only from after the 2020 step change
england %>% filter(week_commencing >ymd('2020-03-01')) %>% head()
make_plot(
  data = england  %>% filter(week_commencing >ymd('2020-03-01')),
  y_lab = paste0("Visitors to BBC News England Nations pages (w/c 2019-03-04 to ",england$week_commencing %>% max(),")"),
  y_value = 'visitors'
  )

###### Time series notes ########
#https://www.youtube.com/watch?v=8cKeAH2aGVI

# Simple models - for completely or mostly random models
  # - averages of all observations to use to predict `meanf()`
  # - naive method  takes the last observations and uses them `naive()`
  # - seasonal naive forecast takes the last observation from the same period e.g feb 2020 to predict feb 2021 `snaive()`
  # - drift method take first and last observations and draws a line between (good for linear) `rwf()`

# Complex models - for data with trend or pattern


############# Time Series ##################
library(tseries)
england_2020<- england %>% filter(week_commencing >ymd('2020-03-01')) %>% mutate(visitors_mil = round(visitors/1000000,1)) %>% select(-visitors)
england<- england%>% mutate(visitors_mil = round(visitors/1000000,1)) %>% select(-visitors)
england %>% head()
# Convert to time series
data_ts<-ts(england$visitors_mil, 
            freq=365.25/7, #to get weeks in a year (2020 has 53 weeks rather than 52)
            start=decimal_date(ymd("2020-03-02"))
            )
data_ts

################# Investigate series #################
class(data_ts)
start(data_ts)
end(data_ts)
frequency(data_ts)
summary(data_ts)

plot(data_ts)

abline(reg=lm(data_ts~time(data_ts))) 
cycle(data_ts)



######## try some simple models ##########

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


###### Trends ######
## there are very few visually obvious trends

plot(data_ts)

## look for a moving average 
lines(ma(data_ts, order =12, centre = T))






