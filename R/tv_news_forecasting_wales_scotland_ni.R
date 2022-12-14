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

###### function to make graph
make_plot <- function(data, y_lab, y_value, split, colour_scheme) {
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
    scale_color_manual(values=c(colour_scheme$actual,colour_scheme$pred ))+
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

region_name = "NI"
### Basic plot
make_plot(
  data = clean_data %>% filter(region ==region_name),
  y_lab = paste0("Viewers to BBC News ",region_name," Bulletin (w/c ",clean_data$week_commencing %>% min()," to ",clean_data$week_commencing %>% max(),")"),
  y_value = 'viewers',
  split = TRUE,
  colour_scheme = nation_colours %>% filter(nation ==region_name)
)

############## remove covid and get data ###########

make_time_series<-function(data, region_needed){
  
  region <<- data %>% filter(region == region_needed) %>%
    mutate(viewers = viewers / 1000000) %>% ## convert to millions
    filter(week_commencing <= '2020-03-02' |
             week_commencing >= '2021-03-01') %>%
    filter(paste0(year, quarter) != '20223')
  
  region$week_commencing %>% max() %>% print()
  region$week_commencing %>% min() %>% print()
  
  
  data_ts<<-ts(region$viewers, 
              freq= 365.25/7, #to get weeks in a year (2020 has 53 weeks rather than 52)
              start=decimal_date(ymd(region$week_commencing %>% min()))
  )

  
 
}

################ Time series ########################
library(tseries)
make_time_series(clean_data, region_name)


##plot
plot(data_ts)
abline(reg=lm(data_ts~time(data_ts))) 

##decompose
plot(decompose(data_ts))

## stationary?
data_ts %>% ur.kpss() %>% summary()
data_ts %>% log() %>% ur.kpss() %>% summary() ##basically the same

##differencing?
ndiffs(data_ts%>% log()) #1 for NI suggest 0

range = data_ts %>% log() %>% max() -data_ts %>% log() %>% min()
middle = data_ts %>% log() %>% min() +0.5*range

plot(data_ts %>% log() %>% diff())
abline(h = 0, col = "red")
abline(h = middle, col = "red")

## stationary?
data_ts %>% log() %>% diff() %>% ur.kpss() %>% summary()

## Seasonality?
data_ts %>% log() %>% nsdiffs() ##0

## having a look
plot(data_ts %>% log() %>% diff() )
abline(h=0, col="red")

##ACF and PACF
acf(data_ts%>% log() %>% diff()) # 1 and 2
pacf(data_ts %>% log() %>% diff())# 


#pdq = 1,1,0
#PDQ = 1,1,0

fit <- forecast::Arima(log(data_ts), order  = c(2,1,1),
                       seasonal = list(order = c(1, 1, 1), period =52.17857),
                       method = 'CSS')
fit
checkresiduals(fit)
pred <- predict(fit, n.ahead = 438)

ts.plot(2.718^pred$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts, 2.718^pred$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")

## automatic model
fit_auto <- auto.arima(log(data_ts))
fit_auto ##suggests 1,0,1 with 1,0,0
checkresiduals(fit_auto) ## residuals are white noise.

pred_auto <- predict(fit_auto, n.ahead = 440) #438 gives until the end of 2030

ts.plot(2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts, 2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")
##just plots flat

######## plot
pred_df<- data.frame(pred)
pred_df<-pred_df %>% mutate(viewers = 2.718^pred)

## get a list of w/c dates
week_commencing <- seq(x_axis_dates[length(x_axis_dates)]+7, by = "week", length.out = nrow(pred_df))
week_commencing %>% min()
week_commencing %>% max()

# make a df to plot with ggplot
covid<-
  clean_data %>% 
  filter(region == region_name) %>%
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
    region %>% select(year, quarter, week_commencing, viewers) %>% mutate(actual_pred = 'actual')
  ) %>%
  rbind(covid) %>% ## add this back in for the plot
  rbind(
    clean_data %>%
      filter(region == region_name) %>%
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

## plot forecast

make_forecast_plot(
  data = forecast_data %>% mutate(region = region_name) %>% filter(year <2031),
  title = paste0("Predicted viewers to local BBC News from ",
                 week_commencing %>% min(),
                 " to ",
                 week_commencing %>% max()
  ),
  y_lab = "Weekly viewers (million)",
  y_value = 'viewers',
  split = TRUE,
  colour_scheme = nation_colours %>% filter(nation ==region_name)
)



write.csv(
  forecast_data %>%
    mutate(region = region_name) %>%
    filter(year < 2031) %>% 
    mutate('viewers_milions' = signif(viewers,3)) %>% 
    select(region, year, quarter, week_commencing, viewers_milions, actual_pred),
  file = paste0("forecasts/", region_name, "_forecast.csv"),
  col.names = TRUE,
  row.names =   FALSE
)


