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



### Basic plot
make_plot(
  data = clean_data %>% filter(region =='NI'),
  y_lab = paste0("Viewers to BBC News ","Northern Ireland"," Bulletin (w/c ",clean_data$week_commencing %>% min()," to ",clean_data$week_commencing %>% max(),")"),
  y_value = 'viewers',
  split = TRUE,
  colour_scheme = nation_colours %>% filter(nation =='NI')
)

############## remove covid and get data ###########

make_time_series<-function(data, region_needed){
  
  region <<- data %>% filter(region == region_needed) %>%
    mutate(viewers = viewers / 1000000) %>% ## convert to millions
    filter(week_commencing <= '2020-03-02' |
             week_commencing >= '2021-03-01') %>%
    filter(paste0(year, quarter) != '20223')
  
  
  data_ts<<-ts(region$viewers, 
              freq= 365.25/7, #to get weeks in a year (2020 has 53 weeks rather than 52)
              start=decimal_date(ymd(region$week_commencing %>% min()))
  )

  
 
}

################ Time series ########################
make_time_series(clean_data, "NI")


##plot
plot(data_ts)
abline(reg=lm(data_ts~time(data_ts))) 

##decompose
plot(decompose(data_ts))

## stationary?
data_ts %>% ur.kpss() %>% summary()
data_ts %>% log() %>% ur.kpss() %>% summary() ##basically the same

##differencing?
ndiffs(data_ts%>% log()) #1 

plot(data_ts %>% log()%>% diff())
abline(h = 0, col = "red")

## stationary?
data_ts %>% log() %>% diff() %>% ur.kpss() %>% summary()

## Seasonality?
data_ts %>% nsdiffs() ##0

## but the plot per quarter visually shows higher viewing in the winter
## use diff(lag = 4) to account for this
plot(data_ts %>%log() %>% diff(lag = 4) %>% diff() ) ## not good
abline(h=0, col="red")

##ACF and PACF
acf(data_ts%>% log() %>% diff()) # 1 and almost at 3
pacf(data_ts %>% log() %>% diff())# at 2 only

acf(data_ts %>% log() %>% diff(lag = 4)%>% diff()) # 1
pacf(data_ts %>% log() %>% diff(lag = 4)%>% diff()) ## t0

#pdq = 1,1,0
#PDQ = 1,1,0

fit <- forecast::Arima(log(data_ts), order  = c(1,1,0),
                       seasonal = list(order = c(1, 1, 0), period =4),
                       method = 'CSS')
fit
checkresiduals(fit)
pred <- predict(fit, n.ahead = 34)

ts.plot(2.718^pred$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts, 2.718^pred$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")

## automatic model
fit_auto <- auto.arima(log(data_ts))
fit_auto ##suggests 0,1,0
checkresiduals(fit_auto) ## residuals are white noise.

pred_auto <- predict(fit_auto, n.ahead = 34) #438 gives until the end of 2030

ts.plot(2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time", ylab = "viewers (mil)")
ts.plot(data_ts, 2.718^pred_auto$pred, log = "y", lty = c(1,3), xlab="time",ylab = "viewers (mil)")
##just plots flat












