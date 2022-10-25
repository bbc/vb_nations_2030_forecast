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


theme_set(theme_classic())
options(scipen = 999)
###### functions ######
### round up/down to nearest value for the axes limits
round_any <- function(x, accuracy, func){func(x/ accuracy) * accuracy}

#download.file('http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar','~/.redshiftTools/redshift-driver.jar')

## get news regions data historic from excel sheet
historic_web_usage <- read_excel("~/Documents/Projects/DS/vb_nations_2030_forecast/historic_web_usage.xlsx")
historic_web_usage$week_commencing<- ymd(historic_web_usage$week_commencing)

historic_web_usage<- historic_web_usage %>%  
  gather(key = nation, value  = visitors, 2:7 )

historic_web_usage %>% head()

## get an extra column where each date is just first of the month to label axis
library(zoo)
historic_web_usage$year_month <- as.Date(as.yearmon(historic_web_usage $week_commencing))
historic_web_usage $quarter<-quarter(historic_web_usage $week_commencing)
historic_web_usage $year<-year(historic_web_usage $week_commencing)

historic_web_usage  %>%  
  group_by(year, quarter) %>% 
  summarise(quarter_min = min(year_month))

historic_web_usage  %>% head()

### get the covid scale factor
covid_factor<- read.csv("Covid trend data.csv")
covid_factor$week_commencing<-dmy(covid_factor$date)

covid_factor<-
  covid_factor %>% select(week_commencing, x_google_mob_retail) %>% 
  mutate(quarter = quarter(week_commencing),
         year = year(week_commencing)) %>% 
  rename(covid = x_google_mob_retail)


covid_factor %>% head()


forecast_data <-
  historic_web_usage  %>%
  left_join(covid_factor, by = c("week_commencing", "quarter", "year")) %>%
  replace(is.na(.), 0) %>%
  arrange(week_commencing)

forecast_data %>% head()

#### get some set colours to use ###
### get colours 
# #get default colour codes already used and find a lightened version for the predicted value
colours <- hue_pal()(4)

nation_colours <-
  sapply(colours, function(x) {
    colorRampPalette(c(x, "#FFFFFF"))(10)
  }) %>%
  as.data.frame() %>%
  filter(row_number() == 5) %>%
  gather(key = actual.color, value = pred.color) %>%
  cbind(data.frame(nation = c('england', "northern_ireland", "scotland", "wales")))

############### have a look at the data there is ##############

## get lines for new year
new_year<- forecast_data%>% group_by(year) %>% summarise(new_year = min(week_commencing))
x_axis_dates<-forecast_data%>% group_by(year,quarter) %>% summarise(year_quarter = min(week_commencing))
x_axis_dates

forecast_data %>% head()
nation_x<-"Scotland"
###graph
ggplot(data = forecast_data %>% 
         filter(nation %in% c('england', 'scotland', 'wales', 'northern_ireland')) %>% 
         #filter(nation == tolower(nation_x)) %>% 
         filter(visitors>0) , 
       aes(x = week_commencing, y = visitors, colour = nation))+
  geom_point()+
  geom_smooth(method = "lm", colour = "black",  linetype = "dashed")+
  scale_y_continuous(
    label = comma,
    limits = ~ c(0, max(.x) * 1.1),
    n.breaks = 10
  )+
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = .1, color = "grey") ,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title=element_blank()#,
    #legend.position="none"
  ) +
  labs(title = paste0("Visitors to BBC ", "local News on web")) +
  geom_vline(
    xintercept = new_year$new_year,
    linetype = "dashed",
    color = "grey"
  ) +
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = new_year$new_year,
    sec.axis = sec_axis(
      name = NULL,
      trans = ~ .,
      labels = function(x)
        format(as.yearqtr(x), "%Y")
    )
  ) + facet_wrap( ~ nation, scales = "free_y")




#############  make a linear model ########
historic_web_usage  %>% head()
forecast_data %>% mutate(week = week(week_commencing)) %>% head()

make_lm <- function(raw_data, forecast_measure, data_source_name, colour_scheme) {
  
  ## get lines for new year
  new_year<- raw_data%>% group_by(year) %>% summarise(new_year = min(week_commencing))
  x_axis_dates<-raw_data%>% group_by(year,quarter) %>% summarise(year_quarter = min(week_commencing))
  x_axis_dates
  
  raw_data<-raw_data %>%
    mutate(week = week(week_commencing)) %>% 
    mutate(log_measure = log(!!sym(forecast_measure)))
  
  model <- lm(data = raw_data,
              formula = log_measure ~ year + week + covid)
  
  #create the sequence of Date objects
  future_dates <-
    data.frame(week_commencing = seq(
      as.Date(forecast_data$week_commencing %>% max() %>% ymd()+7),
      by = "week",
      length.out = 52*8
    )) %>%
    mutate(week = week(week_commencing),
      quarter = quarter(week_commencing),
           year = year(week_commencing)
           ) %>%
    filter(year < 2031) %>%
    mutate(covid = 0)


  forecast <<- predict(model, future_dates)


actual_forecast <<-
  future_dates %>%
  cbind(forecast) %>%
  mutate({{forecast_measure}} := round(exp(forecast),0)) %>%
  select(-forecast) %>%
  mutate(actual_pred = 'pred') %>%
    rbind(
      raw_data %>%
        select(week_commencing, week,quarter, year, covid, !!sym(forecast_measure)) %>%
        mutate({{forecast_measure}} := round(!!sym(forecast_measure),0)) %>%
        mutate(actual_pred = 'actual')
    ) %>%
    arrange(week_commencing) %>%
    mutate(nation = raw_data$nation %>% unique())

  print(actual_forecast %>% head())
  
  
  perc_change <<-
    actual_forecast %>%
    group_by(year) %>%
    summarise({{forecast_measure}} := mean(!!sym(forecast_measure))) %>%
    mutate(perc_change = round(100 * (
      !!sym(forecast_measure) / first(!!sym(forecast_measure)) - 1
    ), 2)) %>%
    mutate(perc_change = formattable(perc_change, digits = 0, format = 'f')) %>%
    left_join(actual_forecast %>% group_by(year) %>% summarise(week_commencing = min(week_commencing)),
              by = 'year')


  print(perc_change)


  ggplot(data = actual_forecast, aes(x = week_commencing,
                                     y = !!sym(forecast_measure),
                                     colour = actual_pred)) +
    {if(grepl("BBC",data_source_name)) geom_point(shape = 16)
      else geom_point(shape = 17)}+
    #geom_point(shape = 16) +
    scale_y_continuous(
      label = label_comma(accuracy = .1),
      limits = ~ c(0, max(.x) * 1.1),
      n.breaks = 10
    ) +
    scale_x_date(
      labels = date_format("%Y-%m-%d"),
      breaks = new_year$new_year,
      sec.axis = sec_axis(
        name = NULL,
        trans = ~ .,
        labels = function(x)
          format(as.yearqtr(x), "%Y")
      )
    )  +
    facet_wrap(~ nation, scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid.major.y = element_line(size = .1, color = "grey") ,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none"
    ) +
  labs(title = paste0("Forecast for ",data_source_name)) +
  xlab("year")+
  geom_vline(
    xintercept = actual_forecast$year %>% unique(),
    linetype = "dashed",
    color = "grey"
  ) +
  scale_color_manual(values = c(colour_scheme$actual, colour_scheme$pred))
  geom_text(
    data = perc_change %>% filter(year %in% c(2019,2023, 2027,2030)) ,
    mapping = aes(
      #Inf,
      label = paste0(round(perc_change,0),"%")
    ),
    #nudge_y = 0.1,
    #hjust   = 0.1,
    vjust   = 1.0,
    colour = "black"
  )

}


### England
make_lm(
  raw_data = forecast_data %>% filter(nation == 'england') %>% filter(visitors >0),
  forecast_measure = "visitors",
  data_source_name = "BBC Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='England')
)


