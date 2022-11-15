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
library(zoo)


theme_set(theme_classic())
options(scipen = 999)
###### functions ######
### round up/down to nearest value for the axes limits
round_any <- function(x, accuracy, func){func(x/ accuracy) * accuracy}

#download.file('http://s3.amazonaws.com/redshift-downloads/drivers/RedshiftJDBC41-1.1.9.1009.jar','~/.redshiftTools/redshift-driver.jar')

### get the covid scale factor
covid_factor<- read.csv("~/Documents/Projects/DS/vb_nations_2030_forecast/data/Covid trend data.csv")
covid_factor$week_commencing<-dmy(covid_factor$date)

covid_factor<-
  covid_factor %>% select(week_commencing, x_google_mob_retail) %>% 
  mutate(quarter = quarter(week_commencing),
         year = year(week_commencing)) %>% 
  rename(covid = x_google_mob_retail)


covid_factor %>% head()

################### data set one ####################
## get news regions data historic from excel sheet that came from Lucy Handscome in the News team
historic_web_usage <- read_excel("~/Documents/Projects/DS/vb_nations_2030_forecast/data/historic_web_usage.xlsx")
historic_web_usage$week_commencing<- ymd(historic_web_usage$week_commencing)
historic_web_usage %>% head() ## lots of the first rows are NA

historic_web_usage<- historic_web_usage %>%  
  gather(key = nation, value  = visitors, 2:7 )

historic_web_usage %>% head()

## get an extra column where each date is just first of the month to label axis
library(zoo)
historic_web_usage$year_month <- as.Date(as.yearmon(historic_web_usage $week_commencing))
historic_web_usage$quarter<-quarter(historic_web_usage $week_commencing)
historic_web_usage$year<-year(historic_web_usage $week_commencing)
historic_web_usage<- historic_web_usage %>% filter(!nation %in% c('combined','bbc_news_overall')  ) 

historic_web_usage<- historic_web_usage %>% filter(year >=2019)
historic_web_usage  %>%  
  group_by(year, quarter) %>% 
  summarise(quarter_min = min(year_month))

historic_web_usage  %>% head()

historic_web_usage$nation %>% unique()

### join the visitor data to the covid factor
forecast_data <-
  historic_web_usage  %>%
  left_join(covid_factor, by = c("week_commencing", "quarter", "year")) %>%
  replace(is.na(.), 0) %>%
  arrange(week_commencing)

forecast_data %>% head()

###################### data set from Rebecca - this is the data that was ACTUALY USED ##########################

old_methodology <- read_excel("~/Documents/Projects/DS/vb_nations_2030_forecast/data/Nations News Online.xlsx",
                                 sheet = '(Old methodology)(clean)')
old_methodology$week_commencing<-ymd(old_methodology$week_commencing)
old_methodology<-old_methodology %>% gather(key = 'nation', value = 'visitors', 3:6)%>% mutate(method = "old") %>% select(-week)
old_methodology

new_methodology<-read_excel("~/Documents/Projects/DS/vb_nations_2030_forecast/data/Nations News Online.xlsx",
           sheet = '1. Nations News Online(clean)')

new_methodology$nation<-tolower(gsub("NATIONS_",'',new_methodology$site))
new_methodology$week_commencing<- as.Date(paste(new_methodology$year, new_methodology$week, 1, sep="-"), "%Y-%U-%u")
new_methodology$week_commencing<-ymd(new_methodology$week_commencing)
new_methodology<- new_methodology %>% select(week_commencing,nation, visitors) %>%  mutate(method = "new")
new_methodology

forecast_data <-
  old_methodology  %>%
  rbind(new_methodology) %>% 
  mutate(year = year(week_commencing), quarter = quarter(week_commencing)) %>% 
  left_join(covid_factor, by = c("week_commencing", "quarter", "year")) %>%
  replace(is.na(.), 0) %>%
  arrange(week_commencing)
forecast_data %>% head()


################################################

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
  cbind(data.frame(nation = c('england','northern_ireland','scotland','wales'  )))

############### have a look at the data there is ##############

## get lines for new year
new_year<- forecast_data%>% group_by(year) %>% summarise(new_year = min(week_commencing))
x_axis_dates<-forecast_data%>% group_by(year,quarter) %>% summarise(year_quarter = min(week_commencing))
x_axis_dates 

new_year<-
new_year %>% rbind(
  data.frame(year_quarter = seq(
    as.Date(forecast_data$week_commencing %>% max() %>% ymd() + 7),
    by = "week",
    length.out = 52 * 9
  )) %>%
    mutate(year = year(year_quarter)) %>%
    filter(year <= 2031 & year >2022) %>%
    group_by(year) %>%
    summarise(new_year = min(year_quarter)) %>%
    select(year, new_year)
)

x_axis_dates 

forecast_data %>% head()

###graph
ggplot(data = forecast_data %>% 
         filter(nation %in% c('england', 'scotland', 'wales', 'northern_ireland')) %>% 
         #filter(nation == tolower(nation_x)) %>% 
         filter(visitors>0) , 
       aes(x = week_commencing, y = visitors, colour = nation))+
  geom_point()+
  geom_smooth(method = "lm", colour = "black",  linetype = "dashed", fullrange=TRUE)+
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
  ) + facet_wrap( ~ nation, scales = "free_y")+
  scale_x_date(
    #limits = c(x_axis_dates$year_quarter %>% min(), x_axis_dates$year_quarter %>% max() ),
    limits = c(new_year$new_year %>% min(), new_year$new_year %>% max() ),
    labels = date_format("%Y-%m-%d"),
    breaks = new_year$new_year,
    sec.axis = sec_axis(
      name = NULL,
      trans = ~ .,
      labels = function(x) format(as.yearqtr(x), "%Y")
    )
  )



## compare to Rioch's path to target from last year. 
## find the paths to target thing - check it's not vastly different

######## compare values since ati march 2019 for all platforms to that of just desktop devices


historic_desktop_usage <- read_csv("~/Documents/Projects/DS/vb_nations_2030_forecast/data/news_historic_users_desktop.csv")
historic_desktop_usage$week_commencing<- dmy(historic_desktop_usage$week_commencing)

historic_desktop_usage %>% head()

## get an extra column where each date is just first of the month to label axis
library(zoo)
historic_desktop_usage$year_month <- as.Date(as.yearmon(historic_desktop_usage$week_commencing))
historic_desktop_usage$quarter<-quarter(historic_desktop_usage$week_commencing)
historic_desktop_usage$year<-year(historic_desktop_usage$week_commencing)
historic_desktop_usage$nation<-tolower(historic_desktop_usage$nation)

historic_desktop_usage<- 
  historic_desktop_usage %>%
  left_join(covid_factor) %>% 
  replace(is.na(.), 0)

historic_desktop_usage  %>% 
  group_by(year, quarter) %>% 
  summarise(quarter_min = min(year_month))


historic_desktop_usage  %>% head()


## compare
comp<-
historic_desktop_usage %>% rename(desktop_visitors = visitors) %>% 
  left_join(
  historic_web_usage %>% rename(all_visitors = visitors), 
  by  = c("week_commencing", 'nation', 'year_month', 'year', 'quarter')
) 
historic_desktop_usage$nation %>% unique()
new_year<- historic_desktop_usage%>% group_by(year) %>% summarise(new_year = min(week_commencing))



###### graph for new ATI data 
ggplot(data = historic_desktop_usage %>% 
         filter(nation %in% c('england', 'scotland', 'wales', 'northern ireland')) %>% 
         #filter(nation == tolower(nation_x)) %>% 
         filter(visitors>0) , 
       aes(x = week_commencing, y = visitors, colour = nation))+
  geom_point()+
  geom_smooth(method = "lm", colour = "black",  linetype = "dashed", fullrange=TRUE)+
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
  labs(title = paste0("Visitors to BBC ", "local News on web - Desktop")) +
  geom_vline(
    xintercept = new_year$new_year,
    linetype = "dashed",
    color = "grey"
  ) +
  scale_x_date(
    limits = c(new_year$new_year %>% min(), new_year$new_year %>% max() ),
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

forecast_data %>% mutate(week = week(week_commencing)) %>% head()

make_lm <- function(raw_data, forecast_measure, data_source_name, colour_scheme) {
  nation = sub('_.*', '' , raw_data$nation %>% unique())
  
  raw_data<-raw_data %>%
    mutate(week = week(week_commencing)) %>% 
    mutate(log_measure = log(!!sym(forecast_measure)))
  
  model <- lm(data = raw_data,
              formula = log_measure ~ year + quarter + covid)
  
  #create the sequence of Date objects
  future_dates <-
    data.frame(week_commencing = seq(
      as.Date(forecast_data$week_commencing %>% max() %>% ymd()+7),
      by = "week",
      length.out = 52*9
    )) %>%
    mutate(week = week(week_commencing),
      quarter = quarter(week_commencing),
           year = year(week_commencing)
           ) %>%
    filter(year < 2031) %>%
    mutate(covid = 0)


  forecast <- predict(model, future_dates)


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


  # ## the data starts at different times so get first full year
  # first_full_year<- actual_forecast %>% group_by(year) %>% count() %>% filter(n>=52) %>% head(n=1) %>% select(year) %>% as.numeric()
  # 
  # print(first_full_year)
  # qrt_avg <<- actual_forecast %>%
  #   filter(year == first_full_year) %>%
  #   summarise(quarter_avg = mean(!!sym(forecast_measure))) %>% as.numeric()
  # 
  ## for the new Rebecca data use 2019 although it's not a full year
  qrt_avg <<- actual_forecast %>%
    filter(year == 2019) %>%
    summarise(quarter_avg = mean(!!sym(forecast_measure))) %>% as.numeric()
  
  qrt_avg_2021<-actual_forecast %>%
    filter(year == 2021) %>%
    summarise(quarter_avg = mean(!!sym(forecast_measure))) %>% as.numeric()
  
  perc_change <<-
    actual_forecast %>%
    group_by(year) %>%
    summarise({{forecast_measure}} := mean(!!sym(forecast_measure))) %>%
    mutate(perc_change = round(100 * (!!sym(forecast_measure) / qrt_avg - 1), 2),
           perc_change_2021 = round(100 * (!!sym(forecast_measure) / qrt_avg_2021 - 1), 2),
    
    ) %>%
    mutate(perc_change = formattable(perc_change, digits = 0, format = 'f'),
           perc_change_2021 = formattable(perc_change_2021, digits = 0, format = 'f')
           ) %>%
    left_join(actual_forecast %>% group_by(year) %>% summarise(week_commencing = min(week_commencing)),
              by = 'year')


  print(perc_change)
  
  ## get lines for new year
  # new_year<- raw_data%>% group_by(year) %>% summarise(new_year = min(week_commencing))
  # x_axis_dates<-raw_data%>% group_by(year,quarter) %>% summarise(year_quarter = min(week_commencing))
  # x_axis_dates
  
  year_starts <<- 
    actual_forecast %>% 
    group_by(year) %>%
    filter(week_commencing == min(week_commencing)) %>%
    ungroup() %>%
    select(year, week_commencing)
  
  
  forecast_graph<-  
  ggplot(data = actual_forecast, aes(x = week_commencing,
                                     y = !!sym(forecast_measure),
                                     colour = actual_pred)) +
    {
      if (grepl("BBC", data_source_name))
        geom_point(shape = 16)
      else
        geom_point(shape = 17)
    } +
    #geom_point(shape = 16) +
    scale_y_continuous(
      label = comma,
      limits = ~ c(0, max(.x) * 1.1),
      n.breaks = 10
    ) +
    scale_x_date(label = year_starts$year,
                     breaks = year_starts$week_commencing
    )  +
    facet_wrap( ~ nation, scales = "free_y") +
    theme(
      axis.text.x = element_text(angle = 90),
      panel.grid.major.y = element_line(size = .1, color = "grey") ,
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none"
    ) +
    labs(title = paste0("Forecast for ", data_source_name)) +
    xlab("year") +
    geom_vline(
      xintercept = actual_forecast$year %>% unique(),
      linetype = "dashed",
      color = "grey"
    ) +
    scale_color_manual(values = c(colour_scheme$actual, colour_scheme$pred))+
    geom_point( data = perc_change %>% filter(year %in% c(2019, 2023, 2027, 2030)) ,
                mapping = aes( x = week_commencing, 
                               y = !!sym(forecast_measure)),
                colour = "dark grey"
                )+
    geom_text(
      data = perc_change %>% filter(year %in% c(2019, 2023, 2027, 2030)) ,
      mapping = aes(  label = paste0(round(perc_change, 0), "%")),
      #hjust   = 0.1,
      vjust   = 1.0,
      colour = "black"
    )



#final_data
final_data<<-
  actual_forecast %>%
  select(year, quarter, nation, actual_pred,!!sym(forecast_measure)) %>%
  group_by(year, quarter) %>% 
  summarise({{forecast_measure}} := mean(!!sym(forecast_measure))) %>% 
  mutate(perc_change = signif(round(100 * ( (!!sym(forecast_measure) / qrt_avg)-1 ), 0), 2)) %>% 
  mutate({{forecast_measure}} := signif(!!sym(forecast_measure),3) )

write.csv( final_data ,
           file = paste0("./forecasts/web/rebecca_data/",
                         nation,'_quarter_',
                         sub('.*_', '' , raw_data$nation %>% unique()),"_",
                         forecast_measure,".csv" 
           ),
           row.names = FALSE
)

write.csv(perc_change %>% 
            mutate({{forecast_measure}} := signif(!!sym(forecast_measure),3) ) %>% 
            select(-week_commencing)
          ,
          file = paste0("./forecasts/web/rebecca_data/",
                        nation,'_annual_',
                        sub('.*_', '' , raw_data$nation %>% unique()),"_",
                        forecast_measure,".csv" 
          ),
          row.names = FALSE
)

print(forecast_graph)
## save plots
filename<-paste0("./forecasts/web/graphs/rebecca_data/",
                 nation,'_annual_',
                 #sub('.*_', '' , raw_data$nation %>% unique()),"_",
                 forecast_measure,".jpg" )
dev.copy(jpeg,filename=filename);
dev.off ();

print(forecast_graph)


}


### England
nation_x<- 'Wales'
make_lm(
  raw_data = forecast_data %>% filter(nation == tolower(gsub(' ','_', nation_x))) %>% filter(visitors >0),
  forecast_measure = "visitors",
  data_source_name = paste0("BBC News ",nation_x),
  colour_scheme = nation_colours %>% filter(nation == gsub(' ','_', tolower(nation_x) ))
)


### England
make_lm(
  raw_data = forecast_data %>% filter(nation == 'england') %>% filter(visitors >0),
  forecast_measure = "visitors",
  data_source_name = "BBC News England",
  colour_scheme = nation_colours %>% filter(nation =='england')
)


### Scotland
make_lm(
  raw_data = forecast_data %>% filter(nation == 'scotland') %>% filter(visitors >0),
  forecast_measure = "visitors",
  data_source_name = "BBC News Scotland",
  colour_scheme = nation_colours %>% filter(nation =='scotland')
)


### Wales
make_lm(
  raw_data = forecast_data %>% filter(nation == 'wales') %>% filter(visitors >0),
  forecast_measure = "visitors",
  data_source_name = "BBC News Wales",
  colour_scheme = nation_colours %>% filter(nation =='wales')
)

### northern ireland

make_lm(
  raw_data = forecast_data %>% filter(nation == 'northern_ireland') %>% filter(visitors >0),
  forecast_measure = "visitors",
  data_source_name = "BBC News Northern Ireland",
  colour_scheme = nation_colours %>% filter(nation =='northern_ireland')
)




### comparison to paths to targets

forecast_data %>% #head() %>% 
  filter(nation == 'england') %>% 
  mutate(month = month(week_commencing)) %>% 
  group_by(year, month) %>% 
  summarise(median_visitors = median(visitors),
            mean_visitors= mean(visitors)
            )



paths_england<-read_excel("~/Documents/Projects/DS/vb_nations_2030_forecast/data/Nations Paths to Targets 2223 20221102.xlsx",
           sheet = 'News 2')[,1:4]
colnames(paths_england)<-gsub(' ','_', tolower(paths_england[1,]))
paths_england<-paths_england %>% rename(month = quarter)  %>% mutate_if(is.character,as.numeric)

paths_england = paths_england[-1,]
paths_england<- paths_england %>% filter(!is.na(actual) | !is.na(path_to_target) )

paths_england %>% head()



comparison = forecast_data %>% #head() %>%
  filter(nation == 'england') %>%
  mutate(month = month(week_commencing)) %>%
  group_by(year, month) %>%
  summarise(median_visitors = median(visitors),
            mean_visitors = mean(visitors)) %>%
  left_join(paths_england,  by = c('year', 'month')) %>% 
  group_by(year, month) %>% 
  mutate(month = sprintf("%02d", month)) %>% 
  gather(key = measure, value = visitors, 3:6 ) %>% 
  mutate(dt = ymd(paste0(year,month, '01')),
         quarter = quarter(dt)
         )

comparison <- comparison %>%
  left_join(
    comparison %>% group_by(year, quarter) %>% summarise(qtr_start = min(dt)),
    by = c('year', 'quarter')
  ) %>% 
  left_join(comparison %>% group_by(year) %>% mutate(year_min = min(dt)) %>% select(year, year_min) %>% unique(),
            by ='year' )

comparison %>% head()

ggplot(data = comparison, aes(x = dt, y = visitors, colour = measure))+
  geom_point()+
  scale_y_continuous(
    label = comma,
    limits = ~ c(0, max(.x) * 1.1),
    n.breaks = 10
  ) +
  scale_x_date(
    limits = c(comparison$dt %>% min(), comparison$dt  %>% max() ),
    labels = date_format("%Y-%m-%d"),
    breaks = comparison$qtr_start,
    sec.axis = sec_axis(
      name = NULL,
      trans = ~ .,
      labels = year)
    )+
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = .1, color = "grey") ,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
   # legend.title = element_blank(),
    #legend.position = "none"
  ) +
  geom_vline(
    xintercept = comparison$year_min %>% unique(),
    linetype = "dashed",
    color = "grey"
  ) +
  ggtitle("Measure comparison with target set for 2023 (from paths to target) \nEngland")



