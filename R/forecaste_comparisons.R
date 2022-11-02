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

forecasts_files<-grep(pattern = "(?=.*csv)(?=.*reach)(?=.*annual)", x = list.files(path = "forecasts/radio"), value = TRUE, perl = TRUE)
forecasts_files
gsub("_.*","", forecasts_files) #nation
gsub("_.*",'',  gsub(".*annual_","", forecasts_files))

nations_forecasts<- do.call(rbind, lapply(forecasts_files, 
                                          function(x) read.csv(file= paste0("forecasts/radio/",x))
                                          ))
nations_forecasts<-
nations_forecasts %>% 
  mutate(actual_pred = gsub('icted','',actual_pred)) %>% 
  mutate(nation_measure = paste0(region, "_",actual_pred))
nations_forecasts$week_commencing <- ymd(as.character(nations_forecasts$week_commencing))

# #get default colour codes already used and find a lightened version for the predicted value
colours <- hue_pal()(4)

nation_colours <-
  sapply(colours, function(x) {
    colorRampPalette(c(x, "#FFFFFF"))(10)
  }) %>%
  as.data.frame() %>%
  filter(row_number() == 5) %>%
  gather(key = actual.color, value = pred.color) %>%
  cbind(data.frame(nation = c('England', "NI", "Scotland", "Wales")))%>% 
  gather(key = measure, value = colour, c(actual.color, pred.color)) %>% 
    mutate(measure = gsub('.color','',measure)) %>% 
    mutate(nation_measure = paste0(nation, "_", measure))
nation_colours



## get lines for new year
new_year<<- nations_forecasts %>% group_by(year) %>% summarise(week_commencing = min(week_commencing))


#### average traffic
avg_traffic <-
  nations_forecasts %>% #head() %>%
  group_by(year, region) %>%
  mutate(median_viewers = median(viewers_milions)) %>%
  select(year,
         week_commencing,
         region,
         median_viewers)



## get dates
x_axis_dates<-nations_forecasts %>% group_by(year) %>% summarise(week_commencing = min(week_commencing))
x_axis_dates


### % decline
perc_change<-
avg_traffic %>% 
  filter(year == 2017 | year == 2030 | year == 2022) %>%
  select(year, region, median_viewers) %>%
  unique() %>% 
  mutate(year = paste0("y_",year)) %>% 
  spread(key = year, value = median_viewers) %>% 
  mutate(perc = paste0(round(100*(y_2030/y_2017 -1),0),"%" ),
         perc_2022 = paste0(round(100*(y_2022/y_2017 -1),0),"%" ),
         ) %>% 
  cbind(avg_traffic %>% 
          ungroup() %>%  
          filter(week_commencing == max(week_commencing)) %>% 
          select(week_commencing,median_viewers)
        ) %>% 
  mutate(nation_measure = paste0(region, "_pred"))
perc_change


######### graph #########

ggplot(data = nations_forecasts, aes(x = week_commencing,  colour = nation_measure)) +
  geom_point(aes(y = viewers_milions)) +
  ylab("Average Weekly Viewers (million)") +
  xlab("Date") +
  labs(title = "Predicted Viewers to BBC Local News") +
  scale_y_continuous(
    label = comma,
    limits = ~ c(min(.x), max(.x) * 1.1),
    n.breaks = 10
  ) +
  scale_colour_manual(breaks = nation_colours$nation_measure,
    values= nation_colours$colour)+
  geom_vline(
    xintercept = new_year$week_commencing,
    linetype = "dashed",
    color = "grey"
  ) +
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = x_axis_dates$week_commencing,
    sec.axis = sec_axis(
      name = NULL,
      breaks = x_axis_dates$week_commencing ,
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
    legend.title=element_blank(),
    legend.position="none"
  ) +
  geom_line(
    data = avg_traffic ,
    aes(x = week_commencing, y = median_viewers),
    linetype = "dashed",
    colour = "black"
  )+
  geom_label(data = perc_change, 
             aes(x = week_commencing, y = median_viewers,label = perc)
             ,colour = "black")

  
  
###### data table
percentage_change<-
avg_traffic %>% 
  select(year, region, median_viewers) %>% 
  unique() %>% left_join(
    avg_traffic %>% 
      filter(year == 2017) %>%
      ungroup() %>% 
      select(region, median_viewers) %>% 
      unique() %>% 
      mutate(median_2017 = median_viewers) %>% 
      select(-median_viewers)
    ,
    by = 'region'
  ) %>% mutate(change_from_2017 = 
                 paste0(round(100*(median_viewers/median_2017 -1),0),"%" ) ) %>% 
  select(-median_2017)
  


############## Plot normalised data to compare ##############

nations_forecasts_scaled<-
nations_forecasts %>% #head() %>%
  group_by(region) %>%
  mutate(scaled_viewers_mil = scale(viewers_milions))

#### average traffic
avg_traffic_scaled <-
  nations_forecasts_scaled%>% #head() %>%
  mutate(scaled_viewers_mil = scale(viewers_milions)) %>% 
  group_by(year, region) %>%
  mutate(median_viewers = median(scaled_viewers_mil)) %>%
  select(year,
         week_commencing,
         region,
         median_viewers)
### % decline
perc_change<-
  avg_traffic_scaled %>% 
  filter(year == 2017 | year == 2030 | year == 2022) %>%
  select(year, region, median_viewers) %>%
  unique() %>% 
  mutate(year = paste0("y_",year)) %>% 
  spread(key = year, value = median_viewers) %>% 
  mutate(perc = paste0(round(100*(y_2030/y_2017 -1),0),"%" ),
         perc_2022 = paste0(round(100*(y_2022/y_2017 -1),0),"%" ),
  ) %>% 
  cbind(avg_traffic_scaled %>% 
          ungroup() %>%  
          filter(week_commencing == max(week_commencing)) %>% 
          select(week_commencing,median_viewers)
  ) %>% 
  mutate(nation_measure = paste0(region, "_pred"))
perc_change

######### graph #########

ggplot(data = nations_forecasts_scaled
       , aes(x = week_commencing,  colour = nation_measure)) +
  geom_point(aes(y = scaled_viewers_mil)) +
  ylab("Average Weekly Viewers (million)") +
  xlab("Date") +
  labs(title = "Predicted Viewers to BBC Local News") +
  scale_y_continuous(
    label = comma,
    limits = ~ c(min(.x), max(.x) * 1.1),
    n.breaks = 10
  ) +
  scale_colour_manual(breaks = nation_colours$nation_measure,
                      values= nation_colours$colour)+
  geom_vline(
    xintercept = new_year$week_commencing,
    linetype = "dashed",
    color = "grey"
  ) +
  scale_x_date(
    labels = date_format("%Y-%m-%d"),
    breaks = x_axis_dates$week_commencing,
    sec.axis = sec_axis(
      name = NULL,
      breaks = x_axis_dates$week_commencing ,
      trans = ~ .,
      labels = date_format("%Y")
    )
  ) +
  #facet_wrap( ~ region, nrow = 4, scales = "free_y") +
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = .1, color = "grey") ,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title=element_blank(),
    legend.position="none"
  ) +
  geom_line(
    data = avg_traffic_scaled,
    aes(x = week_commencing, y = median_viewers),
    linetype = "dashed",
    colour = "black"
  )+
  geom_label(data = perc_change, 
             aes(x = week_commencing, y = median_viewers,label = perc)
             ,colour = "black")








