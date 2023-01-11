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

############ Read in data ############
### covid factor 
covid_factor<- read.csv("~/Documents/Projects/DS/vb_nations_2030_forecast/data/Covid trend data.csv")
covid_factor$week_commencing<-dmy(covid_factor$date)

covid_factor<-
  covid_factor %>% select(week_commencing, x_google_mob_retail) %>% 
  rename(covid = x_google_mob_retail)

covid_factor %>% head()

## read in tv and radio data
radio<-read.csv('~/Documents/Projects/DS/vb_nations_2030_forecast/data/england_rajar_bbc.csv')
tv<-read.csv('~/Documents/Projects/DS/vb_nations_2030_forecast/data/england_tv.csv') %>% mutate(year_quarter = paste0(year,'_q',quarter))
tv$week_commencing<- ymd(tv$week_commencing)

##clean
radio <- radio %>% mutate(region = 'England') %>%
  select(region, year, quarter, year_quarter, reach000s, covid)
radio %>% head()


##clean
tv <-
  tv %>% select(region, year, quarter, year_quarter, week_commencing, viewers) %>%
  left_join(covid_factor, by = 'week_commencing')

tv %>% head()
new_year<-radio %>% filter(quarter ==1) %>% select(year_quarter)

### radio Graph
ggplot(data = radio, 
       aes(x = year_quarter, y = hours000s))+
  geom_point()+
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
    legend.title=element_blank(),
    legend.position="none"
  ) +
  labs(title = "RAJAR local radio listeners") +
  geom_vline(
    xintercept = new_year$year_quarter,
    linetype = "dashed",
    color = "grey"
  ) +
  scale_x_discrete(
    labels = radio$year_quarter[radio$quarter ==1],
    breaks =  radio$year_quarter[radio$quarter ==1],

  ) 

