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
###### functions ######
### round up/down to nearest value for the axes limits
round_any <- function(x, accuracy, func){func(x/ accuracy) * accuracy}


### locally
driver <-JDBC("com.amazon.redshift.jdbc41.Driver","~/.redshiftTools/redshift-driver.jar",identifier.quote = "`")
my_aws_creds <-read.csv("~/Documents/Projects/DS/redshift_creds.csv",header = TRUE,stringsAsFactors = FALSE)
url <-paste0("jdbc:redshift://localhost:5439/redshiftdb?user=",my_aws_creds$user,"&password=",my_aws_creds$password)
conn <- dbConnect(driver, url)
dbGetQuery(conn,"select distinct brand_title, series_title from prez.scv_vmb ORDER BY RANDOM() limit 10;")

sql<- "SELECT * FROM central_insights_sandbox.vb_news_regions_historic_users;"
nations_traffic<-dbGetQuery(conn, sql)
nations_traffic$week_commencing<- ymd(nations_traffic$week_commencing)
nations_traffic[,3:7] <- sapply(nations_traffic[,3:7],as.numeric)

nations_traffic$week_commencing %>% head()

## get an extra column where each date is just first of the month to label axis
library(zoo)
nations_traffic$year_month <- as.Date(as.yearmon(nations_traffic$week_commencing))
nations_traffic$quarter<-quarter(nations_traffic$week_commencing)
nations_traffic$year<-year(nations_traffic$week_commencing)

nations_traffic %>%  
  group_by(year, quarter) %>% 
  summarise(quarter_min = min(year_month))
#### Make time series graph
nations_traffic %>% head()

temp <- nations_traffic %>%
  group_by(year, quarter) %>%
  summarise(quarter_min = min(year_month)) %>%
  ungroup() %>%
  select(quarter_min) %>%
  rbind(nations_traffic %>% summarise(quarter_min = max(year_month)))%>% as.list()

x_axis_dates <- temp$quarter_min 
x_axis_dates %>% length()
rm(temp)

ggplot(data= nations_traffic, aes(x = week_commencing, colour = page_producer) )+
  geom_line(aes(y = visitors_raw))+
  #geom_line(aes(y = mean),linetype="dotted")+
  ylab("Visitors")+
  xlab("Date")+
  labs(title = "Weekly visitor BBC News Nations pages (w/c 2019-03-04 to 2022-08-29)") +
  scale_y_continuous(label = comma,
                     n.breaks = 10) +
  scale_x_date(labels = date_format("%Y-%m-%d"),
               breaks = x_axis_dates)+
  theme(axis.text.x = element_text(angle = 90))+
  # geom_vline(xintercept = ymd(labels$week_commencing), linetype="dashed",
  #            color = "black")+
  geom_text_repel( data = labels,
             mapping = aes(x = week_commencing, y = visitors_raw, #Inf, 
                           label = event),
             hjust   = -0.1,
             vjust   = 1.2,
             colour = "black")


labels <-
  data.frame(week_commencing = ymd(c('2020-03-16', '2020-03-23', '2020-08-10', '2021-01-04', '2021-03-08', 
                      '2020-10-19', '2021-08-09', '2022-08-01')),
             event = c("UK schools close", "Lockdown 1", "Scottish exam results","Lockdown 3", "Sarah Everard murder",
                       "Welsh national lockdown", "A Level results", "Euros win"),
             page_producer = c("England","England", "Scotland", "England", "England",
                        "Wales","England", "England" )
             )%>%  left_join(nations_traffic[,1:3], by = c("week_commencing", "page_producer") )
labels
labels %>%  left_join(nations_traffic[,1:3], by = c("week_commencing", "page_producer") )




