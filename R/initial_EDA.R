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
sql<- "SELECT * FROM central_insights_sandbox.vb_news_regions_historic_users;"
nations_traffic<-dbGetQuery(conn, sql)
nations_traffic$week_commencing<- ymd(nations_traffic$week_commencing)
nations_traffic[,3:7] <- sapply(nations_traffic[,3:7],as.numeric)


## get an extra column where each date is just first of the month to label axis
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

## label key events
labels <-
  data.frame(
    week_commencing = ymd(
      c(
        '2020-03-16',
        '2020-03-23',
        '2020-08-10',
        '2021-01-04',
        '2021-03-08',
        '2020-10-19',
        '2021-08-09',
        '2022-08-01',
        '2020-03-09',
        '2022-09-05'
      )
    ),
    event = c(
      "UK schools close",
      "Lockdown 1",
      "Scottish exam results",
      "Lockdown 3",
      "Sarah Everard murder",
      "Welsh national lockdown",
      "A Level results",
      "Euros win",
      "ROI schools close",
      "Queen's death"
    ),
    page_producer = c(
      "England",
      "England",
      "Scotland",
      "England",
      "England",
      "Wales",
      "England",
      "England",
      "Northern Ireland",
      "England"
    )
  ) %>%  left_join(nations_traffic[, 1:3], by = c("week_commencing", "page_producer")) %>% 
  arrange(week_commencing)
labels

## get lines for Christmas/new year
new_year<-ymd(c("2020-01-01","2021-01-01","2022-01-01"))



############## function so as to not repeat code for similar graphs ##########
make_graph <- function(data, y_value, labels, v_line, title, y_lab, split, avg_label) {
data$year<-year(data$week_commencing)
avg_traffic<-
  data %>% #head() %>%
    group_by(page_producer, year) %>%
    mutate(
      median_visitors = median(!!sym(y_value)),
      mean_visitors = mean(!!sym(y_value))
    ) %>%
    select(week_commencing,
           page_producer,
           median_visitors,
           mean_visitors) 

  print(avg_traffic)
  
ggplot(data = data, aes(x = week_commencing, colour = page_producer)) +
    geom_point(aes(y = !!sym(y_value))) +
    {if(is.data.frame(labels))geom_point(data = labels,
               aes(y = !!sym(y_value), x = week_commencing))} +
    ylab(y_lab) +
    xlab("Date") +
    labs(title = title) +
    {if(split == TRUE) 
    scale_y_continuous(label = comma,
                       limits = ~ c(min(.x), max(.x)*1.2),
                       n.breaks = 10)
      else scale_y_continuous(label = comma,
                              limits = ~ c(min(.x), max(.x)*1.1),
                              n.breaks = 20)} +
    geom_vline(xintercept = v_line,
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
    {if(split == TRUE)facet_wrap( ~ page_producer, nrow = 4, scales = "free_y")}+
    { if(is.data.frame(labels))geom_text(
      data = labels ,
      mapping = aes(
        x = week_commencing,
        y = !!sym(y_value),
        #Inf,
        label = event
      ),
      #nudge_y = 0.1,
      #hjust   = 0.1,
      vjust   = -1.0,
      colour = "black"
    )}+
    theme(axis.text.x = element_text(angle = 90),
          panel.grid.major.y = element_line( size=.1, color="grey" ) ,
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()
          )+
  ### add averages line and text
  {if(avg_label ==TRUE)geom_text(
    data = avg_traffic %>% filter(
      week_commencing == min(avg_traffic$week_commencing) |
        week_commencing == max(avg_traffic$week_commencing)
    ) %>%
      mutate(median = signif(median_visitors,2)),
    mapping = aes(
      x = week_commencing,
      y = median,
      #Inf,
      label = comma(median),
      colour = page_producer
    ),
    nudge_y = 0.1,
    #hjust   = 0.1,
    vjust   = 1.5
  )}+
  geom_line(data = avg_traffic,
            aes(x =week_commencing, y = median_visitors, colour = page_producer ),
            linetype = "dashed"
  )


}

##### find averages per year
# nations_traffic %>% #head() %>%
#   group_by(page_producer, year) %>%
#   mutate(
#     median_visitors = median(visitors_raw),
#     mean_visitors = mean(visitors_raw)
#   ) %>%
#   select(week_commencing,
#          page_producer,
#          median_visitors,
#          mean_visitors) %>%
#   filter(
#     week_commencing == min(avg_traffic$week_commencing) |
#       week_commencing == max(avg_traffic$week_commencing)
#   ) %>% 
#   mutate(median = round(median_visitors, -5))

### Plot visitor traffic
make_graph(
  data = nations_traffic,
  y_value = "visitors_raw",
  labels = labels,
  v_line = new_year,
  y_lab = "Number of Visitors",
  title = "Visitors to BBC News Nations pages (w/c 2019-03-04 to 2022-09-05)",
  split = FALSE,
  avg_label = TRUE
)



make_graph(
  data = nations_traffic,
  y_value = "visitors_raw",
  labels = "",
  v_line = new_year,
  y_lab = "Number of Visitors",
  title = "Visitors to BBC News Nations pages (w/c 2019-03-04 to 2022-09-05)",
  split = TRUE,
  avg_label = FALSE
)


##look at the number of Nations pages being visited that week (minimum 10 visits)
num_pages<-dbGetQuery(conn, "SELECT DISTINCT week_commencing, page_producer, pages FROM central_insights_sandbox.vb_news_regions_num_pages ORDER BY 1,2 DESC;")
num_pages %>% head()
num_pages$week_commencing<- ymd(num_pages$week_commencing)
num_pages$pages<- as.numeric(num_pages$pages)
num_pages$year<-year(num_pages$week_commencing)

labels<- labels %>% left_join(num_pages, by = c('week_commencing', 'page_producer'))

make_graph(
  data = num_pages,
  y_value = "pages",
  labels = "",
  v_line = new_year,
  y_lab = "Pages",
  title = "Number of BBC News Nations pages visited (w/c 2019-03-04 to 2022-09-05)",
  split = TRUE,
  avg_label = TRUE
)

###### visitors, given the number of pages######
visitors_per_pages <- nations_traffic %>%
  left_join(num_pages, by = c('week_commencing', 'page_producer'))  %>%
  group_by(week_commencing, page_producer) %>%
  summarise(visitors_per_pages = visitors_raw / pages) %>% ungroup()


labels<- labels %>% left_join(visitors_per_pages, by = c('week_commencing', 'page_producer'))


make_graph(
  data = visitors_per_pages,
  y_value = "visitors_per_pages",
  labels = "",
  v_line = new_year,
  y_lab = "Visitors per Pages",
  title = "Visitors per Pages to BBC News Nations pages (w/c 2019-03-04 to 2022-09-05)",
  split = TRUE,
  avg_label = TRUE
)



###################### visitors to all of BBC News ######################

all_news<- dbGetQuery(conn, "SELECT week_commencing, visitors FROM central_insights_sandbox.vb_news_regions_historic_users_all_news ORDER BY 1 asc;")
all_news %>% head()
all_news$week_commencing<- ymd(all_news$week_commencing)
all_news$visitors<- as.numeric(all_news$visitors)



all_labels <- labels %>% select(week_commencing,event, page_producer) %>% 
rbind(
  data.frame(
    week_commencing  = ymd(
      c(
        '2020-11-02',
        '2022-02-21',
        '2019-12-09',
        '2021-12-13',
        '2022-07-04'
      )
    ),
    event = c(
      'US Election',
      'Russia invades Ukraine',
      "UK Election",
      "Omicron restrictions",
      "Boris resigns"
    ),
    page_producer = c('England', 'England', 'England', 'England', "England")
  )
) %>%
  left_join(all_news, by = "week_commencing") %>% arrange(week_commencing)
all_labels
all_news$page_producer <-"England" ## just add this so i can use the graph function


make_graph(
  data = all_news,
  y_value = "visitors",
  labels = all_labels,
  v_line = new_year,
  y_lab = "Visitors",
  title = "Visitors to all BBC News pages (w/c 2019-03-04 to 2022-09-12)",
  split = FALSE,
  avg_label = TRUE
)


###### N&R visitors per all news visitors
nations_traffic %>% head()
all_news %>% head()

nations_perc<-
all_news %>% 
  mutate(all_visitors = visitors) %>%
  select(-visitors, -page_producer) %>% 
  left_join(nations_traffic %>% select(week_commencing,page_producer, visitors_raw ), 
            by = "week_commencing") %>% 
  mutate(nations_perc = round(100*visitors_raw/all_visitors,1))
nations_perc %>% head()

all_labels_perc <-
  all_labels %>%
  select(week_commencing, event, page_producer)  %>%
  rbind(data.frame(
    week_commencing  = ymd(c('2021-08-09')),
    event = c('Devon shooting' ),
    page_producer = c('England')
  )) %>% 
  left_join(nations_perc, by = c('week_commencing', "page_producer"))

all_labels_perc %>% 
  filter(page_producer !='England' | event %in% c('A Level Results', "Euros win", "Sarah Everard murder", "Devon shooting") ) %>% 
  select(week_commencing, page_producer, event, nations_perc)


make_graph(
  data = nations_perc,
  y_value = "nations_perc",
  labels = all_labels_perc %>% 
    filter(page_producer !='England' | event %in% c('A Level Results', "Euros win", "Sarah Everard murder", "Devon shooting") ) %>% 
    select(week_commencing, page_producer, event, nations_perc),
  v_line = new_year,
  y_lab = "Percentage (%) ",
  title = "Nations visitor numbers as a percentage of all BBC News visitors (w/c 2019-03-04 to 2022-09-05)",
  split = TRUE,
  avg_label = TRUE
)

# -- how many articles are there a day for england/wales etc
# -- are nations promoted more on the homepage?
#   -- compareto all bbc traffic
# -- ask Emily Wales things
## linear trend, then try logistic fits
## raw visitors?
## visitors compare to overall visitors?
## not arima models


  