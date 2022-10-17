library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(zoo)
library(formattable)
theme_set(theme_classic())


################ read in covid data set ##############

covid_factor<- read.csv("Covid trend data.csv")
covid_factor$week_commencing<-dmy(covid_factor$date)

covid_factor<-
  covid_factor %>% select(week_commencing, x_google_mob_retail) %>% 
  mutate(quarter = quarter(week_commencing),
         year = year(week_commencing)) %>% 
  select(-week_commencing) %>% 
  group_by(year, quarter) %>% 
  mutate(covid = mean(x_google_mob_retail)) %>% 
  select(-x_google_mob_retail) %>% 
  mutate(year_quarter = paste0(year, "_q",quarter)) %>% 
  unique()

covid_factor %>% head()
covid_factor$year_quarter %>% unique()


############ read in all sheets of the RAJAR data into a list of DF ########
path <- "../nations_radio_trends.xlsx"
radio_data <-
  path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)
names(radio_data)<- tolower(gsub(' ','_', excel_sheets(path)))

# Bring the dataframes to the global environment
#list2env(radio_data ,.GlobalEnv)

# df<-eng_3mw
# df_name<-"eng_3mw"

get_messy_data<- function(df){
  bbc_data <- df[4:7, 2:ncol(df)]
  bbc_data[1, 1] <- "measure"
  colnames(bbc_data) <- bbc_data[1, ]
  bbc_data <- bbc_data[-1, ]
  bbc_data
  
  
  other_data<- rbind( df[4, 2:ncol(df)],df[8:10, 2:ncol(df)])
  other_data[1, 1] <- "measure"
  colnames(other_data) <-other_data[1, ]
  other_data <- other_data[-1, ]
  other_data
  
  raw_data<- list(bbc_data, other_data)
  names(raw_data)<- c("bbc_data", "other_data")
  
  return(raw_data)
}

clean_data <- function(df, df_name) {

  ## create a df with the period and each measure as a column
  clean_df_name<- df_name
  
  assign(clean_df_name, 
         data.frame(period = colnames(df[, 2:length(colnames(df))])) %>% 
           mutate(period_temp = period) %>% 
           separate(period_temp, into = c("quarter", "year"), sep = " ") %>% 
           mutate(quarter = as.numeric(gsub('Q','',quarter))) %>% 
           mutate(year= as.numeric(year))%>% 
           mutate(year_quarter = paste0(year, "_q",quarter)) %>%
           full_join(covid_factor, by = c("quarter", "year", "year_quarter")) %>% 
           replace(is.na(.),0) %>% 
           arrange(year_quarter) %>% 
           mutate(source = df_name) %>% 
           filter(year <2020)
         ,
         envir = .GlobalEnv
         ) ## create a df with that name and the periods as a column


  for (row in 1:nrow(df)) {
    measure_name <- tolower(gsub("'", "", gsub(' ', '_', df[row, 1]))) ## name the measure
    measure_name<- gsub("%",'_perc', measure_name)
    measure_name <- gsub("[^[:alnum:] ]", "", measure_name)
    
    ## get a df with that one measure formatted right
    measure <-df[row, 2:ncol(df)] %>% gather(key = "period", value = {{measure_name}})
    measure[[2]]<-as.numeric(measure[[2]])
    
    ## join the measures together into one df
    assign({{clean_df_name}}, 
           get({{clean_df_name}}) %>% 
              left_join(measure, by = "period")
           ,envir = .GlobalEnv)
    }

  return(get({{clean_df_name}}))
}


### clean the dfs that are needed
for(x in 1:length(radio_data)){
  ## if it's a nation with 6 or 3 months (not uk)
  if( (grepl("3", names(radio_data)[x] ) | grepl("6", names(radio_data)[x] ) ) & !grepl("uk", names(radio_data)[x])){
    print(names(radio_data)[x])
    
    ##get the two sets of data from the raw rajar sheet
    data_sets<<-get_messy_data(df = radio_data[[x]])
    
    ## clean the data
    clean_data(df = data_sets[[1]],
               df_name = paste0(names(radio_data)[x],"_bbc")
               ) 
    clean_data(df = data_sets[[2]],
               df_name = paste0(names(radio_data)[x],"_commercial")
    ) 
  }


}

england_3mw_bbc %>% head()


################ have a look at the data there is ##############
## get all the dfs as one for graph
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) # get all df
dfs<- dfs[grepl('mw',dfs) ] ## only keep the ones i need

all_data<-do.call(rbind, dfs) # merge into one
row.names(all_data) <- NULL

all_data<-all_data %>% mutate(nation = sub('_.*','' , source )) 
all_data %>% head()

## get lines for new year
new_year<- all_data %>% filter(quarter == 1) %>% select(year_quarter) %>% unique()

###graph
ggplot(data = all_data, aes(x = year_quarter, y = reach000s, colour = source))+
  geom_point()+
  scale_y_continuous(
    label = comma,
    limits = ~ c(0, max(.x) * 1.1),
    n.breaks = 10
  )+
  facet_wrap( ~ nation, scales = "free_y")+
  theme(
    axis.text.x = element_text(angle = 90),
    panel.grid.major.y = element_line(size = .1, color = "grey") ,
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.title=element_blank()#,
    #legend.position="none"
  ) +
  labs(title = "RAJAR local radio listeners") +
  geom_vline(
    xintercept = new_year$year_quarter,
    linetype = "dashed",
    color = "grey"
  ) 

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
  cbind(data.frame(nation = c('england', "ulster", "scotland", "wales")))



#############  make a linear model ########
england_3mw_bbc %>% head()

make_lm <- function(raw_data, forecast_measure, data_source_name, colour_scheme) {

  raw_data<-raw_data %>% mutate(log_measure = log(!!sym(forecast_measure)))
  
  model <- lm(data = raw_data,
              formula = log_measure ~ year + poly(quarter, 3) + covid)
  
  #create the sequence of Date objects
  future_dates <-
    data.frame(qrt_start = seq(
      as.Date("2020-01-01"),
      by = "quarter",
      length.out = 50
    )) %>%
    mutate(quarter = quarter(qrt_start),
           year = year(qrt_start)) %>%
    mutate(year_quarter = paste0(year, "_q", quarter)) %>%
    filter(year < 2031) %>%
    mutate(covid = 0) %>%
    select(-qrt_start)
  
  
  forecast <- predict(model, future_dates)


  
  actual_forecast <<-
    future_dates %>%
    cbind(forecast) %>%
    mutate({{forecast_measure}} := round(exp(forecast),0)) %>%
    select(-forecast) %>%
    mutate(actual_pred = 'pred') %>%
    rbind(
      raw_data %>%
        select(quarter, year, year_quarter, covid, !!sym(forecast_measure)) %>%
        mutate({{forecast_measure}} := round(!!sym(forecast_measure),0)) %>% 
        mutate(actual_pred = 'actual')
    ) %>%
    arrange(year_quarter) %>%
    mutate(source = raw_data$source %>% unique()) %>%
    mutate(nation = sub('_.*', '' , source)) %>% 
    mutate(test = round(!!sym(forecast_measure),0) )
  
  
  perc_change <<-
    actual_forecast %>%
    group_by(year) %>%
    summarise({{forecast_measure}} := mean(!!sym(forecast_measure))) %>%
    mutate(perc_change = round(100 * (!!sym(forecast_measure) / first(!!sym(forecast_measure)) - 1), 2)) %>% 
    mutate(perc_change = formattable(perc_change, digits = 1, format = 'f')) %>% 
    left_join(actual_forecast %>%  group_by(year) %>% summarise(year_quarter = min(year_quarter)), by = "year")
  
  print(perc_change %>% select(-year_quarter) )


  ggplot(data = actual_forecast, aes(x = year_quarter,
                                      y = !!sym(forecast_measure),
                                      colour = actual_pred)) +
    {if(grepl("BBC",data_source_name)) geom_point(shape = 16)
      else geom_point(shape = 17)}+
    #geom_point(shape = 16) +
    scale_y_continuous(
      label = scales::number_format(accuracy = 1),
      limits = ~ c(0, max(.x) * 1.1),
      n.breaks = 10
    ) +
    scale_x_discrete(label = actual_forecast$year %>% unique(),
                     breaks = actual_forecast$year_quarter[actual_forecast$quarter ==
                                                                      1]) +
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
    xlab("year") +
    geom_vline(
      xintercept = actual_forecast$year_quarter[actual_forecast$quarter == 1],
      linetype = "dashed",
      color = "grey"
    ) +
    scale_color_manual(values = c(colour_scheme$actual, colour_scheme$pred))+
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
  raw_data = england_3mw_bbc,
  forecast_measure = "reach000s",
  data_source_name = "BBC Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='england')
)
make_lm(
  raw_data = england_3mw_commercial,
  forecast_measure = "reach000s",
  data_source_name = "Commercial Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='england')
  
)

#### Scotland
make_lm(
  raw_data = scotland_6mw_bbc,
  forecast_measure = "reach000s",
  data_source_name = "BBC Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='scotland')
)
make_lm(
  raw_data = scotland_6mw_commercial,
  forecast_measure = "reach000s",
  data_source_name = "Commercial Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='scotland')
  
)

#### Wales
make_lm(
  raw_data = wales_6mw_bbc,
  forecast_measure = "reach000s",
  data_source_name = "BBC Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='wales')
)
make_lm(
  raw_data = wales_6mw_commercial,
  forecast_measure = "reach000s",
  data_source_name = "Commercial Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='wales')
  
)

#### Ulster
make_lm(
  raw_data = ulster_6mw_bbc,
  forecast_measure = "reach000s",
  data_source_name = "BBC Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='ulster')
)
make_lm(
  raw_data = ulster_6mw_commercial,
  forecast_measure = "reach000s",
  data_source_name = "Commercial Local Radio",
  colour_scheme = nation_colours %>% filter(nation =='ulster')
  
)



