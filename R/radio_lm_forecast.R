library(ggplot2)
library(tidyverse)
library(readxl)



## read in all sheets into a list of DF
path <- "../nations_radio_trends.xlsx"
radio_data <-
  path %>%
  excel_sheets() %>%
  set_names() %>%
  map(read_excel, path = path)
names(radio_data)<- tolower(gsub(' ','_', excel_sheets(path)))

# Bring the dataframes to the global environment
#list2env(radio_data ,.GlobalEnv)

clean_data <- function(df, df_name) {
  ### pull out the data needed excluding the labels
  temp <- df[4:7, 2:ncol(df)]
  temp[1, 1] <- "measure"
  colnames(temp) <- temp[1, ]
  temp <- temp[-1, ]
  temp
  
  ## create a df with the period and each measure as a column
  #clean_df_name <-paste0(deparse(substitute(df)), "_clean") ## make a nice name
  clean_df_name<- paste0(df_name, "_clean")
  
  assign(clean_df_name, data.frame(period = colnames(temp[, 2:length(colnames(temp))])),
         envir = .GlobalEnv
         ) ## create a df with that name and the periods as a column
  

  for (row in 1:nrow(temp)) {
    measure_name <- tolower(gsub("'", "", gsub(' ', '_', temp[row, 1]))) ## name the measure
    
    ## get a df with that one measure formatted right
    measure <-temp[row, 2:ncol(temp)] %>% gather(key = "period", value = {{measure_name}})
    
    ## join the measures together into one df
    assign({{clean_df_name}}, get({{clean_df_name}}) %>% left_join(measure, by = "period"),envir = .GlobalEnv)
    }

  return(get({{clean_df_name}}))
}


### clean the dfs that are needed
for(x in 1:length(radio_data)){
  ## if it's a nation with 6 or 3 months (not uk)
  if( (grepl("3", names(radio_data)[x] ) | grepl("6", names(radio_data)[x] ) ) & !grepl("uk", names(radio_data)[x])){
    print(names(radio_data)[x])
    
    ## clean the data
    clean_data(df = radio_data[[x]],
               df_name = names(radio_data)[x]
               ) ## clean the data
  }

}

scotland_6mw_clean


