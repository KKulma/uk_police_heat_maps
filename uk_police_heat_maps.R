library(httr)
library(rvest)
library(purrr)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)
library(stringr)


### scraping with rvest() ####

wiki_link <- "https://wiki.openstreetmap.org/wiki/List_of_London_Underground_stations"

wiki_tbl <- wiki_link %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>% 
  #html_name()
  html_table(fill = TRUE) %>% 
  as.data.frame()

str(wiki_tbl)
glimpse(wiki_tbl)
name <- "Enfield"

#### building function that take lat, long & date as arguments ####

get_crime <- function(lat, long, date, name) {
  
  base <- "https://data.police.uk/api/crimes-street/all-crime"
  police_url <- paste0(base, "?", "lat=", lat, "&lng=", long, "&date=", date)  
  get_police <- GET(police_url)
  
  police_data_json <- content(get_police, "text")
  police_data <- fromJSON(police_data_json, flatten = TRUE) %>% 
    mutate(location = name)
  return(police_data)
  
}

wiki_selected <- 
  wiki_tbl %>% 
  select(name = Name,
         lat = Latitude,
         long = Longitude)


glimpse(wiki_selected)
first_wiki <- wiki_selected[1, ]

first_test <- get_crime(first_wiki$lat, first_wiki$long, "2017-12", first_wiki$name)
str(first_test)

set.seed(999)
k <- sample(as.numeric(rownames(wiki_selected)), 20)
wiki_sample <- wiki_selected[k, ]
wiki_sample

### grid approach with names in data frame ####

# 1-month test

triple_test <- pmap(list(lat = wiki_sample$lat,
                         long = wiki_sample$long,
                         name = wiki_sample$name,
                         date = "2017-12"),  get_crime)


glimpse(triple_test)
glimpse(bind_rows(triple_test))

# mutli-loation multi-month test ####
iter_months <- str_sub(seq(ymd('2016-01-01'),ymd('2018-05-20'), by = 'month'), start = 1, end = 7)

#sample_grid <- expand.grid(name = wiki_sample$name, date = iter_months) %>% 
#  inner_join(wiki_sample)

final_df<-data.frame()
for(i in 1:length(iter_months)){
  pre_final_list <- pmap(list(lat = wiki_sample$lat,
                              long = wiki_sample$long,
                              name = wiki_sample$name,
                              date = iter_months[i]),
                         get_crime)
  pre_final_df <- bind_rows(pre_final_list)
  final_df <- bind_rows(final_df, pre_final_df)
  
}


glimpse(final_df)
table(final_df$location, final_df$month)

saveRDS(final_df, "20180719-final-police-df-Jan16-May18")

police_grid <- final_df %>%
  rename(date = month) %>% 
  unique() %>% 
  #mutate(year = year(as.yearmon(date)),
  #year = paste(year(as.yearmon(date)),month(as.yearmon(date)), sep="."),
  #       month = month(as.yearmon(date))) %>% 
  #select(category, id, date, month, year) %>% 
  group_by(location, date) %>% 
  summarise(n_crimes = n_distinct(id))


ggplot(police_grid,aes(x=date,y=location, fill=n_crimes))+
  #add border white colour of line thickness 0.25
  geom_tile(colour="white",size=0.25)+
  #remove x and y axis labels
  labs(x="",y="")+
  #remove extra space
  scale_y_discrete(expand=c(0,0))+
  #define new breaks on x-axis
  scale_x_discrete(expand=c(0,0), 
                   breaks=c("2016-01","2017-01", "2018-01"))+
  #one unit on x-axis is equal to one unit on y-axis.
  #maintains aspect ratio.
  scale_fill_viridis(option = "B") +
  ggtitle("Number of crimes at London Tube stations") +
  coord_fixed()+
  #set a base size for all fonts
  theme_grey(base_size=8)+
  #theme options
  theme(
    # vertical labels on x axis
    axis.text.x = element_text(),#angle = 90, hjust = 1),
    #bold font for both axis text
    axis.text=element_text(face="bold"),
    #set thickness of axis ticks
    axis.ticks=element_line(size=0.4),
    #remove plot background
    plot.background=element_blank(),
    #remove plot border
    panel.border=element_blank()
  ) +
  guides(fill=guide_legend(title="Number of crimes"))





