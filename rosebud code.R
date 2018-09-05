library(rvest)
library(dplyr)
library(plyr)
library(stringr)
base_url="http://www.nigeria70.com/nigerian_news_paper/archives"

outlets = c("archives/this_day","archives/vanguard","archives/guardian","archives/the_punch")
news_outlets=base_url %>%
  read_html()%>%
  html_nodes(paste0(".holder a[href*='",outlets,"']",collapse = ", "))%>%
  html_attr("href")
news_outlets <- paste0("http://www.nigeria70.com",news_outlets)
news_outlets
get_archive_links <- function(url){
  links=url %>%
    read_html()%>%
    html_nodes(paste0(".holder a[href*='",2011:2018,"/']",collapse = ", ")) %>%
    html_attr("href")
  
  return(links)
}
headline_archives<- sapply(news_outlets,get_archive_links)
headline_archives<- paste0("http://www.nigeria70.com",headline_archives)


get_data <- function(url){
  #Headlines
  headlines <-url %>%
    read_html()%>%
    html_nodes(".holder a")%>%
    html_text()
  headlines=headlines[headlines!=""]
  if(length(headlines)>0){
  dates <- url %>%
    read_html()%>%
    html_nodes(".holder h3")%>%
    html_text()
  first_headline <- url %>%
    read_html()%>%
    html_nodes("h3 + p > a")%>%
    html_text()
  position_of_dates=which(headlines %in% first_headline)
  headline_dates=character(0)
  for(idx in seq_along(headlines)){
    lst1=which(idx>=position_of_dates)
    lst1=lst1[length(lst1)]
    date=dates[lst1]
    headline_dates=append(headline_dates,date)
  }
  name=str_split(url,"/",simplify = T)[6] %>%
    str_to_title()
  
  headlines_df=try(data.frame(headline=headlines,Date=headline_dates,news_outlet=name),silent=F)
  if(class(headlines_df)!="try-error"){
  return(headlines_df)
  }
  else{
    return(NULL)
  }
  }
  
}

full_df=data.frame(headline=character(0),Date=character(0),news_outlets=character(0))
#ldply(headline_archives$archives,get_data)
for(i in 185:length(headline_archives)){
  df=get_data(headline_archives$archives[i])
  full_df=rbind(full_df,df)
  print(paste0(i,": ",scales::percent(i/length(headline_archives))))
  flush.console()
}
