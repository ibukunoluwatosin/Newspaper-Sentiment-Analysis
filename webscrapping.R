install.packages("rvest")
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)

#Set Working directory
setwd("C:/Users/nc169de/Desktop/ASD/1. Sentiment analysis project")

#packageDescription("rvest")
#DATA COLLECTION
#Create list of url
get_url <- function(outlet,yr,mnt) {
  
  outlet <- paste0("http://www.nigeria70.com/nigerian_news_paper/archives/",outlet,"/")
  year <- paste0(outlet,yr,"/")
  url <- lapply(year, function (x) {paste0(x,mnt)}) %>% unlist()
  return(url)
}

this_day <- c(get_url("this_day", 2011:2017, 1:12),get_url("this_day", 2018, 1:6))
vanguard <- c(get_url("vanguard", 2011:2017, 1:12),get_url("vanguard", 2018, 1:6))
guardian <- c(get_url("guardian", 2011:2017, 1:12),get_url("guardian", 2018, 1:6))
punch <- c(get_url("the_punch", 2011:2017, 1:12),get_url("the_punch", 2018, 1:6))
sahara <- c(get_url("sahara_reporters", 2011:2017, 1:12),get_url("sahara_reporters", 2018, 1:6))
#-------------------------

#Get Headlines function
get_headline <- function(url) {
  headlines=url %>%
    read_html%>%
    html_nodes(".holder a") %>%
    html_text()
  headlines=headlines[headlines !=""]
  if(length(headlines)>0){
    return(headlines)
  }
  else{
    return(NA)
  }
  
}
#Get Dates
tempdate <- function(url){
 dates = url %>%
   read_html%>%
   html_nodes(".holder h3") %>%
   html_text()
  return(dates)
}
#Get first headliines for each day
firstHeadline <- function(url) {
  first_headline=url %>%
    read_html%>%
    html_nodes("h3 + p > a")%>%
    html_text()
  return(first_headline)
}
#Get the index of the headlines
headline_position <- function(headline,first_headline){
  
  position_of_dates = which(headline %in% first_headline)
  return (position_of_dates)
}
#Allocate date
allocate_date <- function(headline, position_of_dates, tempdates) {
  date = character(0)
  for(i in seq_along(headline)) {
    
    lst1=which( i>= position_of_dates)
    lst1=lst1[length(lst1)]
    date1 <- tempdates[lst1]
    date=append(date,date1)
    }
  return(date)
}

#Get Data frame
#----------
get_dataframe <- function (links) {
   temp_headlines <- sapply(links,get_headline)[,1]
  if(length(temp_headlines) >0) 
    {
    temp_tempdate <- sapply(links,tempdate)[,1]
    temp_fheadline <- sapply(links,firstHeadline)[,1]
    temp_date_position = which(temp_headlines %in% temp_fheadline)
    temp_date = character(0)
    for(i in seq_along(temp_headlines)) {
    
      lst1=which( i>= temp_date_position)
      lst1=lst1[length(lst1)]
      date1 <- temp_tempdate[lst1]
      temp_date=append(temp_date,date1)
  }
    temp_df = try(data.frame(Headlines=temp_headlines,Date=temp_date), silent = F)
    if (class(temp_df) != "try-error"){
      return(temp_df)
  }
    else{
      return(NULL)
  }}
}

#Vanguard
vanguard_df <- data.frame(Headlines=character(0),Date=character(0))
for(i in 78:length(vanguard) ){
  df=get_dataframe(vanguard[i])
  vanguard_df=rbind(vanguard_df,df)
  print(paste0(i,": ",scales::percent(i/length(vanguard))))
  flush.console()
}
write.csv(vanguard_df,"Vanguard.csv", row.names = F)

#Guardian missing months: 36-48 (2013/12 - 2014/12, 2015/04-2016/02)
guardian_df <- data.frame(Headlines=character(0),Date=character(0))
for(i in 1:length(guardian) ){
  df=get_dataframe(guardian[i])
  guardian_df=rbind(guardian_df,df)
  #Basically logs the output so you know where you stopped
  print(paste0(i,": ",scales::percent(i/length(guardian))))
  flush.console()
}
write.csv(guardian_df,"Guardian.csv", row.names = F)

#Punch missing months 1-10(2011/11-2011/12) 19-25(2012/07-2013/01)
punch_df <- data.frame(Headlines=character(0),Date=character(0))
for(i in 1:length(punch) ){
  df=get_dataframe(punch[i])
  punch_df=rbind(punch_df,df)
  #Basically logs the output so you know where you stopped
  print(paste0(i,": ",scales::percent(i/length(punch))))
  flush.console()
}
write.csv(punch_df,"Punch.csv", row.names = F)

#This day missing months:(2011/3, 2016/3)
this_day_df <- data.frame(Headlines=character(0),Date=character(0))
for(i in 1:length(this_day) ){
  df=get_dataframe(this_day[i])
  this_day_df=rbind(this_day_df,df)
  #Basically logs the output so you know where you stopped
  print(paste0(i,": ",scales::percent(i/length(this_day))))
  flush.console()
}
write.csv(this_day_df,"This day.csv", row.names = F)

sahara_df <- data.frame(Headlines=character(0),Date=character(0))
for(i in 1:length(sahara) ){
  df=get_dataframe(sahara[i])
  sahara_df=rbind(sahara_df,df)
  #Basically logs the output so you know where you stopped
  print(paste0(i,": ",scales::percent(i/length(sahara))))
  flush.console()
}
write.csv(sahara_df,"Sahara.csv", row.names = F)
