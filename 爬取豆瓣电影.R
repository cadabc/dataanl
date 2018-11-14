ls()
rm(list=ls())

library(xml2) 
library(rvest)
library(stringr)

for (j in 0:9){
  if(j==0){
url <- 'https://movie.douban.com/top250'
  } else{
url <- paste('https://movie.douban.com/top250?start=',25*j,'&filter',sep="") 
}


webpage <- read_html(url)    #xml2
#排名
rank_data_html <- html_nodes(webpage,'em')  #rvest
rank_data <- html_text(rank_data_html)     #rvest
rank_data <- as.numeric(rank_data)
#电影名
title_data_html <- html_nodes(webpage,'.title:nth-child(1)')
title_data <- html_text(title_data_html)
#评分
score_data_html <- html_nodes(webpage,'.rating_num')
score_data <- html_text(score_data_html)
score_data <- as.numeric(score_data)
#评价人数
counter_data_html <- html_nodes(webpage,'.star:nth-child(2)')
counter_data <- html_text(counter_data_html)
counter_data <- unlist(str_extract_all(counter_data,"[0-9]+")) #stringr
a <- length(unlist(str_extract_all(counter_data,"[0-9]+")))
counter_data <- counter_data[seq(3,a,3)]
counter_data <- as.numeric(counter_data)
counter_data
#描述
description_data_html <- html_nodes(webpage,'.inq')
description_data <- html_text(description_data_html)

gener_data_html <- html_nodes(webpage,'.bd p:nth-child(1)')
gener_data <- html_text(gener_data_html)
gener_data <- gener_data[-1]
#年份
year_data <- NULL
for(i in 1:length(gener_data)){
  year_data[i] <- unlist(str_extract_all(gener_data[i],"[0-9]+"))[1]
}


#取导演数据
directors_data <- NULL
for(i in 1:length(gener_data)){
n1 <-str_trim(strsplit(gener_data[i],'\n')[[1]][2])
n2 <- regexpr("\\s{3}",n1)
directors_data[i] <- substr(n1,5,n2-1)
}

#取主演数据
actors_data <- NULL
for(i in 1:length(gener_data)){
  n1 <- regexpr("主",strsplit(gener_data[i],'\n')[[1]][2])
  n2 <- nchar(strsplit(gener_data[i],'\n')[[1]][2])
  actors_data[i] <- substr(strsplit(gener_data[i],'\n')[[1]][2],n1+4,n2)
}

#取国家数据
country_data <- NULL
for(i in 1:length(gener_data)){
  a<-strsplit(gener_data[i],'\n')[[1]][3]
  n1 <- length(unlist(gregexpr("\\s\\/\\s",a)))
  n2 <- unlist(gregexpr("\\s\\/\\s",a))[n1]
  n3 <- unlist(gregexpr("\\s\\/\\s",a))[n1-1]
  country_data[i] <- substr(a,n3+3,n2-1)
}

#取类型
style_data <- NULL
for(i in 1:length(gener_data)){
  a<-strsplit(gener_data[i],'\n')[[1]][3]
  n1 <- length(unlist(gregexpr("\\s\\/\\s",a)))
  n2 <- unlist(gregexpr("\\s\\/\\s",a))[n1]
  n3 <- unlist(gregexpr("\\s\\/\\s",a))[n1-1]
  style_data[i] <- substr(a,n2+3,nchar(a))
  }

if(j== 0){
rank =rank_data
title = title_data
score = score_data
description = description_data
year =year_data
country=country_data
directors=directors_data
actors=actors_data
style=style_data
counter =counter_data} else{
rank =c(rank,rank_data)
title = c(title,title_data)
score = c(score,score_data)
description = c(description,description_data)
year =c(year,year_data)
country=c(country,country_data)
directors=c(directors,directors_data)
actors=c(actors,actors_data)
style=c(style,style_data)
counter =c(counter,counter_data)
}}

for (i in 250:215){
  description[i]=description[i-1]
}
description[215] <- ""
film.data1 <- data.frame(排名 = rank,电影名 = title,评分=score,
                           简介 =description,上映年份 = year,
                           国家=country, 导演 = directors,
                           主演=actors,题材 = style,评价人数=counter)

write.csv(film.data1,"豆瓣电影top250.csv",row.names = FALSE)
