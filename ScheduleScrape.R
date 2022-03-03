install.packages("rvest")
install.packages("stringr")
install.packages("dplyr")
install.packages("gdata")
install.packages("Rtools")
install.packages("xml2")
install.packages("RSelenium")
install.packages("here")

library(rvest)
library(stringr)
library(dplyr)
library(gdata)
library(xml2)
library(RSelenium)
library(here)



### Grab aquatic Species List
Aqurl="https://www.dfo-mpo.gc.ca/species-especes/identify-eng.html"
aqpage=read_html(Aqurl) %>%
  html_elements("td")


## Get list of years and link to the schedule year page
url="https://www.isdm-gdsi.gc.ca/csas-sccs/applications/events-evenements/index-eng.asp"
mainurl="https://www.isdm-gdsi.gc.ca/csas-sccs/applications/events-evenements/"
yearlinks=read_html(url) %>%
  html_elements("main .list-inline a") %>%
  html_attr("href")

yeartext=read_html(url) %>%
  html_elements("main .list-inline a") %>%
  html_text()

ScheduleYearLink=data.frame(year=yeartext, html=paste0(mainurl,yearlinks))
write.csv(ScheduleYearLink, "outputdata/ScheduleYearLink.csv")

  

page <- read_html("https://www.fbi.gov/about-us/cjis/ucr/crime-in-the-u.s/2013/crime-in-the-u.s.-2013/tables/5tabledatadecpdf/table_5_crime_in_the_united_states_by_state_2013.xls")


pageAdd <- page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xls") %>% # find those that end in xls
  .[[1]]     

## Go to each year page and get the schedule table for each year
Sched.table=data.frame()
for(i in 1:nrow(ScheduleYearLink)){
  page <- read_html(ScheduleYearLink[i,2])
  
  
  pageAdd <- page %>%
    html_elements("button.pull-right") %>%       # find all links
    download.file(destfile="outputdata/2022schedule.xls")
  
  mydestfile <- "D:/Kumar/table5.xls" # change the path and file name as per your system
  download.file(pageAdd, mydestfile, mode="wb")
  tab=read_html(taburl)%>%
    html_elements("button.pull-right")
  
}



  html_nodes("ul") %>%
  html_nodes("li") %>%
  html_nodes('a') %>%
  html_attr('href')
