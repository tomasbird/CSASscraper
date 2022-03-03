### Test scripts for scraping Open CAnada site

install.packages("rvest")
install.packages("stringr")
install.packages("dplyr")
install.packages("gdata")
install.packages("Rtools")

library(rvest)
library(stringr)
library(dplyr)
library(gdata)

## for reading from 
url="https://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/index-eng.asp"
files=read_html(url) %>%
  html_nodes("ul") %>%
  html_nodes("li") %>%
  html_nodes('a') %>%
  html_attr('href')


resultfiles7=subset(files, (grepl("result-eng", files) & grepl("series=7", files)))
resultfiles6=subset(files, (grepl("result-eng", files) & grepl("series=6", files)))
resultfiles5=subset(files, (grepl("result-eng", files) & grepl("series=5", files)))
resultfiles4=subset(files, (grepl("result-eng", files) & grepl("series=4", files)))
resultfiles3=subset(files, (grepl("result-eng", files) & grepl("series=3", files)))
resultfiles2=subset(files, (grepl("result-eng", files) & grepl("series=2", files)))
resultfiles1=subset(files, (grepl("result-eng", files) & grepl("series=1", files)))



puburl="https://www.isdm-gdsi.gc.ca/csas-sccs/applications/Publications/"


#### Series 7 = SAR
alltab7=tibble()

for(i in resultfiles7){
  newurl=paste0(puburl, i)
  newtab=read_html(newurl) %>%
    html_nodes("table") %>%
    html_table() %>%
    data.frame() %>%
    mutate(year_num=gsub("[/]", "_", Number),
           Title=unlist(lapply(Title, FUN=function(x) strsplit(x, "[\r]")[[1]][1]))) %>%
    tibble()
  
  heads= read_html(newurl) %>%
    html_nodes("table") %>%
    html_elements("thead") %>%
    html_elements("th")%>%
    html_text()
  
  
  Authlist=read_html(newurl) %>%
    html_nodes("table") %>%
    html_elements("tbody") %>%
    html_elements("td") %>%
    gsub("[\r|\n|\t]", "", .) 
  
  rows=length(Authlist)/4
  
  authtable=matrix(Authlist, rows, length(heads), byrow=T) %>%
    data.frame()
  colnames(authtable)=c("url", "Regions", "Title", "Authors")
  
  authtable$Title=gsub("<td>|</td>|<br>", "", authtable$Title)
  authtable$Authors=gsub("<td>|</td>", "", authtable$Authors)
  authtable$Authors=gsub("<br>", "; ", authtable$Authors)
  authtable$Regions=gsub("<td>|</td>", "", authtable$Regions)
  authtable$Regions=gsub("<br>", "; ", authtable$Regions) 
  authtable$url=unlist(lapply(authtable$url, FUN=function(x) html_elements(minimal_html(x), "a") %>% html_attr("href")))     
  
  
  links=read_html(newurl) %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_nodes("a") %>%
    html_attr("href")%>%
    tibble() %>%
    mutate(docpath=unlist(lapply(., FUN=function(x) parse_url(x)$path)),
           docseries=unlist(lapply(docpath, FUN=function(x) strsplit(x, "/")[[1]][3])),
           docyear=unlist(lapply(docpath, FUN=function(x) strsplit(x, "/")[[1]][4])),
           docname=unlist(lapply(docpath, FUN=function(x) strsplit(x, "/")[[1]][5])),
           doclang=unlist(lapply(docname, FUN=function(x) strsplit(x, "-")[[1]][2])),
           doctype=unlist(lapply(doclang, FUN=function(x) strsplit(x, "[.]")[[1]][2])),
           doclang=unlist(lapply(doclang, FUN=function(x) strsplit(x, "[.]")[[1]][1])),
           year_num=unlist(lapply(docname, FUN=function(x) strsplit(x, "-")[[1]][1])),
           year=unlist(lapply(year_num, FUN=function(x) strsplit(x, "_")[[1]][1])),
           num=unlist(lapply(year_num, FUN=function(x) strsplit(x, "_")[[1]][2])))
  colnames(links)[1]="url"
  
  linkstab=merge(authtable, links, by="url") %>%
    tibble() %>%
    mutate(bullets=NA)
    
    for(j in 1:nrow(linkstab)){
      if(linkstab$doctype[j] %in% c("html","htm") & !is.na(linkstab$doctype[j])){
        linkstab$bullets[j]=read_html(linkstab$url[j]) %>%
          html_elements(xpath="//main//li") %>% 
         html_text()%>% 
          paste(collapse="\n ")
      }
    }

    write.csv(linkstab, file=with(linkstab, paste0(docseries[1],"-", docyear[1], ".csv")))
    print(i)
    
  }
  write.csv(alltab7x, "alltab7.csv")

  sar= dir()[grepl("sar", tolower(dir()))]
  
  sard=data.frame()
  for(k in sar){
    dat=read.csv(k)
    sard=rbind(sard, dat)
  }
  
  write.csv(sard, file="AllSARdocs.csv")


#### Series 4 = Research Documents
alltab4=tibble()

for(i in resultfiles4){
  newurl=paste0(puburl, i)
  newtab=read_html(newurl) %>%
    html_nodes("table") %>%
    html_table() %>%
    data.frame() %>%
    mutate(year_num=gsub("[/]", "_", Number),
           Title=unlist(lapply(Title, FUN=function(x) strsplit(x, "[\r]")[[1]][1]))) %>%
    tibble()
     
  heads= read_html(newurl) %>%
    html_nodes("table") %>%
    html_elements("thead") %>%
    html_elements("th")%>%
    html_text()
  
  
   Authlist=read_html(newurl) %>%
        html_nodes("table") %>%
        html_elements("tbody") %>%
        html_elements("td") %>%
        gsub("[\r|\n|\t]", "", .) 
  
   rows=length(Authlist)/4
   
   authtable=matrix(Authlist, rows, length(heads), byrow=T) %>%
     data.frame()
     colnames(authtable)=c("url", "Regions", "Title", "Authors")
     
 authtable$Title=gsub("<td>|</td>|<br>", "", authtable$Title)
 authtable$Authors=gsub("<td>|</td>", "", authtable$Authors)
 authtable$Authors=gsub("<br>", "; ", authtable$Authors)
 authtable$Regions=gsub("<td>|</td>", "", authtable$Regions)
 authtable$Regions=gsub("<br>", "; ", authtable$Regions) 
 authtable$url=unlist(lapply(authtable$url, FUN=function(x) html_elements(minimal_html(x), "a") %>% html_attr("href")))     
  
  
  links=read_html(newurl) %>%
    html_nodes("tr") %>%
    html_nodes("td") %>%
    html_nodes("a") %>%
    html_attr("href")%>%
    tibble() %>%
    mutate(docpath=unlist(lapply(., FUN=function(x) parse_url(x)$path)),
           docseries=unlist(lapply(docpath, FUN=function(x) strsplit(x, "/")[[1]][3])),
           docyear=unlist(lapply(docpath, FUN=function(x) strsplit(x, "/")[[1]][4])),
           docname=unlist(lapply(docpath, FUN=function(x) strsplit(x, "/")[[1]][5])),
           doclang=unlist(lapply(docname, FUN=function(x) strsplit(x, "-")[[1]][2])),
           doctype=unlist(lapply(doclang, FUN=function(x) strsplit(x, "[.]")[[1]][2])),
           doclang=unlist(lapply(doclang, FUN=function(x) strsplit(x, "[.]")[[1]][1])),
           year_num=unlist(lapply(docname, FUN=function(x) strsplit(x, "-")[[1]][1])),
           year=unlist(lapply(year_num, FUN=function(x) strsplit(x, "_")[[1]][1])),
           num=unlist(lapply(year_num, FUN=function(x) strsplit(x, "_")[[1]][2])))
  colnames(links)[1]="url"
  
  linkstab=merge(authtable, links, by="url") %>%
    tibble() %>%
    mutate(abstract=NA)
  
  for(j in 1:nrow(linkstab)){
    if(linkstab$doctype[j] %in% c("html","htm") & !is.na(linkstab$doctype[j])){
      uls=read_html(linkstab$url[j]) %>%
        html_elements("main") %>% 
        html_elements("p") 
      
      linkstab$abstract[j]=uls[!(grepl("class=", uls)|grepl("This document is available in", uls))] %>%
        html_text() %>% paste0(., collapse="\n ")
      
    }
  }
  alltab4=rbind(alltab4, linkstab)
  write.csv(linkstab, file=with(linkstab, paste0(docseries[1],"-", docyear[1], ".csv")))
  print(i)
  
}

rf=dir() %>%
  subset(., grepl("resdocs", tolower(.)))

rd=data.frame()
for(k in rf){
  dat=read.csv(k)
  rd=rbind(rd, dat)
}

write.csv(rd, file="AllResdocs.csv")


# For reading from Open Canada 
read.CSAS.urls=function(url, subname){
  resultfiles<-read_html(url) %>%
    html_nodes('tr') %>%
    html_nodes("td") %>%
    html_nodes('a') %>%
    html_attr("href")
  
  
  
  Resnames<-read_html(url) %>%
    html_nodes('table') %>%
    html_table()
  
  tab=data.frame(Resnames[[1]][,1], files)
  
  dl.folder=paste0("C:\\Users\\birdt\\Documents\\GitHub\\CSASscraper\\Data\\", subname)
  
  for(i in 1:nrow(tab)){
    filename=strsplit( tab[i,2], "download")[[1]][2]
    filename=gsub("/", "", filename)
    try(download.file(tab[i,2], destfile=paste0(dl.folder,"\\", filename)), silent=T)
    
  }
  
}

# Science Advice
csasurl<-"https://open.canada.ca/data/en/dataset/ea82d9e8-2c90-47a7-b903-a485d43f01c7"
read.CSAS.urls(csasurl, "SAR")

### Ecosystem Status Report
ESRurl="https://open.canada.ca/data/en/dataset/5e1475b2-4bee-4de1-8cc6-c65e73fb0e3f"
read.CSAS.urls(ESRurl, "ESR")

### Stock Status Report
SSRurl="https://open.canada.ca/data/en/dataset/09a54332-838b-4485-bb46-18a5eb97cbea"
read.CSAS.urls(SSRurl, "SSR")

### Habitat Status Report
HSRurl="https://open.canada.ca/data/en/dataset/8673804c-be41-4cda-88dc-1f43a68909ce"
read.CSAS.urls(HSRurl, "HSR")

### Science Response Process
SRPurl="https://open.canada.ca/data/en/dataset/504112f8-efff-4eed-8c28-0ba7fe113294"
read.CSAS.urls(SRPurl, "SRP")

### CSAS proceedings
Procurl="https://open.canada.ca/data/en/dataset/f405a6f7-defb-427f-a6cb-8ff5d3732605"
read.CSAS.urls(Procurl, "Proc")

### CSAS Research Documents
RDurl="https://open.canada.ca/data/en/dataset/252d41c5-6e76-40b4-aee7-db64c2e2ce53"
read.CSAS.urls(RDurl, "RD")


### Bind files
library(gdata)
        
### Research Documents
RDfiles=dir("Data\\RD")
for(i in RDfiles){
  tab=read.xls(i, sheet=1, blank.lines.skip=TRUE, perl="C:\\Users\\birdt\\AppData\\Local\\Programs\\Git\\usr\\bin\\perl.exe")
  }






