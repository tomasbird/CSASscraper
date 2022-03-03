### download species records from OBIS
install.packages("robis")

library(devtools)
library(dplyr)
install_github("iobis/robis")
devtools::install_github("iobis/obistools")
library("obistools")

 
library(robis)

robis::checklist()



### group CU data
types=c("Chinook", "Chum", "Coho", "Even_Year_Pink", "Odd_Year_Pink", "Lake_Type_Sockeye", "River_Type_Sockeye")

CU_Bound=data.frame()
CU_Status=data.frame()

datlist=dir("Dictionaries")
for(t in types){
  datlist=dir(paste0("Dictionaries/", t))
  boundfile=datlist[grep("CU_BOUNDARY_En", datlist)]
  bounddf=read.csv(paste0("Dictionaries/", t, "/",boundfile))
  
  sitefile= datlist[grep("CU_SITES_En", datlist)]
  sitedf=read.csv(paste0("Dictionaries/", t, "/",sitefile)) %>%
    subset(select=c("FULL_CU_IN", "Y_LAT", "X_LONG", "GFE_ID")) %>% 
    group_by(FULL_CU_IN) %>%
    summarise(LAT_C=mean(Y_LAT),
           LONG_C=mean(X_LONG),
           nsites=length(GFE_ID)) %>%
    subset(select=c("FULL_CU_IN", "LAT_C", "LONG_C")) %>%
    unique() %>% 
    right_join(bounddf) %>%
    mutate(species=t)

  
CU_Bound=rbind(CU_Bound, sitedf)
statusfile=datlist[grep("CU_STATUS_En", datlist)]
statdf=try(read.csv(paste0("Dictionaries/", t, "/",statusfile)), silent=T)

if(class(statdf)=="data.frame"){
  sitedf=sitedf %>%
    right_join(CU_Bound)}  

}

write.csv(CU_Bound, "Dictionaries/CU_Boundaries.csv")
