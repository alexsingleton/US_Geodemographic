library(readr)
library(tidycensus)
library(purrr)
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
library(magrittr)

vars <- read.csv("data/usa_trt_varnames_051313.csv",stringsAsFactors = F)

##SELECTED VARIABLES
in.vars <- as.character(vars[vars$new_set==0 & is.na(vars$new_set) == FALSE, "var"])
vars <- vars[vars$var %in% in.vars,]

##get list of variables with code and relative denom

vars %<>%
  mutate(code= sub('.*\\_','',var),
         code= paste0(substring(code,1,nchar(code)-3),"_",substring(code,nchar(code)-2,nchar(code))),
         pct=ifelse(substring(desc,1,3)=="PCT",TRUE,FALSE),
         denom=ifelse(pct==TRUE,paste0(substr(code,start=1, stop=6),"_001"),"NULL"))


denom<-unique(vars[vars$pct==TRUE,]$denom)
nopct<-vars[vars$pct==FALSE,]$code[4:length(vars[vars$pct==FALSE,]$code)]
codes<-append(vars[4:nrow(vars),]$code,denom)  

##set api key and retrieve country codes
census_api_key("c8117e4c46133dba95cd1654d808c8fcdbbedb2b")
us <- unique(fips_codes$state)[1:51]

##to parallelize the calls
c<- detectCores() - 1  
cl <- makeCluster(c)
registerDoParallel(cl)

x<-foreach(i = 1:length(codes),.packages=c('purrr','dplyr','tidycensus')) %dopar%{
  df <- map_df(us, function(x) {
    get_acs(geography = "tract", variables = codes[i], 
            state = x, year = 2018)
  })
  error <- paste0("moe_",codes[i])
  est <- paste0("est_",codes[i])
  df <-df %>%
    select(-variable) %>%
    rename(!! est := estimate,
           !! error := moe)
}

##transform the list into two dfs one for estimates one for errors
x2<-x %>%
  reduce(left_join,by=c("GEOID","NAME")) 
x3<-map(set_names(c("est","moe")),~select(x2,starts_with(.x),c("GEOID","NAME")))

data<-x[["est"]]
moe<-x[["moe"]]

##write data as r objects
saveRDS(moe,"./data/moe.rds")
saveRDS(data,"./data/data.rds")


#####Calculate proportions

getPrt<-function (.x, .y) {
  num<-data[,.x]
  denom<-data[,.y]
  prt<-num/denom
  return(prt)
}

num<-paste0("est_",vars[vars$pct=="TRUE",]$code)
denom<-paste0("est_",vars[vars$pct=="TRUE",]$denom)

prt<-purrr::map2(.x=num,.y=denom,.f=getPrt) %>% as.data.frame() 
names(prt)<-sub("est","prt",names(prt))

data_prt<-data.frame(data,prt)
