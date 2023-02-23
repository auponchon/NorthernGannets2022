extract_bassrock_2019<-function(file){
    library(lubridate)
    
      xx<-file %>% 
            read.table(.,header=T,
                       sep=",",
                       quote="") %>% 
            mutate(Date=as.Date(Date, format="%d/%m/%Y")) %>% 
          
            rename("id"="BTO",
                  "lat" = "Latitude",
                   "long" = "Longitude") %>% 
            mutate(id=as.factor(id),
                   datetime=as.POSIXct(strptime(paste(Date,
                                                      Time,
                                                      sep=" "),
                                                format="%F %H:%M:%S"),"GMT"),
                   site="Bass Rock",
                   alt=0,
                   speed=0,
                   sero=NA) %>% 
            dplyr::filter(lat!=0 & year(datetime) ==2019) %>% 
            dplyr::select(id,datetime,long,lat,alt,speed,site,sero) %>% 
         #   dplyr::filter(long != dplyr::lag(long, 1) & alt != dplyr::lag(alt, 1)) %>% 
            dplyr::arrange(id,datetime) %>% 
            #  distinct(id,datetime, .keep_all= TRUE) %>% 
            droplevels()
      
      return(xx)
}
