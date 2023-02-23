extract_rouzic_2019<-function(file){
    xx<-file %>% 
        read.table(.,header=T,
                   sep=",",
                   quote="") %>% 
        mutate(id=as.factor(paste0("GPS_",
                                   as.numeric(sub("(?i).*GPS.*?(\\d+).*", 
                                                  "\\1", 
                                                  file)))),
               Date=as.Date(Date, format="%Y/%m/%d")) %>% 
        relocate(id,.before=Date) %>% 
        rename("lat" = "Latitude",
               "long" = "Longitude",
               "alt" = "Altitude",
               "speed" = "Speed") %>% 
        mutate(datetime=as.POSIXct(strptime(paste(Date,
                                                  Time,
                                                  sep=" "),
                                            format="%F %H:%M:%S"),"GMT"),
               site="Rouzic",
               sero=NA) %>% 
        
        dplyr::filter(lat!=0) %>% 
        dplyr::select(id,datetime,long,lat,alt,speed,site,sero) %>% 
        dplyr::filter(long != dplyr::lag(long, 1) & alt != dplyr::lag(alt, 1)) %>% 
        dplyr::arrange(id,datetime) %>% 
        #  distinct(id,datetime, .keep_all= TRUE) %>% 
        droplevels()
    
    return(xx)
}
