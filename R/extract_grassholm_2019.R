extract_grassholm2019<-function(file){
    
    xx<-file %>% 
        read.csv2(.,header=T,
                   sep=",",
                   quote="",
                  as.is=T)  %>% 
        rename("id"="X.LAT.",
               "lat" = "X.LON.",
               "long" = "X.row_ID.",
               "datetime" = "X.trip.",
               "speed" = "X.speed.") %>% 
        mutate(datetime=gsub('\\"', "",datetime),
               id=as.factor(gsub('\\"', "",id))) %>% 
        mutate_at(c("long","lat","speed"),as.numeric) %>% 
        mutate(datetime=as.POSIXct(strptime(datetime,
                                            format="%F %H:%M:%S"),"GMT"),
               site="Grassholm",
               alt=NA,
               sero=NA) %>% 
        dplyr::filter(lat!=0) %>% 
        dplyr::select(id,datetime,long,lat,alt,speed,site,sero) %>% 
    #    dplyr::filter(long != dplyr::lag(long, 1) & alt != dplyr::lag(alt, 1)) %>% 
        dplyr::arrange(id,datetime) %>% 
        #  distinct(id,datetime, .keep_all= TRUE) %>% 
        droplevels()
    
    return(xx)
}
    