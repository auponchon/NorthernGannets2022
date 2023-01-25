extract_grassholm<-function(file,starting){

# weird<-files[c(6,10)]
    
       
xx<-file %>% 
    read_delim(.,col_names=F,delim="\t",skip=1) %>% 
    as.data.frame(.) %>% 
        rename(id = X1,
               datetime = X2,
               long =X8,
               lat= X7,
               alt = X9) %>% 
    dplyr::select(id,datetime, long, lat, alt) %>% 
    mutate(id=as.factor(substr(id,1,4)),
           datetime=as.POSIXct(strptime(datetime,format="%d/%m/%Y %H:%M:%S"),"GMT"),
           long=as.numeric(long),
           lat=as.numeric(lat),
           speed=0,
           site=as.factor("Grassholm"),
           sero=NA) %>% 
   dplyr::filter(lat!=0 & datetime > starting ) %>% 
    dplyr::select(id,datetime,long,lat,alt,speed,site,sero) %>% 
   # mutate(datetime=round_date(datetime,unit="2 minutes")) %>% 
    dplyr::filter(long != dplyr::lag(long, 1) & alt != dplyr::lag(alt, 1)) %>% 
    dplyr::arrange(id,datetime) %>% 
  #  distinct(id,datetime, .keep_all= TRUE) %>% 
    droplevels()

return(xx)
}
