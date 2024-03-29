######################################################################################
## Parameters common to the trip analyses
######################################################################################
library(fields)

#WGS84 projection
wgscrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#France and UK projection
projcrs<-"+proj=utm +zone=30 +ellps=WGS84"

#Coordinates of Rouzic colony (France)
colo_coord_rouzic<-data.frame(long=-3.436752, lat=48.899868,name="Rouzic")
#Coordinates of Bass Rock colony (Scotland)
colo_coord_bassrock<-data.frame(long=-2.641272, lat=56.077072,name="Bass Rock")
#Coordinates for Grassholm (England)
colo_coord_grassholm<-data.frame(long=-5.4803, lat=51.731286, name="Grassholm")

start<-as.POSIXct("2022-08-11 08:00:00",format="%F %H:%M:%S",tz="GMT") #remove positions onland before deployment for bassrock


#Time resolution for interpolation
reso<-60*15 #(in seconds)

#distance threshold from the colony to determine a trip
dist.threshold<-1  

##Parameters to define clean trips and filter locations
row.thres<- 5               #minimum number of rows constituting a trip
dur.thres<-1         #minimum duration of a trip (in h)





######################################################################################
## Function to give a trip number based on the consecutive distances to the colony
######################################################################################
define_trips<-function(data,dist.min){
    
        w<-1
        z<-1
        for (a in 1:nrow(data)){
            #Define points away from the colony (travelNb) and in the colony (onlandNb)
            ifelse(data[a,"distmax"]>=dist.min,  
                   data[a,"travelNb"]<-w,
                   data[a,"onlandNb"]<-z) 
            ifelse(data[a,"travelNb"]==0,w<-w+1,z<-z+1)
        } 
        
        trip.nb<-sort(unique(data$travelNb))
        land.nb<-sort(unique(data$onlandNb))
      
        
        #assign consecutive id numbers for trips and period on land
        v<-1
        q<-1
         
        #assign consecutive id trips at sea
        if(length(trip.nb)>1){
            for (z in 2:length(trip.nb)){
                trip<-which(data$travelNb==trip.nb[z])
                ifelse(data$datetime[trip[length(trip)]] != data$datetime[nrow(data)],
                       rowtrip<-c(trip[1]-1,trip,trip[length(trip)]+1),
                       rowtrip<-c(trip[1]-1,trip))
  #              if(length(rowtrip) >= row.min & sum(data$difftimemin[rowtrip]) > dur.min){
                    data$travelNb[rowtrip]<-v
                    v<-v+1  
                    #}   #if trip ok, consecutive number
                #else{data$travelNb[rowtrip]<-1000                }
            } #end of loop within trips
        }
        
        
        #assign consecutive id periods on land
        if(length(land.nb)>1){
            for (z in 2:length(land.nb)){
                onland<-which(data$onlandNb==land.nb[z])
                if(data$travelNb[1]==0){data$onlandNb[1]<-1}
                if(data$travelNb[nrow(data)]==0){
                    data$onlandNb[nrow(data)]<-data$onlandNb[nrow(data)-1]}
                if(length(onland)>1){
                    number<-onland[-c(1,length(onland))]
                data$onlandNb[number]<-q
                q<-q+1}
               
            }    #if trip ok, consecutive number
        } #end of loop within trips
        
        data$onlandNb[data$travelNb!=0]<-0      
            
       
      #calculate total distance for the trip
            for (b in 2:nrow(data)){
                if(data$travelNb[b]>0){
                    ifelse(data[b-1,"travelNb"] != 0 & data[b-1,"travelNb"]!=data[b,"travelNb"],
                           data[b,"totalpath"]<-0,
                           data[b,"totalpath"]<-data$distadj[b]+data$totalpath[b-1])}
            }
    
    return(data)
}



######################################################################################
## Function to give the summary of all raw trips by individuals
######################################################################################

trips_summary_ind<-function (dataset,colony){
    nb.trip<-unique(dataset$trip.id) 
    dist.max.all.trips<-NULL
    
    for (k in 1:length(nb.trip)){
        temp<-subset(dataset,dataset$trip.id==nb.trip[k])
                maxi<-data.frame(id=temp$id[1],
                           #      travelNb=nb.trip[k],
                                 trip.id=temp$trip.id[1],
                                 Distmaxkm=max(temp$distmax),
                                 TripDurh=sum(temp$difftimemin)/60,
                                 maxDiffTimeh=max(temp$difftimemin)/60,
                                 TotalPathkm=temp$totalpath[nrow(temp)],
                                 nlocs=nrow(temp),
                                 DateEnd=temp$datetime[nrow(temp)],
                                 site = colony$name)
                dist.max.all.trips<-rbind(dist.max.all.trips,maxi)
                
            }
    
    return(dist.max.all.trips)
}


######################################################################################
## Function to give the summary of all periods on land by individuals
######################################################################################

land_summary_ind<-function (dataset,colony){
    ids<-unique(dataset$id) 
    all.land<-NULL
    
    for (k in 1:length(ids)){
        temp<-subset(dataset,dataset$id==ids[k])
        nb.land<-sort(unique(temp$onlandNb))
        
        if (length(nb.land)>1){
            for (a in 2:length(nb.land)){
                tempo<-subset(temp,temp$onlandNb==nb.land[a])
                
                maxo<-data.frame(id=ids[k],
                                 onlandNb=nb.land[a],
                                 onland.id=paste(nb.land[a],sep="."),
                                 LandDurh=sum(tempo$difftimemin)/60,
                                 maxDiffTimeh=max(tempo$difftimemin)/60,
                                 nlocs=nrow(tempo),
                                 DateEnd=tempo$datetime[nrow(tempo)],
                                 site=colony$name)
                all.land<-rbind(all.land,maxo)
                
            }
        }
    }
    
    
    return(all.land)
}

######################################################################################
## Function to add last missing location in colony for a trip
######################################################################################

add_missing_return<-function(dataset,time.int,colony){
    
    trip<-unique(dataset$trip.id)
    
    for (a in 1:length(trip)){
        
        temp.trip<- dataset %>% 
            dplyr::filter(trip.id==trip[a])
        
        if (temp.trip$distmax[nrow(temp.trip)] > dist.threshold & temp.trip$distmax[nrow(temp.trip)] < 25){
            taily1<-temp.trip[nrow(temp.trip),] 
            taily<-taily1 %>% 
                mutate(datetime=datetime+time.int,
                       long=colony$long,
                       lat=colony$lat,
                       distmax=0,
                       speed=0,
                       difftimemin=time.int/60,
                       distadj=as.vector(rdist.earth(temp.trip[nrow(temp.trip),c("long","lat")],
                                                     colony[1,c("long","lat")],miles=F)),
                       totalpath=temp.trip$totalpath[nrow(temp.trip)] + 
                           as.vector(rdist.earth(temp.trip[nrow(temp.trip),c("long","lat")],
                                                 colony[1,c("long","lat")],miles=F)))
            
            
            dataset<-rbind(dataset,taily)
            
  #          print(c(a,trip[a]))
        }   }
    
    dataset<-dataset %>%
        arrange(id,datetime,trip.id)
    
    return(dataset)
    
    
}


######################################################################################
## Function to interpolate locations with regular intervals with adehabitatLT
######################################################################################
interpol_ltraj<-function(data,time.int,colony){
library(adehabitatLT)
    
locs<-data %>% 
    dplyr::filter(travelNb!=0)

#convert to a ltraj object
locs<-droplevels(locs)
traj<-as.ltraj(locs[,c('long','lat')],date=locs$datetime,id=locs$id,
               burst=locs$trip.id, infolocs=locs)

#redistribute data with a resolution of 60s and project it in UTM
traj2=redisltraj(traj,u=time.int,burst='burst',type='time',samplex0=T,
                 addbit=T)
#loc.interp<-ltraj2spdf(traj2)

#convert the ltraj object to a dataframe and add missing info
loc.interp<-ld(traj2) %>% 
    rename(datetime=date,
           long=x,
           lat=y,
           trip.id=burst) %>% 
    mutate(site="rouzic",
           totalpath=0,
           distadj=0) %>% 
    dplyr::select(id,datetime,long,lat,site,trip.id,totalpath,distadj)

loc.interp$distmax<-as.vector(rdist.earth(loc.interp[,c("long","lat")],
                               colony[1,c("long","lat")],miles=F))

loc.interp$difftimemin<-c(0,difftime( loc.interp$datetime[2:nrow(loc.interp)],
                                     loc.interp$datetime[1:nrow(loc.interp)-1],units="mins"))
loc.interp$difftimemin[loc.interp$difftimemin != 15]<-0

for (b in 2:nrow(loc.interp)){
    ifelse(loc.interp[b-1,"trip.id"] != loc.interp[b,"trip.id"],
           loc.interp[b,"distadj"]<-0,
           loc.interp[b,"distadj"]<-rdist.earth(loc.interp[b,c("long","lat")],
                                                loc.interp[b-1,c("long","lat")], miles = F))
    
        ifelse(loc.interp[b-1,"trip.id"] != loc.interp[b,"trip.id"],
               loc.interp[b,"totalpath"]<-0,
               loc.interp[b,"totalpath"]<-loc.interp$distadj[b]+loc.interp$totalpath[b-1])
}

return(loc.interp)
}



######################################################################################
## Function to interpolate locations with regular intervals with pastecs
######################################################################################
interpol_pastecs<-function(data,time.int,colony){
    library(pastecs)
    
    locs<-data %>% 
        dplyr::filter(travelNb!=0)
    
    #convert to a ltraj object
    locs<-droplevels(locs)
    
    
    trip<-unique(locs$trip.id)
    
    new.trip1<-NULL
    
    #interpolate data for each trip separately 
    for (y in 1:length(trip)){
        tripy<-locs %>% 
            dplyr::filter(trip.id==trip[y])
        
        
        tripy$TimeSinceOrigin<-rep(0,nrow(tripy))
        
        for (z in 1:nrow(tripy)){
            tripy$TimeSinceOrigin[z]<-difftime(tripy$datetime[z],tripy$datetime[1],units="sec")
        }
        
        
        # Resampling of long and lat with regular time intervals 
        sub.lat1 <- regul(x=tripy$TimeSinceOrigin, y=tripy$lat,
                          n=round((max(tripy$TimeSinceOrigin)/time.int),0),
                          deltat=time.int, methods="linear",units="sec")
        sub.lon1 <- regul(x=tripy$TimeSinceOrigin, y=tripy$long,
                          n=round((max(tripy$TimeSinceOrigin)/time.int),0),
                          deltat=time.int, methods="linear",units="sec")
        
        new.sub <- data.frame(id=rep(tripy$id[1],length(sub.lon1[[1]])),
                              datetime=tripy$datetime[1]+as.vector(sub.lon1[[1]]),
                              long=sub.lon1[[2]]$Series, 
                              lat=sub.lat1[[2]]$Series, 
                              site=rep(colony$name,length(sub.lon1[[1]])),
                              travelNb=rep(tripy$travelNb[1],length(sub.lon1[[1]])),
                              trip.id=rep(trip[y],length(sub.lon1[[1]])))
        
        new.sub<-new.sub %>% 
            mutate(distmax=as.vector(rdist.earth(new.sub[,c("long","lat")],
                                               colony[1,c(1,2)],miles=F)),
                   difftimemin=c(0,difftime(new.sub$datetime[2:nrow(new.sub)],
                                       new.sub$datetime[1:nrow(new.sub)-1],units="mins")),
                   distadj=0,
                   totalpath=0,
                   speed=0,
                   alt=0,
                   sero=0,
                   onlandNb=0)
        
        for (z in 2:nrow(new.sub)){
            new.sub$distadj[z]<-rdist.earth(new.sub[z,c("long","lat")],
                                            new.sub[z-1,c("long","lat")], miles=F)
            
            new.sub$totalpath[z]<-new.sub$totalpath[z-1] + new.sub$distadj[z]
            new.sub$speed[z]<-new.sub$distadj[z]/new.sub$difftimemin[z]
        }
     
        new.sub<-new.sub %>% 
            dplyr::select(id,datetime,long,lat,alt,speed,site,sero,distmax,
                          distadj,totalpath,difftimemin,travelNb,onlandNb,trip.id)
        
        new.trip1<-rbind(new.trip1,new.sub)
    }
    
    new.trip<-add_missing_return(new.trip1,time.int,colony)
    return(new.trip)
}

######################################################################################
## Function to plot a legend in ggplot
######################################################################################

g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}

    
    


