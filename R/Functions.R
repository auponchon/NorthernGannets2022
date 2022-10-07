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

trips_summary_ind<-function (dataset){
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
                                 DateEnd=temp$datetime[nrow(temp)])
                dist.max.all.trips<-rbind(dist.max.all.trips,maxi)
                
            }
    
    return(dist.max.all.trips)
}


######################################################################################
## Function to give the summary of all periods on land by individuals
######################################################################################

land_summary_ind<-function (dataset){
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
                                 DateEnd=tempo$datetime[nrow(tempo)])
                all.land<-rbind(all.land,maxo)
                
            }
        }
    }
    
    
    return(all.land)
}

######################################################################################
## Function to interpolate locations with regular intervals
######################################################################################
interpol<-function(data,time.int){

locs<-data %>% 
    dplyr::filter(travelNb!=0)

#convert to a ltraj object
locs<-droplevels(locs)
traj<-as.ltraj(locs[,c('long','lat')],date=locs$datetime,id=locs$id,
               burst=locs$trip.id, infolocs=locs)

#redistribute data with a resolution of 60s and project it in UTM
traj2=redisltraj(traj,u=time.int,burst='burst',type='time')
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
                               colo_coord_rouzic[1,c("long","lat")],miles=F))

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

