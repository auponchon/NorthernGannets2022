######################################################################################
## Function to give a trip number based on the consecutive distances to the colony
######################################################################################

define_trips<-function(data,dist.min){
    
        w<-1
        z<-1
        for (a in 1:nrow(data)){
            #Define points away from the colony (travalNb) and in the colony (onlandNb)
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
  #              if(length(rowtrip) >= row.min & sum(data$difftime[rowtrip]) > dur.min){
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
                if(length(onland)>1){
                    number<-onland[-c(1,length(onland))]}
                if(data$travelNb[1]==0){data$onlandNb[1]<-1}
                data$onlandNb[number]<-q
                q<-q+1
            }    #if trip ok, consecutive number
        } #end of loop within trips
        
        data$onlandNb[data$travelNb!=0]<-0      
            
       
      #calculate total distance for the trip
            for (b in 2:nrow(data)){
                if(data$travelNb[b]>0){
                    data[b,"totalpath"]<-data$distadj[b]+data$totalpath[b-1]}
            }
    
    return(data)
}



######################################################################################
## Function to give the summary of all raw trips by individuals
######################################################################################

raw_trips_summary_ind<-function (dataset){
    ids<-unique(dataset$id) 
    dist.max.all.trips<-NULL
    
    for (k in 1:length(id)){
        temp<-subset(dataset,dataset$id==ids[k])
        nb.trip<-sort(unique(temp$travelNb))
        
        if (length(nb.trip)>1){
            for (a in 2:length(nb.trip)){
                tempo<-subset(temp,temp$travelNb==nb.trip[a])
                
                maxi<-data.frame(ID=ids[k],travelNb=nb.trip[a],
                                 Distmax(km)=max(tempo$distmax),TripDur(h)=sum(tempo$difftime)/60,
                                 maxDiffTime(h)=max(tempo$difftime)/60,TotalPath(km)=tempo$totalpath[nrow(tempo)])
                dist.max.all.trips<-rbind(dist.max.all.trips,maxi)
                
            }
        }
    }
    
    
    return(dist.max.all.trips)
}
