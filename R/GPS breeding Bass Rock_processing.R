#####################################
##     GPS data processing         ##
##       Jana Jeglinski            ##
##        August 2022              ##
#####################################


library(sf)
library (tidyr)
library(stringi)
library(tmap)
library(ggplot2)
library(viridis)
library(viridisLite)


### Load loc solved data
# insert your file path here if necessary
mylocation <- "~/OneDrive - University of Glasgow/M7 Gannet AI urgency/DATA/Data breeding 2022"
root = mylocation  
setwd(root)
files = list.files(root, pattern = ".pos")

# get Tag Id from file name
myfiles <- list.files(path=root)
tags <- sub(".*Tag","",files)
tags <- sub("_.*","",tags)

# append column names & metadata
n<-length(files)
tracklist = vector('list', n) # to store the data

for (i in 1:length(files)){
  dat = read.table(files[i], sep = ",",header=F,skip=5)
  cols<-c("Day","Month","Year","Hour","Min","Sec","Sec_of_day","Sats","Latitude","Longitude","Altitude","clock_offset","acccuracy_ind","BatteryVoltage","pressure_mbar","Temperature")
  colnames(dat)[1:16]<-cols
  
  dat<-subset(dat,dat$Latitude!=0 & dat$Altitude!=0) # get rid of Pathtrack data with no observation, no risk as no animal migrated further than western Sahara
  dat$Date<-paste(dat$Day,dat$Month,dat$Year,sep="-")
  dat$Time<-paste(dat$Hour,dat$Min,dat$Sec, sep=":")
  GMTDT<-paste(dat$Date,dat$Time)
  dat$GMT <- as.POSIXct(strptime(as.character(GMTDT), "%d- %m- %y %H:%M:%S"),"GMT")  # stored as vector in secs passed since 1 Jan 1970 1 am
  dat$TagID <- rep(tags[i],nrow(dat))
  dat$Tagtype<-rep("PATHTRACK",nrow(dat))
  tracklist[[i]]<-data.frame(dat)
}


# return as data frame

ptracks22<-plyr::ldply(tracklist)
rm(tracklist)
rm(dat,tags,files)

ptracks22$sensor <- "GPS"
ptracks22$TagID <- as.integer(ptracks22$TagID)

# make spatial object
t_locs <- st_as_sf(ptracks22, coords = c("Longitude", "Latitude"))
st_crs(t_locs) <- 4326


### Processing ---

## crop to capture date
setwd(root)

cap <- read.csv("Capture data August 2022.csv", header = T)

# convert BST to UTC
cap$GMT <- lubridate::dmy_hms(paste(cap$Date,cap$Time_capture))
lubridate::tz(cap$GMT) <- "Europe/London"
cap$GMT <- lubridate::with_tz(cap$GMT, tz = "UTC")
cap$TagID <- cap$Tag_ID


# crop to capture date & time & remove duplicates
# duplicates can occurr if server downloads data twice or during processing
tags <- unique(t_locs$TagID)

for (i in 1:length(tags)){
  subdat1 <- t_locs %>% filter(TagID == tags[i])
  subcap <- cap %>% filter(Tag_ID == tags[i])
  subdat <- subdat1 %>% filter(GMT >= subcap$GMT)
  
  subdat <- subdat[order(subdat$GMT),]
  dups <- which(duplicated(subdat$GMT)==TRUE)
  if (length(dups) == 0) subkeep <- subdat else {
    subkeep <- subdat[-dups, ] } # omit duplicates
  
  if (i ==1) tdat <- subkeep else tdat <- rbind(tdat,subkeep)
  print(i)
}

rm(subdat,subdat1, subcap, subkeep,dups)

# double check if duplicates removed
getDuplicatedTimestamps(x=as.factor(tdat$TagID), 
                        timestamps=as.POSIXct(tdat$GMT, 
                                              format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                        sensorType=tdat$sensor)

# append capture information to tracking data

t_locs <- full_join(t_locs,cap_current,by = "TagID")
t_locs$TagID <- as.factor(t_locs$TagID)

### Quick plotting---

# quick plot
# https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html#basemaps-and-overlay-tile-maps
tmap_mode("view")
tm_basemap("OpenTopoMap") +
  tm_shape(t_locs) + tm_dots(col = "TagID", palette = "viridis")
