library(here)
library(tidyverse)
library(wesanderson)
library(gridExtra)
library(nlme)
library(emmeans)
library(DHARMa)
library(plotrix)

source(here("R","trip_functions.R"))

#bassrock for trips 2022
load(here("Data","NewlyCreatedData","loc_clean_interp_bassrock.RData"))
trips.bassrock2022<-trips_summary_ind(loc.interp.bassrock,
                                  colony=colo_coord_bassrock)

#bassrock for trips 2019
load(here("Data","NewlyCreatedData","loc_clean_interp_bassrock2019.RData"))
trips.bassrock2019<-trips_summary_ind(loc.clean.interp.bass19,
                                      colony=colo_coord_bassrock)

#rouzic for trips 2022
load(here("Data","NewlyCreatedData","loc_clean_interp_rouzic.RData"))
trips.rouzic2022<-trips_summary_ind(loc.interp.rouzic,
                                colony=colo_coord_rouzic)

#bassrock for trips 2019
load(here("Data","NewlyCreatedData","loc_clean_interp_rouzic2019.RData"))
trips.rouzic2019<-trips_summary_ind(loc.clean.interp.rouz19,
                                      colony=colo_coord_rouzic)

#grassholm for trips 2022
load(here("Data","NewlyCreatedData","loc_clean_interp_grassholm.RData"))
trips.grass2022<-trips_summary_ind(loc.interp.grassholm,
                               colony=colo_coord_grassholm)

#grassholm for trips 2019
load(here("Data","NewlyCreatedData","loc_clean_interp_grassholm2019.RData"))
trips.grassholm2019<-trips_summary_ind(loc.clean.interp.grass19,
                                    colony=colo_coord_grassholm)

#calculate day number for all 3 colonies in 2019
trips2022<-trips.bassrock2022 %>% 
    rbind(trips.rouzic2022, trips.grass2022) %>% 
    mutate(day=as.numeric(as.Date(DateEnd) - as.Date("2022-06-20")),
           year=as.factor(year(DateEnd)))


#calculate day number for all 3 colonies in 2019
trips2019<-trips.bassrock2019 %>% 
    rbind(trips.rouzic2019, trips.grassholm2019) %>% 
    mutate(day=as.numeric(as.Date(DateEnd) - as.Date("2019-06-20")),
           year=as.factor(year(DateEnd)))


trips<-rbind(trips2022,trips2019)

trips$year<-factor(trips$year,levels=c("2019","2022"))

trips.bass<-trips %>% 
    dplyr::filter(site=="Bass Rock")
trips.rouz<-trips %>% 
    dplyr::filter(site=="Rouzic")
trips.grass<-trips %>% 
    dplyr::filter(site=="Grassholm")



mycol<-met.brewer(6, name="Signac",type="discrete")



newdatrouz<-expand.grid(site=unique(trips.rouz$site),
                        day=seq(min(trips.rouz$day),
                                max(trips.rouz$day),1),
                        year=as.factor(unique(trips.rouz$year)))

newdatbass<-expand.grid(site=unique(trips.bass$site),
                        day=seq(min(trips.bass$day),
                                max(trips.bass$day),1),
                        year=as.factor(unique(trips.bass$year)))

newdatgrass<-expand.grid(site=unique(trips.grass$site),
                         day=seq(min(trips.grass$day),
                                 max(trips.grass$day),1),
                         year=as.factor(unique(trips.grass$year)))

#plot with separate datasets maximal distance to the colony
glmm.dist.lme.rouz<-lme(log(Distmaxkm) ~ day + year, 
                          random=~1 | id , 
                          data=trips.rouz)

glmm.dist.lme.bass<-lme(log(Distmaxkm) ~ day + year, 
                            random=~1 | id , 
                            data=trips.bass)

glmm.dist.lme.grass<-lme(log(Distmaxkm) ~ day + year, 
                         random=~1 | id , 
                         data=trips.grass)

newdatrouz$pred<-predict(glmm.dist.lme.rouz, 
                         newdata=newdatrouz,level=0)
newdatbass$pred<-predict(glmm.dist.lme.bass, 
                         newdata=newdatbass,level=0)
newdatgrass$pred<-predict(glmm.dist.lme.grass, 
                          newdata=newdatgrass,level=0)
grouz <- ref_grid(glmm.dist.lme.rouz, 
                  cov.keep= c('day','year'))
gbass <- ref_grid(glmm.dist.lme.bass, 
                  cov.keep= c('day','year'))
ggrass <- ref_grid(glmm.dist.lme.grass, 
                   cov.keep= c('day','year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('day','year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('day','year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('day','year'),
                               level= 0.95),
                       site="Grassholm")


newdatall<-rbind(newdatbass,newdatrouz,newdatgrass)
emm<-rbind(emmbass,emmrouz,emmgrass)


trips$Inter<-interaction(trips$year,trips$site)
newdatall$Inter<-interaction(newdatall$year,newdatall$site)
emm$Inter<-interaction(emm$year,emm$site)

distgg<-ggplot(trips, aes(x=day, y=Distmaxkm),group=Inter) + 
    geom_point(aes(color=Inter, shape=year),
               alpha=0.6, size=1.5) +
    geom_line(data=newdatall,aes(x=day,y=exp(pred),color=Inter,
                                 linetype=year),lwd=1.5) +
     # geom_ribbon(data= data.frame(emm),
     #            aes(ymin= exp(lower.CL), ymax= exp(upper.CL), y= NULL,
     #                fill=Inter),
     #            alpha=0.4,colour = NA) +
    labs(y="Maximal distance to the colony (km)",tag="a)",x="") +
    # scale_x_continuous(limits=c(0,65),
    #                  #  labels=daysx,
    #                    breaks=seq(0,65,10),
    #                    expand=c(0.001,0.05)) +
    scale_y_continuous(limits=c(0,700,100),
                       breaks=seq(0,700,100),
                       expand=c(0.01,0)) +
    scale_colour_manual(values=mycol) +
    scale_fill_manual(values=mycol)  +
    theme_light() +
    theme(legend.position="none") 
print(distgg) 



#plot with separate datasets maximal distance to the colony
glmm.dur.lme.rouz<-lme(log(TripDurh) ~ day + year, 
                        random=~1 | id , 
                        data=trips.rouz)

glmm.dur.lme.bass<-lme(log(TripDurh) ~ day + year, 
                        random=~1 | id , 
                        data=trips.bass)

glmm.dur.lme.grass<-lme(log(TripDurh) ~ day + year, 
                         random=~1 | id , 
                         data=trips.grass)

newdatrouz$pred<-predict(glmm.dur.lme.rouz, 
                         newdata=newdatrouz,level=0)
newdatbass$pred<-predict(glmm.dur.lme.bass, 
                         newdata=newdatbass,level=0)
newdatgrass$pred<-predict(glmm.dur.lme.grass, 
                          newdata=newdatgrass,level=0)
grouz <- ref_grid(glmm.dur.lme.rouz, 
                  cov.keep= c('day','year'))
gbass <- ref_grid(glmm.dur.lme.bass, 
                  cov.keep= c('day','year'))
ggrass <- ref_grid(glmm.dur.lme.grass, 
                   cov.keep= c('day','year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('day','year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('day','year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('day','year'),
                               level= 0.95),
                       site="Grassholm")


newdatall<-rbind(newdatbass,newdatrouz,newdatgrass)
emm<-rbind(emmbass,emmrouz,emmgrass)

newdatall$Inter<-interaction(newdatall$year,newdatall$site)
emm$Inter<-interaction(emm$year,emm$site)

durgg<-ggplot(trips, aes(x=day, y=TripDurh),group=Inter) + 
  geom_point(aes(color=Inter, shape=year),
             alpha=0.6, size=1.5) +
  geom_line(data=newdatall,aes(x=day,y=exp(pred),color=Inter,
                               linetype=year),lwd=1.5) +
  # geom_ribbon(data= data.frame(emm),
  #             aes(ymin= exp(lower.CL), ymax= exp(upper.CL), y= NULL,
  #                 fill=Inter),
  #             alpha=0.4,colour = NA) +
  labs(y="Trip duration (h)",tag="b)",x="") +
  # scale_x_continuous(limits=c(0,65),
  #                  #  labels=daysx,
  #                    breaks=seq(0,65,10),
  #                    expand=c(0.001,0.05)) +
  scale_y_continuous(limits=c(0,200,50),
                     breaks=seq(0,200,50),
                     expand=c(0.01,0)) +
  scale_colour_manual(values=mycol) +
  scale_fill_manual(values=mycol)  +
  theme_light() +
theme(legend.position="none") 
print(durgg) 




#plot with separate datasets maximal distance to the colony
glmm.totdist.lme.rouz<-lme(log(TotalPathkm) ~ day + year, 
                       random=~1 | id , 
                       data=trips.rouz)

glmm.totdist.lme.bass<-lme(log(TotalPathkm) ~ day + year, 
                       random=~1 | id , 
                       data=trips.bass)

glmm.totdist.lme.grass<-lme(log(TotalPathkm) ~ day + year, 
                        random=~1 | id , 
                        data=trips.grass)

newdatrouz$pred<-predict(glmm.totdist.lme.rouz, 
                         newdata=newdatrouz,level=0)
newdatbass$pred<-predict(glmm.totdist.lme.bass, 
                         newdata=newdatbass,level=0)
newdatgrass$pred<-predict(glmm.totdist.lme.grass, 
                          newdata=newdatgrass,level=0)
grouz <- ref_grid(glmm.totdist.lme.rouz, 
                  cov.keep= c('day','year'))
gbass <- ref_grid(glmm.totdist.lme.bass, 
                  cov.keep= c('day','year'))
ggrass <- ref_grid(glmm.totdist.lme.grass, 
                   cov.keep= c('day','year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('day','year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('day','year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('day','year'),
                               level= 0.95),
                       site="Grassholm")


newdatall<-rbind(newdatbass,newdatrouz,newdatgrass)
emm<-rbind(emmbass,emmrouz,emmgrass)


trips$Inter<-interaction(trips$year,trips$site)
newdatall$Inter<-interaction(newdatall$year,newdatall$site)
emm$Inter<-interaction(emm$year,emm$site)

totdistgg<-ggplot(trips, aes(x=day, y=TotalPathkm),group=Inter) + 
  geom_point(aes(color=Inter, shape=year),
             alpha=0.6,size=1.5) +
  geom_line(data=newdatall,aes(x=day,y=exp(pred),color=Inter,
                               linetype=year),lwd=1.5) +
  # geom_ribbon(data= data.frame(emm),
  #             aes(ymin= exp(lower.CL), ymax= exp(upper.CL), y= NULL,
  #                 fill=Inter),
  #             alpha=0.4,colour = NA) +
  labs(y="Total distance travelled (km)",tag="c)",x="") +
  # scale_x_continuous(limits=c(0,65),
  #                  #  labels=daysx,
  #                    breaks=seq(0,65,10),
  #                    expand=c(0.001,0.05)) +
  scale_y_continuous(limits=c(0,2000,500),
                     breaks=seq(0,2000,500),
                     expand=c(0.01,0)) +
  scale_colour_manual(values=mycol) +
  scale_fill_manual(values=mycol)  +
  theme_light() 


mylegend<-g_legend(totdistgg)


print(totdistgg) 

tiff(here::here("outputs","lmm_time_year.tiff"),
     width=4000,
     height=3200,
     res=300,
     compression="lzw")
grid.arrange(arrangeGrob(distgg,
            durgg,
            totdistgg+ theme(legend.position="none") ,
            nrow=3),
            mylegend, 
            ncol=2,
           widths=c(1,0.1)) 
dev.off()
