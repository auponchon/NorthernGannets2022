library(here)
library(tidyverse)
library(wesanderson)
library(MetBrewer)
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

#bassrock for nest attendance 2022
load(here("data","NewlyCreatedData","rawLocBassrock.Rdata"))
land.bassrock22<-rawLoc.bassrock %>% 
  land_summary_ind(.,colony=colo_coord_bassrock) %>% 
  dplyr::filter(LandDurh > 1) 

#bassrock for nest attendance 2019
load(here("data","NewlyCreatedData","rawLocBassrock2019.Rdata"))
land.bassrock19<-rawLoc.bass2019 %>% 
  land_summary_ind(.,colony=colo_coord_bassrock) %>% 
  dplyr::filter(LandDurh > 1) 

#rouzic for trips 2022
load(here("Data","NewlyCreatedData","loc_clean_interp_rouzic.RData"))
trips.rouzic2022<-trips_summary_ind(loc.interp.rouzic,
                                colony=colo_coord_rouzic)

#rouzic for nest attendance 2022
load(here("data","NewlyCreatedData","rawLocRouzic.Rdata"))
land.rouzic22<-rawLoc.rouzic %>% 
  land_summary_ind(.,colony=colo_coord_rouzic) %>% 
  dplyr::filter(LandDurh > 1 & id!="222278") 


#rouzic for nest attendance 2022
load(here("data","NewlyCreatedData","rawLocRouzic2019.Rdata"))
land.rouzic19<-rawLoc.rouzic2019 %>% 
  land_summary_ind(.,colony=colo_coord_rouzic) %>% 
  dplyr::filter(LandDurh > 1) 


#bassrock for trips 2019
load(here("Data","NewlyCreatedData","loc_clean_interp_rouzic2019.RData"))
trips.rouzic2019<-trips_summary_ind(loc.clean.interp.rouz19,
                                      colony=colo_coord_rouzic)

#grassholm for trips 2022
load(here("Data","NewlyCreatedData","loc_clean_interp_grassholm.RData"))
trips.grass2022<-trips_summary_ind(loc.interp.grassholm,
                               colony=colo_coord_grassholm)
#grassholm for nest attendance 2022
load(here("data","NewlyCreatedData","rawLocGrassholm.Rdata"))
land.grass22<-rawLoc.grassholm %>% 
  land_summary_ind(.,colony=colo_coord_grassholm) %>% 
  dplyr::filter(LandDurh > 1 & id!="9006") 

#grassholm for nest attendance 2019
# load(here("data","NewlyCreatedData","rawLocGrassholm2019.Rdata"))
# land.grass19<-rawLoc.grass2019 %>% 
#   land_summary_ind(.,colony=colo_coord_grassholm) %>% 
#   dplyr::filter(LandDurh > 1) 

#grassholm for trips 2019
load(here("Data","NewlyCreatedData","loc_clean_interp_grassholm2019.RData"))
trips.grassholm2019<-trips_summary_ind(loc.clean.interp.grass19,
                                    colony=colo_coord_grassholm)

#calculate day number for all 3 colonies in 2019
trips2022<-trips.bassrock2022 %>% 
    rbind(trips.rouzic2022, trips.grass2022) %>% 
    mutate(day=as.numeric(as.Date(DateEnd) - as.Date("2022-06-20")),
           Year=as.factor(year(DateEnd)))



#calculate day number for all 3 colonies in 2019
trips2019<-trips.bassrock2019 %>% 
    rbind(trips.rouzic2019,trips.grassholm2019) %>% 
    mutate(day=as.numeric(as.Date(DateEnd) - as.Date("2019-06-20")),
           Year=as.factor(year(DateEnd)))

#calculate day number for all 3 colonies
lands19<-land.bassrock19 %>% 
  rbind(land.rouzic19 ) %>% 
  mutate(day=as.numeric(as.Date(DateEnd) - as.Date("2019-06-20")))

#calculate day number for all 3 colonies
lands22<-land.bassrock22 %>% 
  rbind(land.rouzic22,land.grass22 ) %>% 
  mutate(day=as.numeric(as.Date(DateEnd) - as.Date("2022-06-20")))


lands<-lands19 %>% 
  rbind(lands22) %>% 
  dplyr::mutate(Year=factor(as.factor(year(DateEnd)),
                            levels=c("2019","2022")))

trips<-rbind(trips2022,trips2019)

trips$Year<-factor(trips$Year,levels=c("2019","2022"))

trips.bass<-trips %>% 
    dplyr::filter(site=="Bass Rock")
trips.rouz<-trips %>% 
    dplyr::filter(site=="Rouzic")
trips.grass<-trips %>% 
    dplyr::filter(site=="Grassholm")


lands.bass<-lands %>% 
  dplyr::filter(site=="Bass Rock")
lands.rouz<-lands %>% 
  dplyr::filter(site=="Rouzic")
# lands.grass<-lands %>% 
#   dplyr::filter(site=="Grassholm")

# 
# mycol<-met.brewer(6, name="Signac",type="discrete")
# 
# 
# 
# newdatrouz<-expand.grid(site=unique(trips.rouz$site),
#                         day=seq(min(trips.rouz$day),
#                                 max(trips.rouz$day),1),
#                         year=as.factor(unique(trips.rouz$year)))
# 
# newdatbass<-expand.grid(site=unique(trips.bass$site),
#                         day=seq(min(trips.bass$day),
#                                 max(trips.bass$day),1),
#                         year=as.factor(unique(trips.bass$year)))
# 
# newdatgrass<-expand.grid(site=unique(trips.grass$site),
#                          day=seq(min(trips.grass$day),
#                                  max(trips.grass$day),1),
#                          year=as.factor(unique(trips.grass$year)))
# 
# #plot with separate datasets maximal distance to the colony
# glmm.dist.lme.rouz<-lme(log(Distmaxkm) ~ day + year, 
#                           random=~1 | id , 
#                           data=trips.rouz)
# 
# glmm.dist.lme.bass<-lme(log(Distmaxkm) ~ day + year, 
#                             random=~1 | id , 
#                             data=trips.bass)
# 
# glmm.dist.lme.grass<-lme(log(Distmaxkm) ~ day + year, 
#                          random=~1 | id , 
#                          data=trips.grass)
# 
# newdatrouz$pred<-predict(glmm.dist.lme.rouz, 
#                          newdata=newdatrouz,level=0)
# newdatbass$pred<-predict(glmm.dist.lme.bass, 
#                          newdata=newdatbass,level=0)
# newdatgrass$pred<-predict(glmm.dist.lme.grass, 
#                           newdata=newdatgrass,level=0)
# grouz <- ref_grid(glmm.dist.lme.rouz, 
#                   cov.keep= c('day','year'))
# gbass <- ref_grid(glmm.dist.lme.bass, 
#                   cov.keep= c('day','year'))
# ggrass <- ref_grid(glmm.dist.lme.grass, 
#                    cov.keep= c('day','year'))
# emmrouz <- data.frame(emmeans(grouz, 
#                               spec= c('day','year'), 
#                               level= 0.95),
#                       site="Rouzic")
# emmbass <- data.frame(emmeans(gbass, 
#                               spec= c('day','year'),
#                               level= 0.95),
#                       site="Bass Rock")
# emmgrass <- data.frame(emmeans(ggrass, 
#                                spec= c('day','year'),
#                                level= 0.95),
#                        site="Grassholm")
# 
# 
# newdatall<-rbind(newdatbass,newdatrouz,newdatgrass)
# emm<-rbind(emmbass,emmrouz,emmgrass)
# 
# 
# trips$Inter<-interaction(trips$year,trips$site)
# newdatall$Inter<-interaction(newdatall$year,newdatall$site)
# emm$Inter<-interaction(emm$year,emm$site)
# 
# distgg<-ggplot(trips, aes(x=day, y=Distmaxkm),group=Inter) + 
#     geom_point(aes(color=Inter, shape=year),
#                alpha=0.6, size=1.5) +
#     geom_line(data=newdatall,aes(x=day,y=exp(pred),color=Inter,
#                                  linetype=year),lwd=1.5) +
#      # geom_ribbon(data= data.frame(emm),
#      #            aes(ymin= exp(lower.CL), ymax= exp(upper.CL), y= NULL,
#      #                fill=Inter),
#      #            alpha=0.4,colour = NA) +
#     labs(y="Maximal distance to the colony (km)",tag="a)",x="") +
#     # scale_x_continuous(limits=c(0,65),
#     #                  #  labels=daysx,
#     #                    breaks=seq(0,65,10),
#     #                    expand=c(0.001,0.05)) +
#     scale_y_continuous(limits=c(0,700,100),
#                        breaks=seq(0,700,100),
#                        expand=c(0.01,0)) +
#     scale_colour_manual(values=mycol) +
#     scale_fill_manual(values=mycol)  +
#     theme_light() +
#     theme(legend.position="none") 
# print(distgg) 
# 
# 
# 
# #plot with separate datasets maximal distance to the colony
# glmm.dur.lme.rouz<-lme(log(TripDurh) ~ day + year, 
#                         random=~1 | id , 
#                         data=trips.rouz)
# 
# glmm.dur.lme.bass<-lme(log(TripDurh) ~ day + year, 
#                         random=~1 | id , 
#                         data=trips.bass)
# 
# glmm.dur.lme.grass<-lme(log(TripDurh) ~ day + year, 
#                          random=~1 | id , 
#                          data=trips.grass)
# 
# newdatrouz$pred<-predict(glmm.dur.lme.rouz, 
#                          newdata=newdatrouz,level=0)
# newdatbass$pred<-predict(glmm.dur.lme.bass, 
#                          newdata=newdatbass,level=0)
# newdatgrass$pred<-predict(glmm.dur.lme.grass, 
#                           newdata=newdatgrass,level=0)
# grouz <- ref_grid(glmm.dur.lme.rouz, 
#                   cov.keep= c('day','year'))
# gbass <- ref_grid(glmm.dur.lme.bass, 
#                   cov.keep= c('day','year'))
# ggrass <- ref_grid(glmm.dur.lme.grass, 
#                    cov.keep= c('day','year'))
# emmrouz <- data.frame(emmeans(grouz, 
#                               spec= c('day','year'), 
#                               level= 0.95),
#                       site="Rouzic")
# emmbass <- data.frame(emmeans(gbass, 
#                               spec= c('day','year'),
#                               level= 0.95),
#                       site="Bass Rock")
# emmgrass <- data.frame(emmeans(ggrass, 
#                                spec= c('day','year'),
#                                level= 0.95),
#                        site="Grassholm")
# 
# 
# newdatall<-rbind(newdatbass,newdatrouz,newdatgrass)
# emm<-rbind(emmbass,emmrouz,emmgrass)
# 
# newdatall$Inter<-interaction(newdatall$year,newdatall$site)
# emm$Inter<-interaction(emm$year,emm$site)
# 
# durgg<-ggplot(trips, aes(x=day, y=TripDurh),group=Inter) + 
#   geom_point(aes(color=Inter, shape=year),
#              alpha=0.6, size=1.5) +
#   geom_line(data=newdatall,aes(x=day,y=exp(pred),color=Inter,
#                                linetype=year),lwd=1.5) +
#   # geom_ribbon(data= data.frame(emm),
#   #             aes(ymin= exp(lower.CL), ymax= exp(upper.CL), y= NULL,
#   #                 fill=Inter),
#   #             alpha=0.4,colour = NA) +
#   labs(y="Trip duration (h)",tag="b)",x="") +
#   # scale_x_continuous(limits=c(0,65),
#   #                  #  labels=daysx,
#   #                    breaks=seq(0,65,10),
#   #                    expand=c(0.001,0.05)) +
#   scale_y_continuous(limits=c(0,200,50),
#                      breaks=seq(0,200,50),
#                      expand=c(0.01,0)) +
#   scale_colour_manual(values=mycol) +
#   scale_fill_manual(values=mycol)  +
#   theme_light() +
# theme(legend.position="none") 
# print(durgg) 
# 
# 
# 
# 
# #plot with separate datasets maximal distance to the colony
# glmm.totdist.lme.rouz<-lme(log(TotalPathkm) ~ day + year, 
#                        random=~1 | id , 
#                        data=trips.rouz)
# 
# glmm.totdist.lme.bass<-lme(log(TotalPathkm) ~ day + year, 
#                        random=~1 | id , 
#                        data=trips.bass)
# 
# glmm.totdist.lme.grass<-lme(log(TotalPathkm) ~ day + year, 
#                         random=~1 | id , 
#                         data=trips.grass)
# 
# newdatrouz$pred<-predict(glmm.totdist.lme.rouz, 
#                          newdata=newdatrouz,level=0)
# newdatbass$pred<-predict(glmm.totdist.lme.bass, 
#                          newdata=newdatbass,level=0)
# newdatgrass$pred<-predict(glmm.totdist.lme.grass, 
#                           newdata=newdatgrass,level=0)
# grouz <- ref_grid(glmm.totdist.lme.rouz, 
#                   cov.keep= c('day','year'))
# gbass <- ref_grid(glmm.totdist.lme.bass, 
#                   cov.keep= c('day','year'))
# ggrass <- ref_grid(glmm.totdist.lme.grass, 
#                    cov.keep= c('day','year'))
# emmrouz <- data.frame(emmeans(grouz, 
#                               spec= c('day','year'), 
#                               level= 0.95),
#                       site="Rouzic")
# emmbass <- data.frame(emmeans(gbass, 
#                               spec= c('day','year'),
#                               level= 0.95),
#                       site="Bass Rock")
# emmgrass <- data.frame(emmeans(ggrass, 
#                                spec= c('day','year'),
#                                level= 0.95),
#                        site="Grassholm")
# 
# 
# newdatall<-rbind(newdatbass,newdatrouz,newdatgrass)
# emm<-rbind(emmbass,emmrouz,emmgrass)
# 
# 
# trips$Inter<-interaction(trips$year,trips$site)
# newdatall$Inter<-interaction(newdatall$year,newdatall$site)
# emm$Inter<-interaction(emm$year,emm$site)
# 
# totdistgg<-ggplot(trips, aes(x=day, y=TotalPathkm),group=Inter) + 
#   geom_point(aes(color=Inter, shape=year),
#              alpha=0.6,size=1.5) +
#   geom_line(data=newdatall,aes(x=day,y=exp(pred),color=Inter,
#                                linetype=year),lwd=1.5) +
#   # geom_ribbon(data= data.frame(emm),
#   #             aes(ymin= exp(lower.CL), ymax= exp(upper.CL), y= NULL,
#   #                 fill=Inter),
#   #             alpha=0.4,colour = NA) +
#   labs(y="Total distance travelled (km)",tag="c)",x="") +
#   # scale_x_continuous(limits=c(0,65),
#   #                  #  labels=daysx,
#   #                    breaks=seq(0,65,10),
#   #                    expand=c(0.001,0.05)) +
#   scale_y_continuous(limits=c(0,2000,500),
#                      breaks=seq(0,2000,500),
#                      expand=c(0.01,0)) +
#   scale_colour_manual(values=mycol) +
#   scale_fill_manual(values=mycol)  +
#   theme_light() 
# 
# 
# mylegend<-g_legend(totdistgg)
# 
# 
# print(totdistgg) 
# 
# tiff(here::here("outputs","lmm_time_year.tiff"),
#      width=4000,
#      height=3200,
#      res=300,
#      compression="lzw")
# grid.arrange(arrangeGrob(distgg,
#             durgg,
#             totdistgg+ theme(legend.position="none") ,
#             nrow=3),
#             mylegend, 
#             ncol=2,
#            widths=c(1,0.1)) 
# dev.off()


#mycol<-wes_palette(2, name="Moonrise2" )
mycol<-viridis(2,begin=0.05,end=0.25,option="A")
mycol<-c("orange","black")


#plot with separate datasets maximal distance to the colony
glmm.dist.lme.rouz<-lme(log(Distmaxkm) ~ Year, 
                        random=~1 | id , 
                        data=trips.rouz)

glmm.dist.lme.bass<-lme(log(Distmaxkm) ~ Year, 
                        random=~1 | id , 
                        data=trips.bass)

glmm.dist.lme.grass<-lme(log(Distmaxkm) ~ Year, 
                         random=~1 | id , 
                         data=trips.grass)

grouz <- ref_grid(glmm.dist.lme.rouz, 
                  cov.keep= c('Year'))
gbass <- ref_grid(glmm.dist.lme.bass, 
                  cov.keep= c('Year'))
ggrass <- ref_grid(glmm.dist.lme.grass, 
                   cov.keep= c('Year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('Year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('Year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('Year'),
                               level= 0.95),
                       site="Grassholm")

emm<-rbind(emmbass,emmrouz,emmgrass)


dist<-ggplot(emm,aes(x=site,y=exp(emmean)), group=Year ) +
  geom_jitter(data=trips, aes(x=site,y=Distmaxkm,color=Year),
              alpha=0.2,
              width=0.1,
              shape=16,
              show.legend=F) +
  geom_point(aes(x=site,y=exp(emmean),color=Year),
             size=2,
             shape=15,
             show.legend = F) +
  geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                     ymax=exp(upper.CL),
                     color=Year),
                width=.2) +
  scale_y_continuous(limits=c(0,700),
                     breaks=seq(0,700,100),
                     expand=c(0.01,0.01))+
  labs(y="Maximal distance to the colony (km)",tag="a)",x="") +
  scale_colour_manual(values=mycol) +
  theme_light() +
  theme(legend.position="none",
        panel.grid.minor = element_blank()) 

print(dist)


## Duration
#plot with separate datasets maximal distance to the colony
glmm.dur.lme.rouz<-lme(log(TripDurh) ~ Year, 
                       random=~1 | id , 
                       data=trips.rouz)

glmm.dur.lme.bass<-lme(log(TripDurh) ~ Year, 
                       random=~1 | id , 
                       data=trips.bass)

glmm.dur.lme.grass<-lme(log(TripDurh) ~ Year, 
                        random=~1 | id , 
                        data=trips.grass)

grouz <- ref_grid(glmm.dur.lme.rouz, 
                  cov.keep= c('Year'))
gbass <- ref_grid(glmm.dur.lme.bass, 
                  cov.keep= c('Year'))
ggrass <- ref_grid(glmm.dur.lme.grass, 
                   cov.keep= c('Year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('Year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('Year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('Year'),
                               level= 0.95),
                       site="Grassholm")

emm<-rbind(emmbass,emmrouz,emmgrass)


dur<-ggplot(emm,aes(x=site,y=exp(emmean)), group=Year ) +
  geom_jitter(data=trips, aes(x=site,y=TripDurh,color=Year),
              alpha=0.2,
              width=0.1,
              shape=16,
              show.legend=F) +
  geom_point(aes(x=site,y=exp(emmean),color=Year),
             size=2,
             shape=15,
             show.legend = F) +
  geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                     ymax=exp(upper.CL),
                     color=Year),
                width=.2) +
  scale_y_continuous(limits=c(0,200,50),
                     breaks=seq(0,200,50),
                     expand=c(0.01,0)) +
  labs(y="Trip duration (h)",tag="b)",x="") +
  scale_colour_manual(values=mycol) +
  theme_light() +
  theme(legend.position="none",
        panel.grid.minor = element_blank()) 

print(dur)




glmm.totdist.lme.rouz<-lme(log(TotalPathkm) ~ Year, 
                           random=~1 | id , 
                           data=trips.rouz)

glmm.totdist.lme.bass<-lme(log(TotalPathkm) ~ Year, 
                           random=~1 | id , 
                           data=trips.bass)

glmm.totdist.lme.grass<-lme(log(TotalPathkm) ~ Year, 
                            random=~1 | id , 
                            data=trips.grass)

grouz <- ref_grid(glmm.totdist.lme.rouz, 
                  cov.keep= c('Year'))
gbass <- ref_grid(glmm.totdist.lme.bass, 
                  cov.keep= c('Year'))
ggrass <- ref_grid(glmm.totdist.lme.grass, 
                   cov.keep= c('Year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('Year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('Year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('Year'),
                               level= 0.95),
                       site="Grassholm")

emm<-rbind(emmbass,emmrouz,emmgrass)


totdist<-ggplot(emm,aes(x=site,y=exp(emmean)), group=Year ) +
  geom_jitter(data=trips, aes(x=site,y=TotalPathkm,color=Year),
              alpha=0.2,
              width=0.1,
              shape=16,
              show.legend=F) +
  geom_point(aes(x=site,y=exp(emmean),color=Year),
             size=2,
             shape=15) +
  geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                     ymax=exp(upper.CL),
                     color=Year),
                width=.2,
                show.legend=F) +
  scale_y_continuous(limits=c(0,2000,500),
                     breaks=seq(0,2000,500),
                     expand=c(0.01,0)) +
  labs(y="Total distance travelled (km)",
       tag="c)",
       x="") +
  scale_colour_manual(values=mycol) +
  theme_light() +
   theme(legend.position="none",,
         panel.grid.minor = element_blank()) 

print(totdist)

## Nest attendance
#plot with separate datasets maximal distance to the colony
glmm.land.lme.rouz<-lme(log(LandDurh) ~ Year, 
                        random=~1 | id , 
                        data=lands.rouz)

glmm.land.lme.bass<-lme(log(LandDurh) ~ Year, 
                        random=~1 | id , 
                        data=lands.bass)


# glmm.land.lme.grass<-lme(log(LandDurh) ~ Year, 
#                         random=~1 | id , 
#                         data=lands.grass)

grouz <- ref_grid(glmm.land.lme.rouz, 
                  cov.keep= c('Year'))
gbass <- ref_grid(glmm.land.lme.bass, 
                  cov.keep= c('Year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('Year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('Year'),
                              level= 0.95),
                      site="Bass Rock")

emm<-rbind(emmbass,emmrouz)


landdur<-ggplot(emm,aes(x=site,y=exp(emmean)), group=Year ) +
  geom_jitter(data=lands, aes(x=site,y=LandDurh,color=Year),
              alpha=0.2,
              width=0.1,
              shape=16) +
  geom_point(aes(x=site,y=exp(emmean),color=Year),
             size=2,
             shape=15) +
  geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                     ymax=exp(upper.CL),
                     color=Year),
                width=.2,
                show.legend=F) +
  scale_y_continuous(limits=c(0,100,20),
                     breaks=seq(0,100,20),
                     expand=c(0.01,0)) +
  labs(y="Time attending the colony (h)",
       tag="d)",
       x="") +
  scale_colour_manual(values=mycol) +
  theme_light() +
  guides(shape=guide_legend(title="Year")) +
theme(panel.grid.minor = element_blank())
print(landdur)

mylegend<-g_legend(landdur)


tiff(here::here("outputs","Figures finales","lmm_year_2022_2019.tiff"),
     width=6000,
     height=3500,
     res=600,
     compression="lzw")
grid.arrange(arrangeGrob(dist,
                         dur,
                         totdist,
                          landdur+
                          theme(legend.position="none") ,
                         nrow=2, ncol=2),
             mylegend, 
             ncol=2,
             widths=c(1,0.1)) 
dev.off()

