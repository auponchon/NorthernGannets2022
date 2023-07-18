set.seed(123)

mycol<-wes_palette(2, name="Moonrise2" )
mycol<-met.brewer(2, name="Homer1",type="discrete")


newdatrouz<-expand.grid(site=unique(trips.rouz$site),
                        year=as.factor(unique(trips.rouz$year)))

newdatbass<-expand.grid(site=unique(trips.bass$site),
                       year=as.factor(unique(trips.bass$year)))

newdatgrass<-expand.grid(site=unique(trips.grass$site),
                         year=as.factor(unique(trips.grass$year)))

#plot with separate datasets maximal distance to the colony
glmm.dist.lme.rouz<-lme(log(Distmaxkm) ~ year, 
                        random=~1 | id , 
                        data=trips.rouz)

glmm.dist.lme.bass<-lme(log(Distmaxkm) ~ year, 
                        random=~1 | id , 
                        data=trips.bass)

glmm.dist.lme.grass<-lme(log(Distmaxkm) ~ year, 
                         random=~1 | id , 
                         data=trips.grass)

grouz <- ref_grid(glmm.dist.lme.rouz, 
                  cov.keep= c('year'))
gbass <- ref_grid(glmm.dist.lme.bass, 
                  cov.keep= c('year'))
ggrass <- ref_grid(glmm.dist.lme.grass, 
                   cov.keep= c('year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('year'),
                               level= 0.95),
                       site="Grassholm")

emm<-rbind(emmbass,emmrouz,emmgrass)


dist<-ggplot(emm,aes(x=site,y=exp(emmean)), group=year ) +
    geom_jitter(data=trips, aes(x=site,y=Distmaxkm,color=year),
                alpha=0.2,
                width=0.1,
                shape=16,
                show.legend=F) +
    geom_point(aes(x=site,y=exp(emmean),color=year),
                size=2,
               shape=15,
               show.legend = F) +
    geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                          ymax=exp(upper.CL),
                          color=year),
                  width=.2) +
    scale_y_continuous(limits=c(0,700),
                       breaks=seq(0,700,100),
                       expand=c(0.01,0.01))+
    labs(y="Maximal distance to the colony (km)",tag="a)",x="") +
        scale_colour_manual(values=mycol) +
        theme_light() +
    theme(legend.position="none") 

print(dist)


## Duration
#plot with separate datasets maximal distance to the colony
glmm.dur.lme.rouz<-lme(log(TripDurh) ~ year, 
                        random=~1 | id , 
                        data=trips.rouz)

glmm.dur.lme.bass<-lme(log(TripDurh) ~ year, 
                        random=~1 | id , 
                        data=trips.bass)

glmm.dur.lme.grass<-lme(log(TripDurh) ~ year, 
                         random=~1 | id , 
                         data=trips.grass)

grouz <- ref_grid(glmm.dur.lme.rouz, 
                  cov.keep= c('year'))
gbass <- ref_grid(glmm.dur.lme.bass, 
                  cov.keep= c('year'))
ggrass <- ref_grid(glmm.dur.lme.grass, 
                   cov.keep= c('year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('year'),
                               level= 0.95),
                       site="Grassholm")

emm<-rbind(emmbass,emmrouz,emmgrass)


dur<-ggplot(emm,aes(x=site,y=exp(emmean)), group=year ) +
    geom_jitter(data=trips, aes(x=site,y=TripDurh,color=year),
                alpha=0.2,
                width=0.1,
                shape=16,
                show.legend=F) +
    geom_point(aes(x=site,y=exp(emmean),color=year),
               size=2,
               shape=15,
               show.legend = F) +
    geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                       ymax=exp(upper.CL),
                       color=year),
                  width=.2) +
    scale_y_continuous(limits=c(0,200,50),
                       breaks=seq(0,200,50),
                       expand=c(0.01,0)) +
    labs(y="Trip duration (h)",tag="b)",x="") +
    scale_colour_manual(values=mycol) +
    theme_light() +
    theme(legend.position="none") 

print(dur)


## Duration
#plot with separate datasets maximal distance to the colony
glmm.totdist.lme.rouz<-lme(log(TotalPathkm) ~ year, 
                       random=~1 | id , 
                       data=trips.rouz)

glmm.totdist.lme.bass<-lme(log(TotalPathkm) ~ year, 
                       random=~1 | id , 
                       data=trips.bass)

glmm.totdist.lme.grass<-lme(log(TotalPathkm) ~ year, 
                        random=~1 | id , 
                        data=trips.grass)

grouz <- ref_grid(glmm.totdist.lme.rouz, 
                  cov.keep= c('year'))
gbass <- ref_grid(glmm.totdist.lme.bass, 
                  cov.keep= c('year'))
ggrass <- ref_grid(glmm.totdist.lme.grass, 
                   cov.keep= c('year'))
emmrouz <- data.frame(emmeans(grouz, 
                              spec= c('year'), 
                              level= 0.95),
                      site="Rouzic")
emmbass <- data.frame(emmeans(gbass, 
                              spec= c('year'),
                              level= 0.95),
                      site="Bass Rock")
emmgrass <- data.frame(emmeans(ggrass, 
                               spec= c('year'),
                               level= 0.95),
                       site="Grassholm")

emm<-rbind(emmbass,emmrouz,emmgrass)


totdist<-ggplot(emm,aes(x=site,y=exp(emmean)), group=year ) +
    geom_jitter(data=trips, aes(x=site,y=TotalPathkm,color=year),
                alpha=0.2,
                width=0.1,
                shape=16,
                show.legend=F) +
    geom_point(aes(x=site,y=exp(emmean),color=year),
               size=2,
               shape=15) +
    geom_errorbar(aes( x=site,   ymin=exp(lower.CL),
                       ymax=exp(upper.CL),
                       color=year),
                  width=.2,
                  show.legend=F) +
    scale_y_continuous(limits=c(0,2000,500),
                       breaks=seq(0,2000,500),
                       expand=c(0.01,0)) +
    labs(y="Total distance travelled (km)",
         tag="c)",
         x="") +
    scale_colour_manual(values=mycol) +
    theme_light() 
#    theme(legend.position="bottom") 

print(totdist)

mylegend<-g_legend(totdist)


tiff(here::here("outputs","lmm_year.tiff"),
     width=3000,
     height=3000,
     res=300,
     compression="lzw")
grid.arrange(arrangeGrob(dist,
                         dur,
                         totdist+ theme(legend.position="none") ,
                         nrow=3),
             mylegend, 
             ncol=2,
             widths=c(1,0.1)) 
dev.off()

