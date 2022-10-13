library(tidyverse)
library(here)
library(sf)
library(crawl)
library(ggspatial)
library(xts)
library(pander)

source(here("R","crawl_functions.R"))

#WGS84 projection
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#Coordinates of Rouzix colony
esri_ocean <- paste0('https://services.arcgisonline.com/arcgis/rest/services/',
                     'Ocean/World_Ocean_Base/MapServer/tile/${z}/${y}/${x}.jpeg')



load(here("data","NewlyCreatedData","ind222269_loc_clean.RData"))

subso<-subso %>% 
    dplyr::filter(travelNb > 0)
lev<-unique(subso$id)

#for (a in 1:length(ids)){


loc.interp<-NULL
trip<-unique(subso$trip.id)


#loop through the trips
for (b in 1:length(trip)){
    print(trip[b])
    
    tbl_locs<-subso %>% 
           dplyr::filter(trip.id==trip[b]) 
       
    sf_locs <- tbl_locs %>% 
        dplyr::arrange(id,datetime) %>% 
        dplyr::group_by(id,datetime) %>% 
        dplyr::filter(difftimemin >=1) %>% 
        sf::st_as_sf(., coords = c("long","lat")) %>% 
        sf::st_set_crs(projcrs)
    
    
    sf_lines <- sf_locs %>% 
        dplyr::arrange(id, datetime) %>% 
        sf::st_geometry() %>% 
        sf::st_cast("MULTIPOINT",ids = as.integer(as.factor(sf_locs$id))) %>% 
        sf::st_cast("MULTILINESTRING") %>% 
        sf::st_sf(ID = as.factor(unique(sf_locs$id)))
    
   
    # ggplot() +
    #   annotation_map_tile(type = esri_ocean, zoomin = 10, progress = "none") +
    #   layer_spatial(sf_lines, size = 0.75,aes(color = ID)) +
    #   scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
    #   theme(legend.position = "none") +
    #   ggtitle("Observed Location Paths",
    #           subtitle = "Northern gannets")
    
    ##adding error measures
    sf_locs <- sf_locs %>%
        dplyr::mutate(
            error_semi_major_axis =  50,
            error_semi_minor_axis =  50,
            error_ellipse_orientation =  0)
 

    
  
    #transform to projected coords
    sf_locs <- sf::st_transform(sf_locs, 3395)
  #  sf::st_geometry(sf_locs)
    
    #devtools::install_github('NMML/crawl',ref='devel')
    #future::plan(multisession)
    sf_locs <- sf_locs %>% 
        dplyr::group_by(id) %>% dplyr::arrange(datetime) %>% 
        tidyr::nest() %>% 
        dplyr::mutate(data = furrr::future_map(data,sf::st_as_sf))
    
    #create ellipses around locations
    sf_locs <- sf_locs %>% 
        dplyr::mutate(
            diag = purrr::map(data, ~ crawl::argosDiag2Cov(
                .x$error_semi_major_axis, 
                .x$error_semi_minor_axis, 
                .x$error_ellipse_orientation)),
            data = purrr::map2(data,diag,bind_cols)) %>% 
        dplyr::select(-diag)
    

    sf_locs <- sf_locs %>% 
        dplyr::mutate(
            fixpar = rep(
                list(c(1,1,NA,NA)),
                nrow(.)
            )  )
    
  
    initial = list(a=c(colo_coord_rouzic$long[1],0,colo_coord_rouzic$lat[1],0),
                   P=diag(c(10000^2,5400^2,10000^2,5400^2)))
    
# fit<-    crwMLE(
#         mov.model=~1,
#         data=sf_locs, coord=c("long","lat"), Time.name="datetime", 
#         initial.state=initial, fixPar=fixPar, theta=c(rep(log(5000),3),log(3*3600), 0),
#         constr=constr,
#         control=list(maxit=2000, trace=1, REPORT=1)
#     )
#     
    
    tbl_locs_fit <- sf_locs %>% 
        dplyr::mutate(fit = furrr::future_pmap(list(d = data,fixpar = fixpar, seed=T),
                                               fit_crawl),
                      params = map(fit, crawl::tidy_crwFit))
    
    
    tbl_locs_fit <- tbl_locs_fit %>% 
        dplyr::mutate(predict = furrr::future_map(fit,
                                                  crawl::crwPredict,
                                                  predTime = '15 min')) 
    
    tbl_locs_fit <- tbl_locs_fit %>% 
        dplyr::mutate(sf_points = purrr::map(predict,
                                             crawl::crw_as_sf,
                                             ftype = "POINT",
                                             locType = "p"),
                      sf_line = purrr::map(predict,
                                           crawl::crw_as_sf,
                                           ftype = "LINESTRING",
                                           locType = "p")  )
    
    sf_pred_lines <- do.call(rbind,tbl_locs_fit$sf_line) %>% 
        mutate(id = tbl_locs_fit$id) 
    
    sf_pred_points <- tbl_locs_fit %>% tidyr::unnest(sf_points) %>% 
        mutate(geometry = sf::st_sfc(geometry)) %>% 
        sf::st_as_sf() %>% 
        sf::st_set_crs(3395)
    
    sf_locs_sp<-sf_locs$data[[1]] %>% 
        mutate(geometry = sf::st_sfc(geometry)) %>% 
        sf::st_as_sf() %>% 
        sf::st_set_crs(3395)
    
    ggplot() + 
        annotation_map_tile(type = esri_ocean,zoomin = 1,progress = "none") +
        layer_spatial(sf_lines, size = 1,color = "black") +
        layer_spatial(sf_pred_lines, size = 0.75, aes(color = "red")) +
        layer_spatial(sf_locs_sp,size=1.5,aes(color="cyan"),shape=16) +
        scale_x_continuous(expand = expansion(mult = c(.6, .6))) +
        scale_y_continuous(expand = expansion(mult = c(0.35, 0.35))) +
        theme(legend.position = "none") +
        ggtitle("Predicted Location Paths", 
                subtitle = paste( "ID", lev , "Trip", b ,sep=" "))
    
    
    ggsave(here("outputs","crawl",paste(lev, "_trip", b,".png", sep="")))
    
    temp.fit<-tbl_locs_fit$predict[[1]]
    temp.fit<-temp.fit[is.na(temp.fit$x)==T,]
    temp.fit$id<-lev
    loc.interp<-rbind(loc.interp,temp.fit)
    
}   


#}#end for individual loop
