load_shp_data <- function(shp_path, prj) {
  d <- sf::read_sf(shp_path)
  sf::st_transform(d, prj)
}

find_gage_points <- function(roads, gages, ann_prj, dist) {
  sf::st_join(sf::st_transform(sf::st_zm(gages), ann_prj),
              sf::st_transform(sf::st_zm(roads), ann_prj),
              st_is_within_distance,
              dist = dist) %>%
    st_set_geometry(NULL) %>%
    dplyr::select(site_no = site_no, road_LINEARID = LINEARID) %>%
    filter(!is.na(road_LINEARID))
}

find_network_gage = function(x, nav){
  um = findNLDI(comid = x, nav = nav, find = "nwis")
  if(length(um$nwissite) > 0){
    data.frame(comid = as.character(um$nwissite$comid[1]), 
               nwis_id = as.character(gsub("USGS-", "", um$nwissite$identifier))[1],
               distance = as.numeric(sf::st_distance(um$site, um$nwissite)[1])/1e6,
               stringsAsFactors = F)
  } else {
    data.frame(comid = as.character(x) , nwis_id = NA, distance =NA, stringsAsFactors = F)
  }
}

find_catchment_intersect = function(roads, catchmask, ann_prj){
  
  sf::st_join(sf::st_transform(sf::st_zm(catchmask), ann_prj),
              sf::st_transform(sf::st_zm(roads), ann_prj),
              st_intersects) %>% 
    st_set_geometry(NULL) %>%
    dplyr::select(comid = comid, road_id = road_id) %>%
    filter(!is.na(road_id))
}

find_nhd_intersect = function(roads, nhd, ann_prj){
  
  sf::st_join(sf::st_transform(sf::st_zm(nhd), ann_prj),
              sf::st_transform(sf::st_zm(roads), ann_prj),
              st_intersects) %>% 
    st_set_geometry(NULL) %>%
    dplyr::select(comid = comid, road_id = road_id) %>%
    filter(!is.na(road_id))
}

mosaic.lf = function(input = NULL, bb = NULL){
  
  s = list()
  bb = getBoundingBox(bb)
  
  for(i in seq_along(input)){
    tmp <- raster::raster(input[i])
    bb = sf::st_transform(bb, as.character(tmp@crs))
    
    if(!is.null(raster::intersect(raster::extent(tmp),raster::extent(bb)))){
      s[[i]] <- raster::crop(tmp, bb, snap = "out")
    }
  }
  
  origins<-t(data.frame(lapply(s,raster::origin)))
  
  min_origin<-c(min(origins[,1]),min(origins[,2]))
  
  change_origins <- function(x,y){
    raster::origin(x)<-y
    x
  }
  
  s <- lapply(s, function(x,y) change_origins(x,min_origin))
  
  if(length(s) == 1){
    mos = s[[1]]
  } else {
    mos = do.call(raster::merge, s)
  }
}

extract_road_depths = function(x, s, roads, all_rc){
  
  r1 = roads[x,]
  s2 = crop(s, getBoundingBox(r1))

  hand_df <- extract(s2, r1)[[1]] %>% 
    data.frame() %>% 
    group_by(catchmask) %>% 
    filter(hand != 0) %>% 
    summarize(HANDmin = min(hand)) %>% mutate(road_ID = r1$LINEARID, flow_cms = NA)
  
  print(x)
  
  if(nrow(hand_df) == 0){
    return(NULL)
  } else{ 
  
    for(i in 1:nrow(hand_df)){
    rc = all_rc %>% filter(CatchId == hand_df$catchmask[i])
    
    if(nrow(rc) > 0){
    hand_df$flow_cms[i] = approx(rc$Stage, rc$`Discharge (m3s-1)`, xout = hand_df$HANDmin[i])$y
    }
  }
  return(hand_df)
  
}
}

find_nwis_norms = function(x){
  
  d = dataRetrieval::readNWISdv(x, param = "00060") %>%
    dataRetrieval::renameNWISColumns() %>% 
    filter(!is.na(Flow))
  
  d2 = d %>% mutate(year = format(Date, "%Y")) %>% 
    group_by(year) %>% 
    summarise(annMax = max(Flow, na.rm = T)) %>% 
    mutate(annMax = annMax * 0.028316847)
  
  if(nrow(d2) < 25){
    bf = NA
  } else {
    bf = bankfull(d2$annMax)
  }
  
  d = d  %>% summarise(meanFlow = mean(Flow, na.rm = T), 
              sdFlow = sd(Flow, na.rm = T), 
              maxFlow = max(Flow,na.rm = T)) 
  
  (d * 0.028316847) %>% mutate(site_no = x, bf = bf )
  
}

bankfull = function(input, exc = .666){
  
  o = data.frame(q = sort(input, decreasing = TRUE),
                 rank = 1:length(input)) %>%
    mutate(val = rank / (length(input) + 1))
  
  
  f1 = approxfun(o$val, o$q)
  
  f1(exc)
}

get = function(x, index){
  nc = ncdf4::nc_open(paste0(base, "year", x, "mappingFile1.nc"))
  flow = ncdf4::ncvar_get(nc, "streamflow", start= c(index,1), count = c(1, nc$dim$time$len)) 
  ncdf4::nc_close(nc)
  flow
}

get_index = function(comid){
  nc = ncdf4::nc_open(paste0(base, 'year1993mappingFile1.nc'))
  id = ncdf4::ncvar_get(nc, "feature_id") 
  ncdf4::nc_close(nc)
  fastmatch::fmatch(comid, id)
}

process_comid = function(comid){

  base = '/Volumes/Seagate5tb/NWM_V12_AGG/'
  
  print(comid)
  index = get_index(comid)
  
  if(!is.na(index)){
    
    flows = lapply(c(1993:2017), get, index = index)
    
    df  = data.frame(dates = seq.Date(as.Date("1993-01-01"), as.Date("2017-12-31"), by = "d"),
                     flows = do.call(c,flows)) 
 
    df = df[complete.cases(df),]
    
    if(nrow(df) > 0){
    d2 = df %>% mutate(year = format(dates, "%Y")) %>% 
      group_by(year) %>% 
      summarise(annMax = max(flows, na.rm = T)) %>% 
      mutate(annMax = annMax)
    
      bf = bankfull(d2$annMax)
    
    d = df  %>% summarise(meanFlow = mean(flows, na.rm = T), 
                         sdFlow = sd(flows, na.rm = T), 
                         maxFlow = max(flows,na.rm = T), 
                         bankfull = bf,
                         comid = comid) 
    return(d)
    
  } else{
    NULL
  } 
  }else {
    NULL
  }
}

# ll = lapply(nhd$comid, process_comid)
# 
# process_comid(comid)

df_to_rdf = function(df, type, key){
  
  if(AOI::checkClass(df, 'sf')){ df = st_drop_geometry(df)}
  
  base_uri = paste0("https://ufokn.demo/", type, "/")
  
  gages.tmp = df %>% 
    gather(subject, object, -key) %>% 
    setNames(c("subject", "predicate", "object")) %>% 
    mutate(subject = paste0(base_uri, subject),
           predicate = paste0("https://ufokn.demo/", predicate, "/")) %>% 
    filter(!is.na(object)) %>% 
    mutate_all(as.character) 
  
  rdf = data.frame(subject = paste0(base_uri, df[[key]]), 
                   predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
                   object = paste0('https://ufokn.demo/type/', type,'/'),
                   stringsAsFactors = F) %>% bind_rows(gages.tmp) 
  
  out = add_to_rdf(rdf, rdf())
  
  out
  
}

add_to_rdf = function(x, rdf) { # dumb implementation, but it does the job!
  message(paste("Adding", nrow(x), "to rdf."))
  for(r in seq(1, nrow(x))) {
    rdf <- rdf_add(rdf, x$subject[r], x$predicate[r], x$object[r])
  }
  return(rdf)
}

