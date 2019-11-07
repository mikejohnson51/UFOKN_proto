##################################################
## Project: UFOKN
## Script purpose: Building Graphs for Gages, NHD and Roads
## Lat Modified: Nov 5, 2019
## Author: Mike Johnson
##################################################

library(AOI)
library(HydroData)
library(nwmRetro)
library(sf)
library(raster)


# Use the roads shp as the minimum bounding box ---------------------------

bb   = getBoundingBox(st_transform(roads, AOI::aoiProj))
usgs = findNWIS(bb)
load("./data/usgs_metadata.rda")
rl = ncdf4::nc_open("/Users/mikejohnson/Documents/GitHub/hydraulics/data_ext/RouteLink_NHDPLUS_v20.nc")
id = ncdf4::ncvar_get(rl, "link")

# Prepare NWIS gage RDF file ----------------------------------------------

nwis = merge(usgs$nwis, usgs_meta, 
             by.x = 'site_no', by.y = "siteID") %>% 
  mutate(urls = paste0('https://waterdata.usgs.gov/',tolower(state),'/nwis/uv?site_no=',site_no),
         nwm_index =  fastmatch::fmatch(nwis$COMID, id)) 

ff = lapply(nwis$site_no, find_nwis_norms)

gages_all = bind_rows(ff) %>% 
  merge(nwis, all.y = TRUE) %>%
  dplyr::select(site_no   = site_no,
                label     = station_nm,
                county    = county,
                state     = state,
                geoid     = geoid,
                bankfullQ = bf, 
                meanFlow  = meanFlow,
                sdFlow    = sdFlow, 
                maxFlow   = maxFlow,
                comid     = COMID, 
                nwm_index = nwm_index,
                huc8      = huc8,
                urls      = urls, 
                geometry  = geometry)

write_sf(gages_all, "./data/nwis/gages.shp" )
out = df_to_rdf(df = gages_all, type = "nwis-gage", key = 'site_no')
rdf_serialize(out, format = "turtle", doc = "./data/gages.rdf")

# NHD Graph ---------------------------------------------------------------

spat = findNHD(bb)
# ll = lapply(nhd$comid, process_comid)
# nwm_process = bind_rows(ll)
# save(nwm_process, file = "./data/processed_nwm_by_comid.rda", compress = "xz")

load("./data/upperMainGages.rda")
um1 = um %>% na.omit() %>% group_by(comid) %>% arrange(distance) %>% slice(n = 1) %>% dplyr::select(comid, us_nwis_site_no = nwis_id, us_gage_dist_km = distance)

load("./data/downMainGages.rda")
dm1 = dm %>% na.omit() %>% group_by(comid) %>% arrange(distance) %>% slice(n = 1) %>% dplyr::select(comid, ds_nwis_site_no = nwis_id, ds_gage_dist_km = distance)

m = merge(um1, dm1, all = TRUE)

nhd = spat %>% 
  dplyr::select(comid = comid,
                label = gnis_name,
                reachcode = reachcode,
                streamorder = streamorde) %>% 
  merge(nwm_process, by = 'comid', all =T) %>% 
  merge(m, by = "comid", all.x = TRUE) %>% 
  mutate(huc8 = substr(reachcode,1,8),
         nwm_index = fastmatch::fmatch(comid, id)) %>% 
  filter(!is.na(nwm_index))
  
all = merge(st_drop_geometry(nhd), gages_all, by = "comid") %>% mutate(
  nME_flow = 100*(meanFlow.x - meanFlow.y) / meanFlow.x,
  nME_bankfull = 100*(bankfullQ - bankfull) / bankfullQ) %>% 
  dplyr::select(comid, site_no, nME_flow, nME_bankfull)

nhd2 = right_join(all, nhd) %>% st_as_sf()

write_sf(nhd2, "./data/nwis/nhd.shp" )
out = df_to_rdf(df = nhd2, type = "nhd-flowline", key = 'comid')
rdf_serialize(out, format = "turtle", doc = "./data/flowlines.rdf")



# Catchment ---------------------------------------------------------------

catch= read_sf("./data/nhd/catchmask.shp") %>% st_transform(5070) %>% mutate(comid = featureid)
nlcd.path = '/Volumes/Seagate5tb/NLCD/nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img'
nlcd = raster(nlcd.path)

get_area = function(x){
catch1 = nlcd %>% crop(catch[x,]) %>% mask(catch[x,]) %>% getValues() %>% table()
percent_urban = sum(catch1[rownames(catch1) %in% c("21", "22", "23", "24")]) / sum(catch1)
data.frame(comid = catch$featureid[x], percent_urban = 100*percent_urban, area = catch$areasqkm[x])
}

#area = lapply(1:nrow(catch), get_area)

area2 =bind_rows(area)

catchments = right_join(catch, area2) %>% dplyr::select(comid, areasqkm, percent_urban) %>% mutate(percent_urban = percent_urban*100)

write_sf(catchments, "./data/nwis/catchmask.shp" )
out = df_to_rdf(df = catchments, type = "nhd-catchments", key = 'comid')
rdf_serialize(out, format = "turtle", doc = "./data/catchments.rdf")



# Roads -------------------------------------------------------------------

roadTypeid = c('S1200', "S1100")
roadType = c("Secondary Road", "Primary Road")
RTid = c("M", "S", "U", "I", "O", "C")
RT = c("Common Name", "State Recognized", "U.S", "Interstate", "Other", "County")

roads = load_shp_data("data/roads/", out_prj) %>% 
  mutate(routeType =  RT[match(RTTYP, RTid)],
         roadType  =  roadType[match(MTFCC, roadTypeid)]) %>% 
  dplyr::select(road_id = LINEARID,
         label = Name,
         routeType = routeType,
         roadType = roadType) %>% st_zm()

write_sf(roads, "./data/nwis/my_roads.shp" )
out = df_to_rdf(df = roads, type = "roads", key = 'road_id')
rdf_serialize(out, format = "turtle", doc = "./data/roads.rdf")


# Road Crossings ----------------------------------------------------------

crosses = find_nhd_intersect(roads, nhd2, ann_prj)
crosses_df =   data.frame(subject = paste0("https://ufokn.demo/roads/", int$road_id ), 
                 predicate = '"https://ufokn.demo/association/crosses"',
                 object = paste0('https://ufokn.demo/nhd-flowlines/', int$comid),
                 stringsAsFactors = F)

crosses = add_to_rdf(crosses_df, rdf())
rdf_serialize(crosses, format = "turtle", doc = "./data/crossings.rdf")

intersects = find_catchment_intersect(roads, catchments, ann_prj)
intersects_df =   data.frame(subject = paste0("https://ufokn.demo/roads/", int$road_id ), 
                          predicate = '"https://ufokn.demo/association/intersects"',
                          object = paste0('https://ufokn.demo/nhd-catchment/', int$comid),
                          stringsAsFactors = F)

intersects = add_to_rdf(intersects_df, rdf())
rdf_serialize(intersects, format = "turtle", doc = "./data/intersects.rdf")



# catchmask = HydroData::query_cida(getBoundingBox(gages), type = "catchments")
# write_sf(catchmask, "./data/nhd/catchmask.shp" )

# HUC6 = unique(substr(nhd$reachcode,1,6))
# 
# base = '/Volumes/Seagate5tb/build_flood_data/data/'
# 
# hand_files = paste0(base, "hand/hand_", HUC6, ".tif") %>% 
#     mosaic.lf(bb)
# catch_files = paste0(base, "catchmask/catchmask_", HUC6, ".tif") %>% 
#    mosaic.lf(bb)
# 
# h = hand_files
# c = catch_files
# 
# h2 = crop(h, getBoundingBox(c))
# extent(h2) = extent(c)
# 
# writeRaster(c, file = "./data/catchmask.tif", options=c('TFW=YES'), overwrite = T)
# writeRaster(h2, file = "./data/hand.tif", options=c('TFW=YES'), overwrite = T)
# 
# all_rc = data.frame()
# 
# for(i in 1:length(HUC6)){
#   load(paste0(base, "/rating/rating_", HUC6[i], ".rda"))
#   rc  = rc %>% filter(CatchId %in% nhd$comid)
#   all_rc = rbind(all_rc, rc)
# }
# 
# save(all_rc, file = "./data/okn_rc.rda", compress = 'xz')
# 
# um = lapply(nhd$comid, find_network_gage, nav = "UM")
# um = bind_rows(um)
# save(um, file = "./data/upperMainGages.rda", compress = "xz")
# 
# dm = lapply(nhd$comid, find_network_gage, nav = "DM")
# dm = bind_rows(dm)
# save(dm, file = "./data/downMainGages.rda", compress = "xz")
# 
# 
hand   = raster('./data/hand.tif')
catchR = raster('./data/catchmask.tif')
s = brick(hand, catchR)
r = roads %>% st_zm() %>% st_transform(s@crs)
#  
 ll = lapply(1:nrow(r), extract_road_depths, s = s, roads = r, all_rc = all_rc)
# minFlows = bind_rows(ll)
# 
# save(minFlows, file = "./data/minFlows.rda", compress = "xz")
# 
# x = 20

load("./data/minFlows.rda") 
minFlows %>% dplyr::select(comid = catchmask, criticalDepth = HANDmin, road_id = road_ID, criticalFlow = flow_cms)
