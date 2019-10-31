# bah -- can't get google working on work network.
# library(googledrive)
# googledrive::drive_auth()
# drive_ls("1S3Iusm2RU3RvsEkxUgEHDYyI6PDuSrb5")
# drive_download("1VpRsP5QRc7DPyd5K_rShR5_wlnJ0ixHw", "data/major_roads/", overwrite = FALSE)
# drive_download("1Dy1kkSq6D7NRUt1svRTOejjIu1HO3x8K", "data/flood_points", overwrite = FALSE)
# DL was manual

library("drake")
source("R/source.R")
source("R/rdf_utils.R")

plan <- drake_plan(out_prj = 4326, # wgs84 lat/lon
                   ann_prj = 5070, # albers in meters
                   dist = 1000, # meters
                   roads = load_shp_data("data/major_roads/", out_prj),
                   gages = load_shp_data("data/gages/", out_prj),
                   close_gages = find_gage_points(roads, gages, ann_prj, dist),
                   roads_base_uri = "https://ufokn.demo/roads/",
                   gages_base_uri = "https://ufokn.demo/gages/",
                   roads_rdf = mint_feature(subject = paste0(roads_base_uri, roads$LINEARID), 
                                            type = "https://ufokn.demo/type/road/",
                                            label = roads$Name, 
                                            rdf = rdf()),
                   roads_rdf2 = create_seealso(subject = paste0(roads_base_uri, roads$LINEARID), 
                                               seealso = paste0(roads_base_uri, "get_data/", roads$LINEARID),
                                               format = "application/vnd.geo+json", 
                                               label = "GeoJSON Representation", 
                                               rdf = rdf_parse(roads_rdf, format = "turtle")),
                   roads_rdf3 = add_geometry(subject = paste0(roads_base_uri, roads$LINEARID), 
                                             geometry = st_geometry(roads), 
                                             rdf = rdf_parse(roads_rdf2, format = "turtle")),
                   gages_rdf = mint_feature(subject = paste0(gages_base_uri, gages$Name),
                                            type = "https://ufokn.demo/type/gage/", 
                                            label = gages$Name, 
                                            rdf = rdf()), 
                   gages_rdf2 = create_seealso(subject = paste0(gages_base_uri, gages$Name), 
                                               seealso = paste0(gages_base_uri, "get_data/", gages$Name),
                                               format = "application/vnd.geo+json", 
                                               label = "GeoJSON Representation", 
                                               rdf = rdf_parse(gages_rdf, format = "turtle")),
                   gages_rdf3 = add_geometry(subject = paste0(gages_base_uri, gages$Name), 
                                             geometry = st_geometry(gages), 
                                             rdf = rdf_parse(gages_rdf2, format = "turtle")),
                   gages_roads_rdf = create_association(subject = paste0(roads_base_uri, close_gages$road_LINEARID), 
                                                        predicate = "https://ufokn.demo/association/nearbyGage", 
                                                        object = paste0(gages_base_uri, close_gages$gage_Name),
                                                        rdf = rdf()),
                   roads_out_file = "output/roads.rdf",
                   target(rdf_serialize(rdf_parse(roads_rdf3, format = "turtle"), doc = roads_out_file), 
                          file_out(roads_out_file)),
                   gages_out_file = "output/gages.rdf",
                   target(rdf_serialize(rdf_parse(gages_rdf3, format = "turtle"), doc = gages_out_file), 
                          file_out(gages_out_file)),
                   gages_roads_out_file = "output/gages_roads.rdf",
                   target(rdf_serialize(rdf_parse(gages_roads_rdf, format = "turtle"), doc = gages_roads_out_file), 
                          file_out(gages_roads_out_file)))

clean()

make(plan)

roads <- readd("roads")
gages <- readd("gages")
close_gages <- readd("close_gages")
