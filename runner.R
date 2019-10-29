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
                   flood_polys = load_shp_data("data/flood_depths/", out_prj),
                   gages = load_shp_data("data/gages/", out_prj),
                   close_gages = find_gage_points(roads, gages, ann_prj, dist),
                   roads_base_uri = "https://ufokn.demo/roads/",
                   gages_base_uri = "https://ufokn.demo/gages/",
                   roads_rdf = mint_feature(subject = paste0(roads_base_uri, roads$LINEARID), 
                                            type = "https://ufokn.demo/type/road/",
                                            label = roads$Name, rdf = rdf()),
                   gages_rdf = mint_feature(subject = paste0(gages_base_uri, gages$Name),
                                            type = "https://ufokn.demo/type/gage/", 
                                            label = gages$Name, rdf = rdf()), 
                   gages_roads_rdf = create_association(subject = paste0(roads_base_uri, close_gages$road_LINEARID), 
                                                        predicate = "https://ufokn.demo/association/nearbyGage", 
                                                        object = paste0(gages_base_uri, close_gages$road_LINEARID),
                                                        rdf = rdf()),
                   roads_out_file = "output/roads.rdf",
                   target(rdf_serialize(roads_rdf, doc = roads_out_file), file_out(roads_out_file)),
                   gages_out_file = "output/gages.rdf",
                   target(rdf_serialize(gages_rdf, doc = gages_out_file), file_out(gages_out_file)),
                   gages_roads_out_file = "output/gages_roads.rdf",
                   target(rdf_serialize(gages_roads_rdf, doc = gages_roads_out_file), file_out(gages_roads_out_file)))


make(plan)

roads <- readd("roads")
gages <- readd("gages")
close_gages <- readd("close_gages")
