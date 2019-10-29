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
    select(gage_Name = Name.x, road_LINEARID = LINEARID) %>%
    filter(!is.na(road_LINEARID))
}