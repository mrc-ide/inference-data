## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)
dir.create("check")

#' Read in areas
areas <- sf::read_sf("depends/zaf_areas.geojson")

#' Save boundaries
sf::write_sf(areas, "zaf_areas.geojson", delete_dsn = TRUE)
