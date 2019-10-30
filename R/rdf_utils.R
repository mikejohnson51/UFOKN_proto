library(sf)
library(dplyr)
library(tidyr)
library(rdflib)

rdf_base <- "http://www.w3.org/2000/01/rdf-schema#"
dct_base <- "http://purl.org/dc/terms/"

split_seealso <- function(x) {
  rbind(select(x, subject, object = seeAlso) %>%
          mutate(predicate = paste0(rdf_base, "seeAlso")),
        select(x, subject = seeAlso, object = format) %>%
          mutate(predicate = paste0(dct_base, "format")),
        select(x, subject = seeAlso, object = label) %>%
          mutate(predicate = paste0(rdf_base, "label"))) %>%
    select(subject, predicate, object)
}

add_to_rdf <- function(x, rdf) { # dumb implementation, but it does the job!
  message(paste("Adding", nrow(x), "to rdf."))
  for(r in seq(1, nrow(x))) {
    rdf <- rdf_add(rdf, x$subject[r], x$predicate[r], x$object[r])
  }
  return(rdf)
}

create_seealso <- function(subject, seealso, format, label = "", rdf) {
  ld <- split_seealso(data.frame(subject = subject, 
                                 seeAlso = seealso, 
                                 format = format, 
                                 label = label, 
                                 stringsAsFactors = FALSE))

  return(rdf_serialize(add_to_rdf(ld, rdf), format = "turtle"))
}

mint_feature <- function(subject, label, type, rdf) {
  ld <- data.frame(subject = subject, label = label, 
                   type = type, stringsAsFactors = FALSE)
  ld <- rename_ld(ld)
  ld <- gather(ld, predicate, object, -subject)
  
  return(rdf_serialize(add_to_rdf(ld, rdf), format = "turtle"))
}

create_association <- function(subject, predicate, object, rdf) {
  ld <- data.frame(subject = subject, 
                   predicate = predicate, 
                   object = object, stringsAsFactors = FALSE)
  
  return(rdf_serialize(add_to_rdf(ld, rdf), format = "turtle"))
}

add_geometry <- function(subject, geometry, rdf) {
  nodes <- paste0("g", seq(1, length(subject)))
  
  for(i in 1:length(subject)) {
  rdf <- rdf_add(rdf, 
                  subject = subject[i], 
                  predicate = "http://www.opengeospatial.org/standards/geosparql/hasGeometry", 
                  object = nodes[i], objectType = "blank")
  
  rdf <- rdf_add(rdf, 
                  subject = nodes[i], 
                  predicate = "http://www.w3.org/1999/02/22-rdf-syntax-ns#type", 
                  object = "http://www.opengeospatial.org/standards/geosparql/Geometry", 
                  subjectType = "blank")
  
  rdf <- rdf_add(rdf, 
                  subject = nodes[i], 
                  predicate = "http://www.opengeospatial.org/standards/geosparql/asWKT", 
                  object = st_as_text(st_zm(geometry[i])), subjectType = "blank")
  }
  return(rdf_serialize(rdf, format = "turtle"))
}

rename_ld <- function(x) {
  
  old_names <- c("subject",  
                 "label", 
                 "type")
  new_names <- c("subject",  
                 paste0(rdf_base, "label"), 
                 "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")
  
  if (!all(names(x) %in% old_names)) 
    stop(paste("unsupported names passed in must be in", old_names))
  
  names_picker <- old_names %in% names(x)
  old_names <- old_names[names_picker]
  new_names <- new_names[names_picker]
  
  names(x)[match(old_names, names(x))] <- new_names
  
  return(x)
}