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

create_seealso <- function(subject, seealso, format, label = "", rdf = NULL) {
  ld <- split_seealso(data.frame(subject = subject, 
                                 seeAlso = seealso, 
                                 format = format, 
                                 label = label, 
                                 stringsAsFactors = FALSE))
  if (!is.null(rdf)) {
    return(add_to_rdf(ld, rdf))
  } else {
    return(ld)
  }
}

mint_feature <- function(subject, label, type, rdf = NULL) {
  ld <- data.frame(subject = subject, label = label, 
                   type = type, stringsAsFactors = FALSE)
  ld <- rename_ld(ld)
  ld <- gather(ld, predicate, object, -subject)
  
  if (!is.null(rdf)) {
    return(add_to_rdf(ld, rdf))
  } else {
    return(ld)
  }
}

create_association <- function(subject, predicate, object, rdf = NULL) {
  ld <- data.frame(subject = subject, 
                   predicate = predicate, 
                   object = object, stringsAsFactors = FALSE)
  if (!is.null(rdf)) {
    return(add_to_rdf(ld, rdf))
  } else {
    return(ld)
  }
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