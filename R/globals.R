# package globals
.globals <- new.env(parent = emptyenv())
.globals$id <- NULL

get_id <- function() {
  .globals$id
}

set_id <- function(id) {
  .globals$id <- id
}


