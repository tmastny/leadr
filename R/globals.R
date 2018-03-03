# package globals
.globals <- new.env(parent = emptyenv())
.globals$path <- NULL
.globals$dir <- NULL
.globals$id <- NULL

get_path <- function() {
  .globals$path
}

set_path <- function(path) {
  .globals$path <- path
}

get_dir <- function() {
  .globals$dir
}

set_dir <- function(dir) {
  .globals$dir <- dir
}

get_id <- function() {
  .globals$id
}

set_id <- function(id) {
  .globals$id <- id
}

