# package globals
.globals <- new.env(parent = emptyenv())
.globals$path <- NULL
.globals$dir <- NULL
.globals$num <- NULL

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

get_num <- function() {
  .globals$num
}

set_num <- function(num) {
  .globals$num <- num
}
