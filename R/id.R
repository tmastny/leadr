#' @importFrom crayon bgGreen black
#' @export
id <- function(num = NULL) {
  if (is.null(num))
    num <- vector("integer")

  as_id(num)
}

#' @export
as_id <- function(x) {
  structure(x, class = "id")
}

#' @export
c.id <- function(x, ...) {
  as_id(NextMethod())
}

#' @export
`[.id` <- function(x, i) {
  as_id(NextMethod())
}

#' @export
format.id <- function(x, ..., formatter = id_color) {
  x_highlight <- which(x == get_id())
  set_id(NULL)

  x_valid <- which(!is.na(x))

  ret <- rep("<NA>", length(x))
  ret[x_valid] <- formatter(x[x_valid], fun = black)
  ret[x_highlight] <- formatter(x[x_highlight], fun = bgGreen)

  format(ret, justify = "right")
}

id_color <- function(x, fun) {
  sprintf(fun(x))
}

#' @export
print.id <- function(x, ...) {
  cat(format(x, ...))
  invisible(x)
}


#' @importFrom pillar type_sum
#' @export
type_sum.id <- function(x) {
  "id"
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.id <- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "right")
}






