#' @export
ed_quarter <- function (x, ...) {
  UseMethod("ed_quarter", x)
}

#' @export
as.ed_quarter <- function (x, ...) {
  UseMethod("ed_quarter", x)
}

#' @export
ed_quarter.ed_quarter <- function (x, mode) {
  if (missing(mode)) {
    mode <- ed_quarter_mode(x)
  } else {
    mode <- match.arg(mode, choices = c("calendar","academic","fiscal"))
  }
  if (attr(x, "mode") == mode) {
    return (x)
  } else {
    return (ed_quarter_mode(x, mode))
  }
}

#' @import stringr
#' @export
ed_quarter.default <- function (
    x, format = c("%Y%i","%Q%Y", "%Q %g","%Y %q","%N %Y"), mode = c("calendar","academic","fiscal")
) {
  new_ed_quarter(x, format, mode)
}

#' @export
ed_quarter.Date <- function (
    x, mode = c("calendar","academic","fiscal")
) {
  mode <- match.arg(mode)
  qt <- ceiling(as.numeric(format(x, "%m")) / 3)
  yr <- as.numeric(format(x, "%Y"))
  if (mode != "calendar") {
    if (qt == 4) {
      yr <- yr + 1
    }
    qt <- qt %% 4 + 1
  }
  new_ed_quarter(paste0(yr,qt), format = "%Y%i", mode = mode)
}

#' @export
ed_quarter.data.frame <- function (
    x, format, mode = c("calendar","academic","fiscal")
) {
  if (ncol(x) < 1 | ncol(x) > 2)
    stop("x must have 1 or 2 columns.")
  if (ncol(x) != length(format))
    stop("Number of columns in x must match the length of format.")
  sapply(seq_along(x), function(i) {
    ed_quarter.character(as.character(x[,i]), format[i])
  })
}

#' @export
ed_quarter.matrix <- function (x, ...) {
  if (ncol(x) > 1) {
    return(ed_quarter.data.frame(as.data.frame(x), ...))
  } else {
    return(ed_quarter.character(as.character(as.vector(x)), ...))
  }
}

#' @export
ed_quarter.list <- function (x, ...) {
  x <- do.call(rbind, x)
  if (!is.null(ncol(x)) && ncol(x) > 1) {
    return(ed_quarter.data.frame(as.data.frame(x), ...))
  } else {
    return(ed_quarter.character(as.character(as.vector(x)), ...))
  }
}

#' @export
as.matrix.ed_quarter <- function (x, mode = c("calendar","academic","fiscal")) {
  mode <- match.arg(mode)
  x <- ed_quarter_mode(x, mode)
  cbind(attr(x, "quarter"), attr(x, "year"))
}

#' @export
format.ed_quarter <- function (x, format, ...) {
  mode <- attr(x, "mode")
  if (missing(format) || is.null(format) || format == "") {
    if (mode == "calendar") {
      format = "%q %Y"
    } else if (mode == "academic") {
      format = "%Q %g"
    } else if (mode == "fiscal") {
      format = "%Y%i"
    }
  }

  fmt_elem <- unlist(str_split(format, "%"))
  out <- rep("", length(x))
  for (i in seq_along(fmt_elem)) {
    f <- str_sub(fmt_elem[i], 1, 1)
    if (f == "")
      next
    if (f == "Y") {
      if (mode == "academic")
        stop("%Y format not allowed for mode = 'academic'")
      elem <- attr(x, "year")
    } else if (f == "y") {
      if (mode == "academic")
        stop("%y format not allowed for mode = 'academic'")
      elem <- str_sub(attr(x, "year"), -2, -1)
    } else if (f == "G") {
      if (mode == "academic"){
        elem <- paste0(attr(x, "year") - 1, "-", attr(x, "year"))
      } else {
        elem <- attr(x, "year")
      }
    } else if (f == "g") {
      if (mode == "academic"){
        elem <- paste0(attr(x, "year") - 1, "-", str_sub(attr(x, "year"), -2, -1))
      } else {
        elem <- str_sub(attr(x, "year"), -2, -1)
      }
    } else if (f == "i") {
      elem <- attr(x, "quarter")
    } else if (f %in% c("q","Q","N")) {
      qt <- attr(x, "quarter")
      if (mode == "calendar") {
        qt <- qt %% 4 + 1
      }
      if (f == "q") {
        elem <- q_quarter[qt]
      } else if (f == "Q") {
        elem <- Q_quarter[qt]
      } else if (f == "N") {
        elem <- N_quarter[qt]
      }
    } else {
      stop("Unknown format: use %Y, %y, %G, %g, %Q, %q, %i, or %N for year and quarter formats.")
    }
    out <- paste0(out, elem, str_sub(fmt_elem[i], 2, -1))
  }
  return(out)
}

#' @export
print.ed_quarter <- function (x, ...) {
  cat(format.ed_quarter(x, ...), fill = 1)
}

#' @export
ed_quarter_mode <- function (x, ...) {
  UseMethod("ed_quarter_mode", x)
}

#' @export
ed_quarter_mode.ed_quarter <- function (x, mode) {
  if (missing(mode)) {
    return(attr(x, "mode"))
  }
  mode <- match.arg(mode, choices = c("calendar","academic","fiscal"))
  x_mode <- attr(x, "mode")
  if (x_mode == mode) {
    return(x)
  }
  if (x_mode != "calendar" & mode != "calendar") {
    attr(x, "mode") <- mode
    return(x)
  }
  x_qt <- attr(x, "quarter")
  x_yr <- attr(x, "year")
  if (x_mode == "calendar") {
    qt <- x_qt %% 4 + 1
    yr <- ifelse(x_qt == 4, x_yr + 1, x_yr)
  } else {
    qt <- (x_qt + 2) %% 4 + 1
    yr <- ifelse(x_qt == 1, x_yr - 1, x_yr)
  }
  make_education_quarter(qt, yr, mode)
}

#' @export
ed_quarter_mode.default <- function (x, ...) {
  ed_quarter(x, ...)
}

#' @export
year <- function (x, format) {
  if (missing(format) || is.null(format) || format == "") {
    if (ed_quarter_mode(x) == "academic") {
      format <- "%g"
    } else {
      format <- "%Y"
    }
  }
  format.ed_quarter(x, format = format)
}

#' @export
quarter <- function (x, format) {
  if (missing(format) || is.null(format) || format == "") {
    if (ed_quarter_mode(x) == "academic") {
      format <- "%Q"
    } else if (ed_quarter_mode(x) == "calendar") {
      format <- "%q"
    } else {
      format <- "%i"
    }
  }
  format.ed_quarter(x, format = format)
}

#' @export
ed_quarter_range <- function (
    fm_quarter, to_quarter, format = c("%Y%i","%Q%Y", "%Q %g","%Y %q","%N %Y"),
    mode = c("calendar","academic","fiscal")
) {
  mode <- match.arg(mode)
  fm_quarter <- ed_quarter(fm_quarter[1], format, mode)
  to_quarter <- ed_quarter(to_quarter[1], format, mode)
  fm <- as.numeric(ed_quarter_mode(fm_quarter, mode = "calendar"))
  to <- as.numeric(ed_quarter_mode(to_quarter, mode = "calendar"))
  fm <- trunc(fm / 10) + ((fm - 1) - signif(fm, 4)) / 4
  to <- trunc(to / 10) + ((to - 1) - signif(to, 4)) / 4
  if (fm < to){
    qt_range <- seq(fm, to, .25)
  } else {
    qt_range <- seq(fm, to, -.25)
  }
  qt_range <- trunc(qt_range) * 10 + (qt_range - trunc(qt_range)) * 4 + 1
  qt_range <- ed_quarter(as.character(qt_range), format = "%Y%i", mode = "calendar")
  return (ed_quarter_mode(qt_range, mode = mode))
}

#' @import stringr
#' @export
`[.ed_quarter` <- function(x, i) {
  elem    <- NextMethod()
  make_education_quarter(as.integer(str_sub(elem, -1, -1)),
                         as.integer(str_sub(elem, 1, 4)),
                         ed_quarter_mode(x))
}

#' @export
`[[.ed_quarter` <- function(x, i) {
  elem    <- NextMethod()
  format(make_education_quarter(as.integer(str_sub(elem, -1, -1)),
                                as.integer(str_sub(elem, 1, 4)),
                                ed_quarter_mode(x)))
}

new_ed_quarter <- function (
    x, format = c("%Y%i","%Q%Y", "%Q %g","%Y %q","%N %Y"), mode = c("calendar","academic","fiscal")
) {
  x <- as.character(x)
  mode <- match.arg(mode)
  format <- format[1]
  fmt_elem <- unlist(str_split(format, "%"))

  yr <- NULL
  qt <- NULL
  for (i in seq_along(fmt_elem)) {
    f <- str_sub(fmt_elem[i], 1, 1)
    if (f == "")
      next
    if (f %in% c("Y","y","G","g")) {
      if (f == "G") {
        if (mode != "academic")
          stop("%G is only used when mode = 'academic'.")
        if (!all(str_detect(x, "^[0-9]{4}.[0-9]{4}")))
          stop("x does not match %G format.")
        yr <- suppressWarnings(as.numeric(str_sub(x, 1, 4))) + 1
        x <- str_sub(x, 10, -1)
      } else if (f == "g") {
        if (mode != "academic")
          stop("%G is only used when mode = 'academic'.")
        if (!all(str_detect(x, "^[0-9]{4}.[0-9]{2}")))
          stop("x does not match %g format.")
        yr <- suppressWarnings(as.numeric(str_sub(x, 1, 4))) + 1
        x <- str_sub(x, 8, -1)
      } else if (f == "Y") {
        if (mode == "academic")
          stop("%Y not allowed for mode = 'academic'. Use %G instead.")
        if (!all(str_detect(x, "^[0-9]{4}")))
          stop("x does not match %Y format.")
        yr <- str_sub(x, 1, 4)
        x <- str_sub(x, 5, -1)
      } else {
        if (mode == "academic")
          stop("%y not allowed for mode = 'academic'. Use %g instead.")
        if (!all(str_detect(x, "^[0-9]{2}")))
          stop("x does not match %y format.")
        yr <- paste0(str_sub(Sys.Date(), 1, 2), str_sub(x, 1, 2))
        x <- str_sub(x, 3, -1)
      }
      yr <- suppressWarnings(as.numeric(yr))
    } else if (f %in% c("Q","q","i","N")) {
      if (f == "Q") {
        qt <- match(str_extract(str_to_upper(x), Q_pattern), Q_quarter)
        x <- str_sub(x, str_length(Q_quarter[qt]) + 1, -1)
      } else if (f == "q") {
        qt <- match(str_extract(str_to_upper(x), q_pattern), q_quarter)
        x <- str_sub(x, str_length(q_quarter[qt]) + 1, -1)
      } else if (f == "N") {
        qt <- match(str_extract(str_to_upper(x), N_pattern), N_quarter)
        x <- str_sub(x, str_length(N_quarter[qt]) + 1, -1)
      } else {
        qt <- match(str_extract(x, i_pattern), i_quarter)
        x <- str_sub(x, str_length(i_quarter[qt]) + 1, -1)
      }
      if (any(is.na(qt)))
        stop(sprintf("x does not match %%%s format.", f))
      if (f %in% c("Q","q","N") & mode == "calendar") {
        qt <- (qt + 2) %% 4 + 1
      }
    } else {
      stop("Unknown format: use %Y, %y, %G, %g, %Q, %q, %i, or %N for year and quarter formats.")
    }
    r <- str_sub(fmt_elem[i], 2, -1)
    if (str_length(r) > 0) {
      f_pattern <- paste0("^", r)
      x <- str_remove(x, f_pattern)
    }
  }
  if (is.null(qt)) {
    qt <- rep(1, length(x))
  }
  if (is.null(yr)) {
    yr <- rep(as.numeric(str_sub(Sys.Date(), 1, 4)), length(x))
  }
  make_education_quarter(qt, yr, mode)
}
make_education_quarter <- function (quarter, year, mode) {
  structure(
    paste0(year, quarter),
    class   = c("ed_quarter","character"),
    quarter = quarter,
    year    = year,
    mode    = mode
  )
}
