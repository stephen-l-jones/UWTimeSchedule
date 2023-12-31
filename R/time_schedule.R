
#' @export
uw_time_schedule <- function (
    quarter, course_prefix, course_number = NULL, campus = c("bothell","tacoma","seattle"),
    start_selenium = TRUE, url_prefix = course_prefix
) {
  if (identical(start_selenium, TRUE)) {
    selenium <- start_selenium()
  } else {
    selenium <- NULL
  }
  if (missing(quarter) || missing(course_prefix)) {
    quarter_mode <- NULL
    schedule <- list()
  } else {
    quarter_mode <- ed_quarter_mode(as.ed_quarter(quarter))
    schedule <- scrape_schedule(quarter, course_prefix, course_number, campus, url_prefix, selenium$cookies)
  }

  append_schedule_function <- function (quarter, course_prefix, course_number = NULL,
                                        campus = c("bothell","tacoma","seattle"), url_prefix = course_prefix) {
    if (is.null(quarter_mode)) {
      quarter_mode <<- ed_quarter_mode(as.ed_quarter(quarter))
    } else {
      quarter <- ed_quarter_mode(as.ed_quarter(quarter), quarter_mode)
    }
    schedule <<- c(schedule,
                   scrape_schedule(quarter, course_prefix, course_number, campus, url_prefix, selenium$cookies))
  }
  section_function <- function() {
    section(schedule)
  }
  instructor_function <- function() {
    instructor(schedule)
  }
  meeting_function <- function() {
    meeting(schedule)
  }
  get_raw_schedule_function <- function () {
    return (schedule)
  }
  get_quarter_mode_function <- function () {
    return (quarter_mode)
  }
  set_raw_schedule_function <- function (x) {
    schedule <<- x
  }
  set_quarter_mode_function <- function (mode = c("calendar","academic","fiscal")) {
    mode <- match.arg(mode)
    if (is.null(quarter_mode)) {
      quarter_mode <<- mode
    } else {
      stop(sprintf("Quarter mode already set to %s.", quarter_mode))
    }
  }
  read_schedule_function <- function (quarter, course_prefix, course_number = NULL,
                                      campus = c("bothell","tacoma","seattle"), url_prefix = course_prefix) {
    quarter_mode <<- ed_quarter_mode(as.ed_quarter(quarter))
    schedule <<- scrape_schedule(quarter, course_prefix, course_number, campus, url_prefix, selenium$cookies)
  }
  start_selenium_function <- function () {
    selenium <<- start_selenium()
  }
  stop_selenium_function <- function () {
    selenium <<- stop_selenium(selenium)
  }
  structure(
    list(
      append_schedule  = append_schedule_function,
      read_schedule    = read_schedule_function,
      instructor       = instructor_function,
      meeting          = meeting_function,
      section          = section_function,
      get_raw_schedule = get_raw_schedule_function,
      set_raw_schedule = set_raw_schedule_function,
      get_quarter_mode = get_quarter_mode_function,
      set_quarter_mode = set_quarter_mode_function,
      start_selenium   = start_selenium_function,
      stop_selenium    = stop_selenium_function
    ),
    class = "time_schedule"
  )
}

#' @export
print.time_schedule <- function(x) {
  print.data.frame(x$section())
}

#' @export
summary.time_schedule <- function(x) {
  quarters <- unique(sapply(x$raw_schedule(), "[[", "quarter"))
  n_quarters <- length(quarters)
  if (n_quarters > 10) {
    quarters <- c(quarters[1:5], "...", quarters[-4:0 + n_quarters])
  }
  courses <- unique(sapply(x$raw_schedule(), "[[", "course_text"))
  n_courses <- length(courses)
  if (n_courses > 10) {
    courses <- c(courses[1:5], "...", courses[-4:0 + n_courses])
  }
  n_sections <- nrow(x$section())

  cat(sprintf("Number of quarters : %s", n_quarters), "\n")
  cat(sprintf("Number of courses  : %s", n_courses),  "\n")
  cat(sprintf("Number of sections : %s", n_sections), "\n")
  cat("Quarters:", paste(quarters, collapse = ", "), "\n")
  cat("Courses:", paste(courses, collapse = ", "), "\n")
}
