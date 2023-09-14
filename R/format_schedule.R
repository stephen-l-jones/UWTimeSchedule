
meeting <- function (schedule) {
  if (length(schedule) == 0) return (NULL)
  df <- format_raw(schedule)
  times <- sapply(df$meeting, nrow)
  out_format <- cbind(
    data.frame(
      sapply(df$quarter, format),
      sapply(df$quarter, function(x) year(ed_quarter_mode(x, "academic"))),
      sapply(df$quarter, function(x) quarter(ed_quarter_mode(x, "academic"))),
      sapply(df$quarter, function(x) as.integer(year(ed_quarter_mode(x, "fiscal")))),
      sapply(df$quarter, function(x) as.integer(quarter(ed_quarter_mode(x, "fiscal")))),
      df$course_text,
      sapply(df$section, function(x) paste(paste(x$course_code, x$section), collapse = " / ")),
      df$course_title,
      df$course_desig,
      df$course_type
    )[rep(seq_along(times), times),],
    do.call(rbind, df$meeting),
    rep(round(1/times, 2), times) 
  )
  names(out_format) <- c(
    "Year quarter","Academic year","Academic quarter","Fiscal year","Fiscal quarter",
    "Course code","Section codes","Course abbreviation","Designation","Course type",
    "Day","Time","Location","Count weight"
  )
  return (out_format)
}

instructor <- function (schedule) {
  if (length(schedule) == 0) return (NULL)
  df <- format_raw(schedule)
  times <- sapply(df$instructor, length)
  out_format <- cbind(
    data.frame(
      sapply(df$quarter, format),
      sapply(df$quarter, function(x) year(ed_quarter_mode(x, "academic"))),
      sapply(df$quarter, function(x) quarter(ed_quarter_mode(x, "academic"))),
      sapply(df$quarter, function(x) as.integer(year(ed_quarter_mode(x, "fiscal")))),
      sapply(df$quarter, function(x) as.integer(quarter(ed_quarter_mode(x, "fiscal")))),
      df$course_text,
      sapply(df$section, function(x) paste(paste(x$course_code, x$section), collapse = " / ")),
      df$course_title,
      df$course_desig,
      df$course_type
    )[rep(seq_along(times), times),],
    do.call(c, df$instructor),
    rep(round(1/times, 2), times) 
  )
  names(out_format) <- c(
    "Year quarter","Academic year","Academic quarter","Fiscal year","Fiscal quarter",
    "Course code","Section codes","Course abbreviation","Designation","Course type",
    "Instructor","Count weight"
  )
  return (out_format)
}

section <- function (schedule) {
  if (length(schedule) == 0) return (NULL)
  df <- format_raw(schedule)
  times <- sapply(df$section, nrow)
  out_format <- cbind(
    data.frame(
      sapply(df$quarter, format),
      sapply(df$quarter, function(x) year(ed_quarter_mode(x, "academic"))),
      sapply(df$quarter, function(x) quarter(ed_quarter_mode(x, "academic"))),
      sapply(df$quarter, function(x) as.integer(year(ed_quarter_mode(x, "fiscal")))),
      sapply(df$quarter, function(x) as.integer(quarter(ed_quarter_mode(x, "fiscal")))),
      df$course_title,
      df$course_desig,
      df$course_type,
      sapply(df$instructor, function(x) paste(x, collapse = " / "))
    )[rep(seq_along(times), times),],
    do.call(rbind, df$section),
    rep(round(1/times, 2), times) 
  )
  names(out_format) <- c(
    "Year quarter","Academic year","Academic quarter","Fiscal year","Fiscal quarter",
    "Course abbreviation","Designation","Course type","Instructor",
    "SLN","Course code","Section","Credits","GE requirement","Enrolled","Limit","Count weight"
  )
  return (out_format)
}

format_raw <- function (schedule) {
  df <- vector("list", length(schedule))
  jo <- list()
  for (i in seq_along(schedule)) {
    x <- schedule[[i]]
    enrollment <- joint_offer_enrollment(x, schedule)
    if (nrow(enrollment) > 1) {
      jo[[length(jo) + 1]] <- list(index = i, quarter = x$quarter, enrollment = enrollment)
    }
    df[[i]] <- data.frame(
      quarter      = I(list(x$quarter)),
      course_text  = x$course_text,
      course_title = x$course_title,
      course_desig = x$sln_desig,
      course_type  = x$data$sln$Type[1],
      section      = I(list(enrollment)),
      instructor   = I(list(str_to_upper(x$data$meetings$Instructor[x$data$meetings$Instructor != ""]))),
      meeting      = I(list(parse_meetings(x$data$meetings)))
    )
  }
  df <- do.call(rbind, df)
  
  # Remove duplicate joint offer courses
  remove_index <- get_remove_index(jo)
  if (length(remove_index) > 0) {
    df <- df[-remove_index]
  }
  
  return (df)
}

joint_offer_enrollment <- function (x, schedule) {
  jo_recur <- function (jo_enroll, jo, schedule) {
    if (is.null(jo)) {
      return (jo_enroll)
    }
    for (i in seq_along(jo$data)) {
      if (length(jo$data[[i]]$sln$SLN[1]) == 0 ||
          jo$data[[i]]$sln$SLN[1] == "" || 
          jo$data[[i]]$sln$SLN[1] %in% jo_enroll[,"sln"]) {
        next
      }
      jo_enroll <- rbind(
        jo_enroll,
        data.frame(sln         = jo$data[[i]]$sln$SLN[1],
                   course_code = jo$data[[i]]$sln$Course[1], 
                   section     = jo$data[[i]]$sln$Section[1],
                   credits     = jo$data[[i]]$sln$Credits[1],
                   ge_req      = jo$data[[i]]$sln$`General Educationand Basic SkillsRequirements`,
                   enrolled    = as.integer(jo$data[[i]]$enrollment$CurrentEnrollment[1]),
                   limit       = as.integer(jo$data[[i]]$enrollment$Limit[1]))
      )
      next_x <- schedule[sapply(schedule, "[[", "sln_text") == jo$data[[i]]$sln$SLN[1]]
      if (length(next_x) == 0) {
        next
      }
      jo_enroll <- jo_recur(jo_enroll, next_x[[1]]$data$joint_offer, schedule)
    }
    return (jo_enroll)
  }
  jo_enroll <- data.frame(sln         = x$sln_text,
                          course_code = x$data$sln$Course[1],
                          section     = x$data$sln$Section[1],
                          credits     = x$data$sln$Credits[1],
                          ge_req      = x$data$sln$`General Educationand Basic SkillsRequirements`,
                          enrolled    = x$data$enrollment$CurrentEnrollment[1],
                          limit       = x$data$enrollment$Limit[1])
  return (jo_recur(jo_enroll, 
                   x$data$joint_offer, 
                   schedule[sapply(schedule, "[[", "quarter") == x$quarter]))
}

remove_blanks <- function (text) {
  text <- text[str_length(text) > 0]
  if (length(text) == 0) {
    text <- NA
  }
  return (text)
}

get_remove_index <- function (jo) {
  if (length(jo) == 0) return (NULL)
  jo <- split(jo, sapply(jo, "[[", "quarter"))
  remove_index <- lapply(jo, function (q) {
    remove_index <- NULL
    if (length(q) < 2) return (remove_index)
    for (i in 1:(length(q)-1)) {
      for (j in (i+1):length(q)) {
        sln_i <- q[[i]]$enrollment[,"sln"]
        sln_j <- q[[j]]$enrollment[,"sln"]
        if (any(sln_i %in% sln_j)) {
          if (length(sln_i) < length(sln_j)) {
            remove_index <- c(remove_index, q[[i]]$index)
          } else {
            remove_index <- c(remove_index, q[[j]]$index)
          }
        }
      }
    }
    return (remove_index)
  })
  do.call(c, remove_index)
}
