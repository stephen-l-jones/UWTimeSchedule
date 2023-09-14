
#' @import rvest
scrape_schedule <- function (
    quarter, course_prefix, course_number = NULL, campus = c("bothell","tacoma","seattle"), cookies
) {
  quarter <- as.ed_quarter(quarter)
  campus  <- match.arg(campus)
  if (!is.null(course_number)) {
    course_filter <- paste0(course_prefix, stringr::str_trim(course_number))
  } else {
    course_filter <- NULL
  }
  sln_list <- list()
  jo_sln_list <- list()

  for (i in seq_along(quarter)) {
    qtr <- quarter[i]
    for (j in seq_along(course_prefix)) {
      pfx <- course_prefix[j]
      cat(sprintf("Retrieving %s %s...", format(qtr), pfx))

      # Scrape course
      schedule_tree <- get_schedule(qtr, pfx, campus)
      if (is.null(schedule_tree)) {
        cat("No data found.\n")
        next
      }
      schedule <- parse_schedule(schedule_tree, qtr, pfx, course_filter)

      # Scrape SLN
      for (k in seq_along(schedule)) {
        sln <- schedule[[k]]
        sln_tree <- get_sln(sln$sln_href, cookies)
        sln$data <- parse_sln(sln_tree, qtr, sln$sln_text)

        # Scrape joint offerings
        if (any(names(sln$data) == "joint_offer")) {
          sln$data$joint_offer$data <- lapply(sln$data$joint_offer$href, function (href) {
            sln_tree <- get_sln(href, cookies)
            parse_sln(sln_tree, qtr, sln$sln_text)
          })
        }
        schedule[[k]] <- sln
      }
      sln_list <- c(sln_list, schedule)
      cat("complete.\n")
    }
  }
  return(sln_list)
}

#' @import rvest
parse_schedule <- function (schedule_tree, quarter, course_prefix, course_filter) {
  schedule_table <- html_elements(schedule_tree, "table")
  course_select  <- paste0("a[name^=\"", course_prefix, "\"]")
  sln_select     <- paste0("a[href^=\"", sln_url_prefix, "\"]")
  sln_list       <- list()
  add_course     <- FALSE

  # Scrape course and SLN
  for (k in seq_along(schedule_table)) {
    tbl <- schedule_table[k]
    course_elem <- html_element(tbl, course_select)
    if (length(course_elem) > 0 && !is.na(course_elem)) {
      course_name  <- html_attr(course_elem, "name")
      course_text  <- html_text2(course_elem)
      course_title <- html_text2(html_element(tbl, "a[href*=\"crscat\"]"))
      add_course   <- (is.null(course_filter) || course_name %in% course_filter)
      next
    }
    if (!add_course) {
      next
    }
    sln_elem <- html_element(tbl, sln_select)
    if (length(sln_elem) == 0 || is.na(sln_elem)) {
      add_course <- FALSE
      next
    }
    sln_href  <- html_attr(sln_elem, "href")
    sln_text  <- html_text2(sln_elem)
    sln_desig <- str_squish(str_sub(str_remove(str_split(html_text(tbl), "\\n")[[1]][2], "\\r"),
                                    designation_position, -1))
    sln_list <- c(sln_list, list(list(
      quarter       = quarter,
      course_name   = course_name,
      course_text   = course_text,
      course_title  = course_title,
      sln_href      = sln_href,
      sln_text      = sln_text,
      sln_desig     = sln_desig
    )))
  }
  return (sln_list)
}

#' @import rvest
get_schedule <- function (
    quarter, course_prefix, campus = c("bothell","tacoma","seattle")
) {
  quarter <- ed_quarter(quarter, mode = "calendar")
  campus <- match.arg(campus) %>%
    switch (
      bothell = "/B",
      tacoma  = "/T",
      ""
    )
  schedule_url <- paste0(
    schedule_url_prefix,
    campus,
    "/", format(quarter, "%Q%Y")[1],
    "/", course_prefix,
    ".html"
  )
  schedule_tree <- NULL
  tryCatch(
    {
      schedule_tree  <- read_html(schedule_url)
    },
    error = function(e) {
      warning(sprintf("URL not found: %s", schedule_url))
    }
  )
  return(schedule_tree)
}

#' @import httr2
#' @import stringr
parse_sln <- function (sln_tree, quarter, sln_text) {
  sln_table <- html_elements(sln_tree, "table[class=\"main\"]")
  sln_data <- vector(mode = "list", 4)
  names(sln_data) <- c("sln","enrollment","meetings","notes")

  for (i in seq_along(sln_table)) {
    headers <- html_elements(sln_table[i], "th")
    headers <- headers[!(html_text(headers) %in% exclude_header_text)]
    if (length(headers) == 0) {
      next
    }
    header_span <- html_attr(headers, "colspan")
    header_span <- ifelse(is.na(header_span), 1, header_span)
    header_text <- rep(html_text(headers), header_span)
    body <- html_elements(sln_table[i], "td")
    body <- body[!(html_text(body) %in% exclude_body_text)]
    xml2::xml_add_sibling(xml2::xml_find_all(body, ".//br"), "p", "\n")
    body_span <- html_attr(body, "colspan")
    body_span <- ifelse(is.na(body_span), 1, body_span)
    body_text <- rep(html_text(body), body_span)

    if (length(header_text) != length(body_text)) {
      warning(sprintf(
        "Table header columns [%s] not the same length as body columns [%s] (%s, SLN = %s, %s table)",
        length(header_text), length(body_text),
        format(quarter), sln_text, html_text(headers[1])
      ))
      len_diff <- length(body_text) - length(header_text)
      if (len_diff > 0) {
        header_text <- c(header_text, rep(" ", len_diff))
      } else {
        body_text <- c(body_text, rep(" ", len_diff * -1))
      }
    }
    body_text <- lapply(str_split(body_text, "\\n"), str_squish)
    names(body_text) <- header_text
    header_id <- html_text(headers[1])
    if (header_id == "SLN") {
      sln_data$sln <- body_text
    } else if (header_id == "CurrentEnrollment") {
      sln_data$enrollment <- body_text
    } else if (header_id == "Days") {
      sln_data$meetings <- body_text
    } else if (header_id == "Notes") {
      sln_data$notes <- body_text
      joint_offer <- parse_joint_offer(body_text$Notes)
      if (!is.na(joint_offer)) {
        sln_data$joint_offer <- list()
        sln_data$joint_offer$text <- joint_offer
        sln_data$joint_offer$href <- parse_joint_offer_href(body)
      }
    } else {
      warning(sprintf(
        "Ignoring table with header %s (%s, SLN = %s)",
        html_text(headers[1]), format(quarter), sln_text
      ))
    }
  }
  return(sln_data)
}

get_sln <- function (sln_href, cookies) {
  req <- request(sln_href)
  req <- req_headers(req, Cookie = cookies)
  resp <- req_perform(req)
  sln_tree <- NULL
  tryCatch(
    {
      sln_tree  <- read_html(resp_body_string(resp))
    },
    error = function(e) {
      warning(sprintf("URL not found: %s", sln_href))
    }
  )
  return(sln_tree)
}


parse_joint_offer <- function (notes) {
  joint <- str_trim(str_remove(str_subset(notes, joint_offer_text), joint_offer_text))
  if (length(joint) == 0) {
    joint <- NA_character_
  }
  return (joint)
}

parse_joint_offer_href <- function (body) {
  a_elems <- html_elements(body, "a")
  hrefs <- sapply(a_elems, html_attr, name = "href")
  hrefs[str_detect(hrefs, sln_url_prefix2)]
}

parse_meetings <- function (meetings) {
  meetings <- meetings[c("Days","Time","Location")]
  meetings <- lapply(meetings, str_replace, "(To be arranged|T\\.B\\.A\\.)", "*")
  meetings <- as.data.frame(meetings)
  meetings <- meetings[str_length(meetings$Days) > 0
                       | str_length(meetings$Time) > 0
                       | str_length(meetings$Location) > 0,]
  meetings <- do.call(rbind, apply(as.matrix(meetings), 1, function(row) {
    row <- unname(row)
    data.frame(day = parse_day(row[1]), time = row[2], location = row[3])
  }, simplify = FALSE))
  return (meetings)
}

parse_day <- function (day) {
  day <- str_trim(str_replace(day, "Th", "R"))
  day <- str_replace(str_split_1(day, ""), "R", "Th")
  return (day)
}
