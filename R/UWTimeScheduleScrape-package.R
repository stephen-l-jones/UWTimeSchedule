sln_url_prefix      <- "https://sdb.admin.washington.edu/timeschd/uwnetid/sln.asp"
sln_url_prefix2     <- "https://sdb.admin.uw.edu/timeschd/UWNetID/sln.asp"
schedule_url_prefix <- "https://www.washington.edu/students/timeschd"

q_quarter <- c("AU","WI","SP","SU")
i_quarter <- as.character(1:4)
Q_quarter <- c("AUT","WIN","SPR","SUM")
N_quarter <- c("Autumn","Winter","Spring","Summer")
q_pattern <- paste0("^(", paste(q_quarter, collapse = "|"), ")")
i_pattern <- paste0("^(", paste(i_quarter, collapse = "|"), ")")
Q_pattern <- paste0("^(", paste(Q_quarter, collapse = "|"), ")")
N_pattern <- paste0("^(", paste(stringr::str_to_upper(N_quarter), collapse = "|"), ")")

exclude_body_text <- c("Display Textbooks"," Subscribe to seat availability notifications")
exclude_header_text <- c("Meetings")
designation_position <- 114
joint_offer_text <- "^Offered jointly with "
