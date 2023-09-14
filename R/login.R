#' @export
get_selenium_driver <- function () {
  RSelenium::rsDriver(browser = "firefox", chromever = NULL, verbose = FALSE)
}

#' @export
manual_login <- function (driver) {

  # Use any page to elicit login
  webpage <- "https://sdb.admin.uw.edu/timeschd/UWNetID/sln.asp?QTRYR=AUT+2021&SLN=10862"
  driver$client$navigate(webpage)
  cat("Please log in from web browser...")

  while(driver$client$getCurrentUrl() != webpage) {
    Sys.sleep(0.5)
  }
  cat("login complete.\n")
}

#' @export
start_selenium <- function () {
  driver <- get_selenium_driver()
  manual_login(driver)
  cookies <- paste(lapply(driver$client$getAllCookies(), function(x) {
    paste0(x$name,"=",x$value)
  }), collapse = "; ")
  list(driver = driver, cookies = cookies)
}

#' @export
stop_selenium <- function (selenium) {
  selenium$driver$client$close()
  selenium$driver$server$stop()
  selenium$driver  <- NULL
  selenium$cookies <- NULL
}

#' @export
close_all_ports <- function () {
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
}
