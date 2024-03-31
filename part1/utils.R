parse_dates <- function(date_string) {
  formats <- c("%Y/%m/%d", "%d %b %Y", "%Y-%m-%d", "%m/%d/%Y, %H:%M:%S", "%d/%m/%Y, %H:%M:%S")

  for (format in formats) {
    parsed_date <- as.POSIXct(date_string, format = format, tz = "")
    if (!is.na(parsed_date)) {
      # Return the date in "28 Aug 2016" format
      return(format(parsed_date, "%d %b %Y"))
    }
  }

  # Return NA if no formats matched
  return(NA)
}
