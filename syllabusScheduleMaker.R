syllabusScheduleMaker <- function(
  year = 2022,
  day.start = "01-10",
  day.fin = "04-27",
  meeting.days = "Tue|Thu",
  file.name = "2022-class.csv",
  save.csv = TRUE
){
  date.1 <- as.POSIXlt(paste(year,day.start, sep="-"), format="%Y-%m-%d")
  # makes start date as POSIXlt object
  date.2 <- as.POSIXlt(paste(year,day.fin, sep="-"), format="%Y-%m-%d")
  # makes end date as POSIXlt object
  dates <- seq.Date(from = as.Date(date.1),to = as.Date(date.2),by = "day")
  # generates a sequence from start to end date
  years <- format(dates, "%Y")
  # extracts year vector 
  month <- months(dates,abbreviate = T)
  # extracts month vector
  days.n <- format(dates, "%d")
  # extracts day number vector
  days <- weekdays(dates,abbreviate = T)
  # produces vector of 3 letter day abbreviations
  df<- data.frame(dates, years,month, days.n, days, days.left = length(days.n):1) 
  # makes data frame where each columan is date, year, month, day number, days left in semester, day abbr
  df.final <- df[ grepl(pattern = meeting.days, x = df$days,perl = F), ]
  # drop all days of the week when class doesn't meet (that don't match metting.days)
  df.final <- cbind.data.frame(df.final, class.meeting = 1:nrow(df.final))
  # add class meeting number column
  
  if(save.csv == TRUE) {write.table(x = df.final, file = file.name, sep = ",",row.names = F)}
  df.final
}

syllabusScheduleMaker(year = 2022,day.start = "01-10",day.fin = "04-27",
                      meeting.days = "Tue|Thu",file.name = "2022.Spring-NeuroMethod.csv",save.csv = F)