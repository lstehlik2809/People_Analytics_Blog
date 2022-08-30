library(tidyverse)
library(lubridate)

# uploading data
data <- readr::read_delim("./multiTimeline.csv", delim = ",")

# data munging
mydata <- data %>%
  dplyr::slice(-1) %>%
  tidyr::separate(
    col = `Category: All categories`, 
    into = c("date", "peopleAnalyticsInterest", "hrAnalyticsInterest", "workforceAnalyticsInterest"),
    sep = ","
  ) %>%
  dplyr::mutate(
    peopleAnalyticsInterest = as.numeric(peopleAnalyticsInterest),
    hrAnalyticsInterest = as.numeric(hrAnalyticsInterest),
    interestInPeopleAnalytics = peopleAnalyticsInterest + hrAnalyticsInterest,
    interestInPeopleAnalytics = normalize(interestInPeopleAnalytics)*100
  ) %>%
  # changing the format and name of the Month variable
  dplyr::mutate(
    date = stringr::str_glue("{date}-01"),
    date = lubridate::ymd(date)
  ) %>%
  # creating new variable month
  dplyr::mutate(
    month = lubridate::month(date,label = TRUE, abbr = TRUE),
    month = factor(month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"), labels = c("Jan","Feb","Mar","Apr","May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec"), ordered = FALSE)
  ) %>%
  # limiting the time window from 2007-01-01 to 2022-07-01
  dplyr::filter(
    date <= "2022-07-01",
    date >= "2007-01-01"
  ) %>%
  # arranging data in ascending order by date
  dplyr::arrange(
    date
  ) %>%
  # creating new variables elapsed time, pandemic, and time elapsed after pandemic outbreak
  dplyr::mutate(
    elapsedTime = row_number(),
    pandemic = case_when(
      date >= '2020-03-01' ~ 1,
      TRUE ~ 0
    ),
    elapsedTimeAfterPandemic = cumsum(pandemic)
  ) %>%
  dplyr::mutate(
    pandemic = as.factor(ifelse(pandemic == 1, "After the pandemic outbreak", "Before the pandemic outbreak"))
  ) %>%
  # final selection of variables
  dplyr::select(
    date, interestInPeopleAnalytics, elapsedTime, month, pandemic, elapsedTimeAfterPandemic
  )
