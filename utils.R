library(dplyr)

count_numbers = function(data) {
  if (!is.numeric(data)) stop("Need numeric input data")
  table(data, useNA = "always")
}

count_by_state = function(data) {
  data %>%
    group_by(STATEFIP) %>%
    summarise(count = n(), count_CICMPGAM = sum(CICMPGAM %in% c(1, 2)))
}
