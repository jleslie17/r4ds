library(tidyverse)


# 20.2 Vector basics ------------------------------------------------------

typeof(letters)
typeof(1:10)
x <- list("a", "b", 1:10)
length(x)

# 20.3 Important types of atomic vectors ----------------------------------

typeof(1)
typeof(1L)
typeof(1.5L)
1.5L

x <- sqrt(2) ^ 2
x == 2
x - 2
dplyr::near(x, 2)

# 20.3.5 Exercises
# 1
?is.finite

# 2
dplyr::near
.Machine$double.rounding
?.Machine

# 5
parse_integer(c("1", "1", "NA"))
parse_logical(c("true", "TRUE"))
parse_logical(c("TRUE", "FALSE", "1", "0", "true", "t", "NA"))

# 20.4 Using atomic vectors -----------------------------------------------

# 20.4.1 Coercion
x <- sample(20, 100, replace = TRUE)
?sample
y <- x > 10
sum(y)
mean(y)
length(x) > 0

# 20.4.2 Test functions
# 20.4.3 Scalars and recycling rules

tibble(x = 1:4, y = 1:2)
tibble(x = 1:4, y = rep(1:2, 2))
tibble(x = 1:4, y = rep(1:2, each = 2))

# 20.4.4 Naming vectors
# All types of vectors can be named. You can name them during creation with c():
c(x = 1, y = 2, z = 4)
# Or after the fact with purrr::set_names():
set_names(1:3, c("a", "b", "c"))

# 20.4.6 Exercises
# 4
last_value <- function(x) {
  if(length(x)){
    x[[length(x)]]
  } else {
    x
  }
}
last_value(1:10)
last_value(1)
last_value(numeric())

even_values <- function(x) {
  if(length(x)){
    x[seq_along(x) %% 2 == 0]
  } else {
    x
  }
}
even_values(1:10)
?seq_along

# test using case to ensure that values not indices
# are being returned
even_values(letters)

not_last <- function(x){
  if(length(x)){
    x[-length(x)]
  } else {
    x
  }
}
not_last(1:10)

even_numbers <- function(x) {
  if(length(x)){
    x[!is.na(x) & x %% 2 == 0]
  } else {
    x
  }
}
even_numbers(1:10)
even_numbers(c(1:10, NA, NaN, Inf))
# 5
x <- c(-5:5, Inf, -Inf, NaN, NA)
x[-which(x > 0)]
x[x <= 0]
# 6
(1:10)[11:12]
c(a = 1, 2)[["b"]]

# 20.5 Recursive vectors (lists) ------------------------------------------

# 20.5.2 Subsetting
a <- list(a = 1:3, b = "a string", c = pi, d = list(-1, -5))
a
str(a[1:2])
str(a[[1]])
a$a
a[["a"]]
str(a[[4]])
str(a[[4]][1])
str(a[[4]][[1]])

# 20.5.4 Exercises
#1
x <- tibble(a = 1:2, b = 3:4)
x
x["a"]
x[["a"]]
str(x[["a"]])

# 20.6 Attributes ---------------------------------------------------------

x <- 1:10
attr(x, "greeting")
attr(x, "greeting") <- "hi"
attr(x, "farewell") <- "bye"
attr(x)
attributes(x)

as.Date
methods(as.Date)
getS3method("as.Date", "default")

# 20.7 Augmented vectors --------------------------------------------------

# 20.7.1 Factors
x <- factor(c("ab", "cd", "ab"), levels = c("ab", "cd", "ef"))
x
typeof(x)
attributes(x)

# 20.7.2 Dates and date-time
x <- as.Date("1971-01-01")
typeof(x)
unclass(x)
attributes(x)

x <- lubridate::ymd_hm("1970-01-01 01:00")
x
typeof(x)
attributes(x)
unclass(x)
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

y <- as.POSIXlt(x)
y
typeof(y)
attributes(y)
attributes(x)

# POSIXlts are rare inside the tidyverse. They do crop up in base R, because
# they are needed to extract specific components of a date, like the year or
# month. Since lubridate provides helpers for you to do this instead, you don’t
# need them. POSIXct’s are always easier to work with, so if you find you have a
# POSIXlt, you should always convert it to a regular data time
# lubridate::as_date_time().

# 20.7.3 Tibbles
tb <- tibble::tibble(x = 1:5, y = 5:1)
tb
attributes(tb)
typeof(tb)

df <- data.frame(x = 1:5, y = 5:1)
typeof(df)
attributes(df)

# 20.7.4 Exercises
#1
hms::hms(3600)
x <- hms::hms(360)
typeof(x)
attributes(x)

#2
tb <- tibble(x = 1:5, y = 1:6)

#3
tb <- tibble(x = list(a = 1:4, b = letters),
             y = 1)
tb
