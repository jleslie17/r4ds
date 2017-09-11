x <- c(1:10, Inf)

# 19.2.1 Exercises
#1
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x)

#2
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == Inf] <- 1
  y[y == -Inf] <- 0
  return(y)
}
rescale01(x)

#3
mean(is.na(x))

#4
variance <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  sq_error <- (x - m) ^ 2
  sum(sq_error)/(n - 1)
}
var(1:10)
variance(1:10)

skewness <- function(x) {
  x <- x[!is.na(x)]
  m <-  mean(x)
  n <- length(x)
  cub_error <- (x - m) ^ 3
  m3 <- sum(cub_error)/n
  sq_error <- (x - m) ^ 2
  s_cubed <- sqrt(sum(sq_error)/(n - 1))
  m3/s_cubed
}

skewness2 <- function(x) {
  x <- x[!is.na(x)] 
  n <- length(x)
  m <- mean(x)
  m3 <- sum((x - m) ^ 3) / n
  s3 <- sqrt(sum((x - m) ^ 2) / (n - 1))
  m3 / s3
}
x <- rgamma(10, 1, 1)
skewness(x)
skewness2(x)

#5
both_na <- function(x, y) {
  sum(is.na(x) & is.na(y))
}
both_na(c(NA, NA,  1, 2),
        c(NA,  1, NA, 2))
both_na(c(NA, NA,  1, 2, NA, NA, 1), 
        c(NA,  1, NA, 2, NA, NA, 1))

# test section break ------------------------------------------------------


#6
?file.info
?file.access
# Load data --------------------------------------

# Plot data --------------------------------------


# 19.4 Conditional execution ----------------------------------------------

?`if`
?switch
?filter
?`ifelse`
x <- c(6:-4)
x
sqrt(x)
sqrt(ifelse(x >= 0, x, NA))

#3
x <- 15
if(x %% 3 == 0) {
  print('fizz')
}
if(x %% 5 == 0) {
  print("buzz")
}
if(x %% 3 == 0 & x %% 5 == 0) {
  print("fizzbuzz")
}
!(x %% 3)

fizzbuzz <- function(x) {
  if(!(x %% 3) & !(x %% 5)) {
    print("fizzbuzz")
  } else if(!(x %% 3)) {
    print("fizz")
  } else if(!(x %% 5)) {
    print("buzz")
  } else {
    print(x)
  }
}
fizzbuzz(15)
fizzbuzz(6)
fizzbuzz(10)
fizzbuzz(13)

#4
?cut
Z <- stats::rnorm(10000)
table(cut(Z, breaks = -6:6))
temp <- seq(-10, 50, by = 5)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf), 
    labels = c("freezing", "cold", "cool", "warm", "hot"),
    right = TRUE)
cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
    labels = c("freezing", "cold", "cool", "warm", "hot"),
    right = FALSE)

#5
?switch
a <- 2
switch(a, "one", "two", "three")
b <- c("one", "two", "three")
switch(a, b)

#6
switch(x, 
       a = ,
       b = "ab",
       c = ,
       d = "cd"
)
x <- "a"
switcheroo <- function(x) {
  switch(x, 
         a = ,
         b = "ab",
         c = ,
         d = "cd"
  )
}
switcheroo("a")
switcheroo("b")
switcheroo("c")
switcheroo("d")
switcheroo("e")

# 19.5 Function arguments -------------------------------------------------

?log
?mean

?stop
?stopifnot

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output")
# 19.5.5 Exercises
# 1
commas(letters, collapse = "-")

# 2
rule("Title", pad = "-+")
