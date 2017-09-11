library(tidyverse)


# 21.2 For loops ----------------------------------------------------------

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10), 
  d = rnorm(10)
)

output <- vector("double", ncol(df))
median(df[[1]])
median(df[1])
df[1]
seq_along(0)
1:length(0)
y <- vector("double", 0)
seq_along(y)
1:length(y)

for(i in seq_along(df)) {
  output[[i]] <- median(df[[i]])
}
output
seq_along(df)

# 21.2.1 Exercises
#1
output <- vector("double", ncol(mtcars))
for(i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[[i]])
}
output

output <- vector("list", ncol(nycflights13::flights))
for(i in seq_along(nycflights13::flights)) {
  output[[i]] <- class(nycflights13::flights[[i]])
}
names(output) <- names(nycflights13::flights)
output

output <- vector("integer", ncol(iris)) 
for(i in seq_along(iris)) {
  output[[i]] <- length(unique(iris[[i]]))
}
output

n <- 10
mu <- c(-10, 0, 10, 100)
output <- vector("list", length(mu))
for(i in seq_along(mu)) {
  output[[i]] <- rnorm(n, mu[[i]])
}
names(output) <- mu
output

#2
library(stringr)
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out
?str_c
str_c(letters)
str_c(letters, collapse = "")


x <- sample(100)
sd <- 0
for (i in seq_along(x)) {
  sd <- sd + (x[i] - mean(x)) ^ 2
}
sd <- sqrt(sd / (length(x) - 1))
sd
sd(x)

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
out
cumsum(x)
all_equal(x, out)
all_equal(cumsum(x), out)

#3
humps <- 5
hump_line <- function(h) {
  if(h > 1) {
    str_c("Alice the camel has ", h, " humps.")
  } else if(h == 1) {
    str_c("Alice the camel has ", h, " hump.")
  } else {
    str_c("Alice the camel has no humps.")
  }
}
for(i in humps:0) {
  for(j in 1:3) {
    writeLines(hump_line(i))
  }
  if(i > 0) {
    writeLines("So go, Alice, go.\n")
  } else {
    writeLines("Now Alice is a horse.")
  }
}

number_in_bed <- 10
how_many_in_bed <- function(x) {
  str_c("There were ", x, " in bed.\n",
        "And the little one said.")
}

for(i in number_in_bed:1) {
  if(i > 1) {
    writeLines(how_many_in_bed(i))
    writeLines("Roll over. Roll over.\n")
  } else {
    writeLines(how_many_in_bed(i))
    writeLines("I'm lonely.")
  }
}

vessels <- 99
bottles_on_wall <- function(n, liquid) {
  for(i in n:1) {
    writeLines(str_c(i, " bottles of ", liquid, " on the wall"))
    writeLines(str_c(i, " bottles of ", liquid))
    writeLines(str_c("Take one down, pass it around."))
    writeLines(str_c(i-1, " bottles of ", liquid, " on the wall.\n"))
  }
}
bottles_on_wall(3, "baking soda water")
microbenchmark::microbenchmark(bottles_on_wall(3, "baking soda water"))
system.time(bottles_on_wall(3, "baking soda water"))
system.time(bottles_on_wall(1000000, "baking soda water"))
bottles_on_wall_no_print <- function(n, liquid) {
  for(i in n:1){
    
  }
}
bottles_on_wall_no_print(1000000, "baking soda water")
system.time(bottles_on_wall_no_print(1000000, "baking soda water"))


output <- vector("integer", 0)
for (i in seq_along(x)) {
  output <- c(output, lengths(x[[i]]))
}
output
no_preallocate <- function(n) {
  x <- sample(n)
  output <- vector("integer", 0)
  for (i in seq_along(x)) {
    output <- c(output, x[[i]])
  }
  return(output)
}
output <- no_preallocate(1000)
length(output)

preallocation <- function(n) {
  x <- sample(n)
  output <- vector("integer", n)
  for (i in seq_along(x)) {
    output[[i]] <- x[[i]]
  }
  output
}
microbenchmark::microbenchmark(no_preallocate(1000), preallocation(1000))
?microbenchmark:: microbenchmark

# 21.3 For loop variations ------------------------------------------------

seq_along(letters)
n <- sample(100, 1)
n
rnorm(n, 0)
my_list <- list(
  a = c(0, 1, 2),
  b = c("here", "there"),
  d = sample(10)
)
my_list
unlist(my_list)
length(unlist(my_list))
?purrr


# 21.4 For loops versus functionals ---------------------------------------

# 21.4.1 Exercises
# 1
?apply

X

# 21.5 The map functions --------------------------------------------------

?map
map(df, mean)
df
df %>% map(mean)

# 21.5.3 Exercises
#1
mtcars %>% map_dbl(mean)

nycflights13::flights %>% map(class)
nycflights13::flights %>% map_chr(typeof)

iris %>% map_int(~ length(unique(.)))
?map
# Or a formula
1:10 %>%
  map(~ rnorm(10, .x))

map(c(-10, 0, 10, 100), rnorm, n = 10)
?rnorm
c(-10, 0, 10, 100) %>% map(rnorm, n = 10)
c(-10, 0, 10, 100) %>% map(rnorm(n = 10))
rnorm(c(-10, 0, 10, 100), n = 10)

#2
mtcars %>% map_lgl(is.factor)

#3
1:5 %>% map(runif)
map(1:5, runif)
map(list(1:5), runif)

#4
map(-2:2, rnorm, n = 5)
map_dbl(-2:2, rnorm, n = 5)

#5
map(x, function(df) lm(mpg ~ wt, data = df))
mtcars %>% map(~ lm(mpg ~ wt, data = .))
names(mtcars)
map(list(mtcars), ~ lm(mpg ~ wt, data = .))
list(mtcars) %>% 
       map(~ lm(mpg ~ wt, data = .))
typeof(mtcars)
length(mtcars)
length(list(mtcars))

# 21.6 Dealing with failure -----------------------------------------------

safe_log <- safely(log)
str(safe_log(10))
str(safe_log("a"))

x <- list(1, 10, "a")
y <- x %>% map(safely(log))
str(y)
y <- y %>% transpose()
str(y)

is_ok <- y$error %>% map_lgl(is_null)
is_ok
x[!is_ok]
y$result[is_ok] %>% flatten_dbl()
?flatten_dbl

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))
?possibly

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()
log(x)
map(x, log)
log(-1)
quietly(log(-1))
map(-1, quietly(log))
map(-1, log)

# 21.9 Other patterns of for loops ----------------------------------------

str(iris)
iris %>% 
  keep(is.factor) %>% 
  str()
iris %>% 
  discard(is.factor) %>% 
  str()

x <- list(1:5, letters, list(10))
x
x %>% some(is.character)
x %>% every(is.vector)
x[3]
x[[3]]

x <- sample(10)
x
x %>% 
  detect(~ . > 5)
x %>% 
  detect_index(~ . > 5)
x %>% 
  head_while(~ . < 5)
x %>% 
  tail_while(~ . > 5)


# 21.9.2 Reduce and accumulate --------------------------------------------

dfs <- list(
  age = tibble(name = "John", age = 30),
  sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
  trt = tibble(name = "Mary", treatment = "A")
)
dfs
dfs %>% 
  reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)

vs %>% reduce(intersect)
x %>% accumulate(`+`)

# 21.9.3 Exercises
#1
