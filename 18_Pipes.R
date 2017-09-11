library(magrittr)

# 18.2 Piping alternatives ====
foo_foo <- little_bunny()
?stop
?tryCatch

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()
rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

mtcars %>% 
  cor(disp, mpg)
mtcars %$%
  cor(disp, mpg)
