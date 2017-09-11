library(modelr)
library(tidyverse)


# 25.2 Gapminder ----------------------------------------------------------


library(gapminder)
gapminder


?broom::glance
?map
1:10 %>% 
  map(rnorm, n = 10) %>% 
  map_dbl(mean)
?rnorm

gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")
nz

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")
nz

nz %>% 
  add_residuals(nz_mod) %>% 
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")
nz
nz %>% 
  add_predictions(nz_mod)
nz %>% 
  add_residuals(nz_mod)

by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()

by_country
by_country$data[1]

# 25.2.2 List-columns
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}
models <- map(by_country$data, country_model)
by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country
by_country$model[1]
by_country %>% 
  filter(continent == "Europe")
by_country %>% 
  arrange(continent, country)

# 25.2.3 Unnesting

by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
by_country
?map2
by_country$resids[1]
by_country$resids[2]
resids <- unnest(by_country, resids)
resids
?unnest
by_country
gapminder
resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

# 24.2.4 Model quality
broom::glance(nz_mod)
by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance)

glance <- by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance

glance %>% 
  arrange(r.squared)
glance %>% 
  ggplot(aes(continent, r.squared)) + 
  geom_jitter(width = 0.5)
glance %>% 
  ggplot(aes(continent, r.squared)) +
  geom_point()

bad_fit <- filter(glance, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()
gapminder
gapminder %>% 
  semi_join(bad_fit, by = "country")

# 25.2.5 Exercises
#1
gapminder
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 0.5)
country_model_quad <- function(df) {
  lm(lifeExp ~ year + I(year ^ 2), data = df)
}
models2 <- map(by_country$data, country_model_quad)
by_country
by_country <- by_country %>% 
  mutate(model2 = map(data, country_model_quad))
by_country
by_country$model2[1]

by_country <- by_country %>% 
  mutate(resids2 = map2(data, model2, add_residuals))
by_country

resids2 <- unnest(by_country, resids2)
resids2
gapminder
resids2 %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country)) +
  facet_wrap(~ continent)
glance2 <- by_country %>% 
  mutate(glance = map(model2, broom::glance)) %>% 
  unnest(glance, .drop = TRUE)
glance2
glance2 %>% 
  arrange(r.squared)
glance2 %>% 
  ggplot(aes(continent, r.squared)) +
  geom_jitter(width = 0.5)

bad_fit2 <- glance2 %>% 
  filter(r.squared < 0.25)
gapminder %>% 
  semi_join(bad_fit2, by = "country") %>% 
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()


# 25.4 Creating list-columns ----------------------------------------------

# 25.4.1 With nesting
gapminder %>% 
  group_by(country, continent) %>% 
  nest()

# You can also use it on an ungrouped data frame, specifying which columns you want to nest:

gapminder %>% 
  nest(year:gdpPercap)
gapminder

# 25.4.2 From vectorised functions
df <- tribble(
  ~x1,
  "a,b,c", 
  "d,e,f,g"
) 
df
df %>% 
  mutate(x2 = stringr::str_split(x1, ","))
?stringr::str_split

# unnest() knows how to handle these lists of vectors:
  
df %>% 
  mutate(x2 = stringr::str_split(x1, ",")) %>% 
  unnest()

sim <- tribble(
  ~f,      ~params,
  "runif", list(min = -1, max = -1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim
sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

# 25.4.3 From multivalued summaries

mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = quantile(mpg))
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg)))
probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)
mtcars %>% 
  group_by(cyl) %>% 
  summarise(p = list(probs), q = list(quantile(mpg, probs))) %>% 
  unnest()
quantile(mtcars$mpg)
quantile(mtcars$mpg, probs)

# 25.4.4 From a named list

x <- list(
  a = 1:5,
  b = 3:4, 
  c = 5:6
) 
x
df <- enframe(x)
df

# no idea what this is doing:
df %>% 
  mutate(
    smry = map2_chr(name, value, ~ stringr::str_c(.x, ": ", .y[1]))
  )
?map2_chr
df$value[1]
stringr::str_c(df$name[1], ": ", as.character(df$value)[1])

#  25.4.5 Exercises
?fivenum
quantile(mtcars$mpg)
fivenum(mtcars$mpg)

# 3
mtcars %>% 
  group_by(cyl) %>% 
  summarise(q = list(quantile(mpg))) %>% 
  unnest()

# 4
mtcars %>% 
  group_by(cyl) %>% 
  summarise_each(funs(list))
?funs

# 25.5 Simplifying list-columns -------------------------------------------

# 25.5.1 List to vector
df <- tribble(
  ~x,
  letters[1:5],
  1:3,
  runif(5)
)
df
df %>% mutate(
  type = map_chr(x, typeof),
  length = map_int(x, length)
)

df <- tribble(
  ~x,
  list(a = 1, b = 2),
  list(a = 2, c = 4)
)
df %>% mutate(
  a = map_dbl(x, "a"),
  b = map_dbl(x, "b", .null = NA_real_)
)

# 25.5.2 Unnesting

tibble(x = 1:2, y = list(1:4, 1)) %>% unnest(y)

# Ok, because y and z have the same number of elements in
# every row
df1 <- tribble(
  ~x, ~y,           ~z,
  1, c("a", "b"), 1:2,
  2, "c",           3
)
df1
df1 %>% unnest(y, z)

# Doesn't work because y and z have different number of elements
df2 <- tribble(
  ~x, ~y,           ~z,
  1, "a",         1:2,  
  2, c("b", "c"),   3
)
df2
df2 %>% unnest(y, z)


# 25.6 Making tidy data with broom ----------------------------------------

broom::glance(mod_diamond)
broom::glance(mod1)
broom::tidy(mod1)
?broom::augment
summary(mod1)
broom::glance(nz_mod)
broom::tidy(nz_mod)
broom::augment(nz_mod)
