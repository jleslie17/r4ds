library(tidyverse)

library(modelr)
options(na.action = na.warn)


# 23.2 A very simple model ------------------------------------------------

ggplot(sim1, aes(x, y)) +
  geom_point()

models <- tibble(
  a1 = runif(250, -20, 40),
  a2 = runif(250, -5, 5)
)

ggplot(sim1, aes(x, y)) + 
  geom_abline(aes(intercept = a1, slope = a2), data = models, alpha = 1/4) +
  geom_point() 

model1 <- function(a, data) {
  a[1] + data$x * a[2]
}
model1(c(7, 1.5), sim1)
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}
measure_distance(c(7, 1.5), sim1)

sim1_dist <- function(a1, a2) {
  measure_distance(c(a1, a2), sim1)
}

models <- models %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))
models
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(models, rank(dist) <= 10)
  )

ggplot(models, aes(a1, a2)) +
  geom_point(data = filter(models, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist))

grid <- expand.grid(
  a1 = seq(-5, 20, length = 25),
  a2 = seq(1, 3, length = 25)
) %>% 
  mutate(dist = purrr::map2_dbl(a1, a2, sim1_dist))

grid %>% 
  ggplot(aes(a1, a2)) +
  geom_point(data = filter(grid, rank(dist) <= 10), size = 4, colour = "red") +
  geom_point(aes(colour = -dist)) 
?expand.grid

ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(
    aes(intercept = a1, slope = a2, colour = -dist), 
    data = filter(grid, rank(dist) <= 10)
  )

# You could imagine iteratively making the grid finer and finer until you
# narrowed in on the best model. But there’s a better way to tackle that
# problem: a numerical minimisation tool called Newton-Raphson search. The
# intuition of Newton-Raphson is pretty simple: you pick a starting point and
# look around for the steepest slope. You then ski down that slope a little way,
# and then repeat again and again, until you can’t go any lower. In R, we can do
# that with optim():
best <- optim(c(0, 0), measure_distance, data = sim1)
best$par
?optim
ggplot(sim1, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

# 23.2.1Exercises
#1
sim1a <- tibble(
  x = rep(1:10, each = 3),
  y = x * 1.5 + 6 + rt(length(x), df = 2)
)
fit1a <- lm(y ~ x, data = sim1a)
ggplot(sim1a, aes(x, y)) +
  geom_point() +
  ylim(c(-10, 30)) +
  geom_smooth(method = "lm") +
  geom_abline(
    aes(intercept = fit1a$coefficients[1],
        slope = fit1a$coefficients[2],
        colour = 'red'),
    data = sim1a)
fit1a$coefficients
sim1a
range(sim1a$y)
?map

simt <- function(i) {
  tibble(
    x = rep(1:10, each = 3), 
    y = x * 1.5 + 6 + rt(length(x), df = 2),
    .id = i
  )
}

lm_df <- function(df) {
  mod <- lm(y ~ x, data = df)
  beta <- coefficients(mod)
  tibble(intercept = beta[1],
         slope = beta[2])
}

simt(1)
lm_df(simt(1))

sims <- map(1:100, 
            simt) %>% 
  map_df(lm_df)
sims

ggplot(sims, aes(x = intercept, y = slope)) +
  geom_point()

#2
make_prediction <- function(a, data) {
  a[1] + data$x * a[2]
}

measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}

# model1(c(7, 1.5), sim1)
# measure_distance <- function(mod, data) {
#   diff <- data$y - model1(mod, data)
#   sqrt(mean(diff ^ 2))
# }
# measure_distance(c(7, 1.5), sim1)
# sim_e2 <- as.double(sims[1,])
# sim_e2
sim_e2 <- simt(1)
best <- optim(c(0, 0), measure_distance, data = sim_e2)
best
lm_e2 <- lm(y ~ x, data = sim_e2)
lm_e2$coefficients
ggplot(sim_e2, aes(x, y)) +
  geom_point() +
  geom_abline(aes(intercept = best$par[1],
                  slope = best$par[2],
                  colour = "blue")) +
  geom_abline(aes(intercept = lm_e2$coefficients[1],
                  slope = lm_e2$coefficients[2],
                  colour = "red"))

# 23.3.1 Predictions ------------------------------------------------------

grid <- sim1 %>% 
  data_grid(x)
grid
grid <- grid %>% 
  add_predictions(sim1_mod)
grid
ggplot(sim1, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)
sim1 <- sim1 %>% 
  add_residuals(sim1_mod)
sim1
summary(sim1_mod)
dim(sim1)

# There are a few different ways to understand what the residuals tell us about
# the model. One way is to simply draw a frequency polygon to help us understand
# the spread of the residuals:
  
ggplot(sim1, aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

# You’ll often want to recreate plots using the residuals instead of the
# original predictor. You’ll see a lot of that in the next chapter.

ggplot(sim1, aes(x, resid)) + 
  geom_ref_line(h = 0) +
  geom_point() 
?add_predictions

# 23.3.3 Exercises
#1

# sim1_mod <- lm(y ~ x, data = sim1)
sim1_mod_loess <- loess(y ~ x, data = sim1)
summary(sim1_mod_loess)
grid
grid <- grid %>% 
  add_predictions(sim1_mod_loess)
grid
ggplot(sim1, aes(x = x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "red", size = 1)
summary(sim1_mod)
sim1
sim1 <- sim1 %>% 
  add_residuals(sim1_mod_loess)
sim1

#2
?add_predictions
add_predictions(sim1, sim1_mod)
sim1
add_predictions(grid, sim1_mod)
spread_predictions(grid, sim1_mod, sim1_mod_loess)
gather_predictions(grid, sim1_mod, sim1_mod_loess)

#3
?geom_ref_line

# 23.4 Formulas and model families ----------------------------------------

df <- tribble(
  ~y, ~x1, ~x2,
  4, 2, 5,
  5, 1, 6
)
df
model_matrix(y ~ x1, data = df)
model_matrix(df, y ~ x1 + x2)
?model_matrix
model.matrix(y ~ x1, data = df)
model_matrix(mtcars, mpg ~ cyl)
dim(mtcars)
model.matrix(y ~ x1, data = df)

# 23.4.1 Categorical variables --------------------------------------------

df <- tribble(
  ~ sex, ~ response,
  "male", 1,
  "female", 2,
  "male", 1
)
model_matrix(df, response ~ sex)

ggplot(sim2) + 
   geom_point(aes(x, y))
mod2 <- lm(y ~ x, sim2)
grid <- sim2 %>% 
  data_grid(x) %>% 
  add_predictions(mod2)
grid

ggplot(sim2, aes(x)) + 
  geom_point(aes(y = y)) +
  geom_point(data = grid, aes(y = pred), colour = "red", size = 4)


# 23.4.2 Interactions (continuous and categorical) ------------------------

ggplot(sim3, aes(x1, y)) + 
  geom_point(aes(colour = x2))
sim3

# There are two possible models you could fit to this data:
  
mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

grid <- sim3 %>% 
  data_grid(x1, x2) %>% 
  gather_predictions(mod1, mod2)
grid
sim3 %>% data_grid(x1, x2)

ggplot(sim3, aes(x1, y, colour = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~ model)

sim3 <- sim3 %>% 
  gather_residuals(mod1, mod2)

ggplot(sim3, aes(x1, resid, colour = x2)) + 
  geom_point() + 
  facet_grid(model ~ x2)
sim3

# 23.4.3 Interactions (two continuous) ------------------------------------

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

sim4
grid <- sim4 %>% 
  data_grid(
    x1 = seq_range(x1, 5), 
    x2 = seq_range(x2, 5) 
  ) %>% 
  gather_predictions(mod1, mod2)
grid

seq_range(c(0.0123, 0.923423), n = 5)
#> [1] 0.0123 0.2401 0.4679 0.6956 0.9234
seq_range(c(0.0123, 0.923423), n = 5, pretty = TRUE)

x1 <- rcauchy(100)
seq_range(x1, n = 5)
#> [1] -115.9  -83.5  -51.2  -18.8   13.5
seq_range(x1, n = 5, trim = 0.10)
#> [1] -13.84  -8.71  -3.58   1.55   6.68
seq_range(x1, n = 5, trim = 0.25)
#> [1] -2.1735 -1.0594  0.0547  1.1687  2.2828
seq_range(x1, n = 5, trim = 0.50)
#> [1] -0.725 -0.268  0.189  0.647  1.104

x2 <- c(0, 1)
seq_range(x2, n = 5)
#> [1] 0.00 0.25 0.50 0.75 1.00
seq_range(x2, n = 5, expand = 0.10)
#> [1] -0.050  0.225  0.500  0.775  1.050
seq_range(x2, n = 5, expand = 0.25)
#> [1] -0.125  0.188  0.500  0.812  1.125
seq_range(x2, n = 5, expand = 0.50)

ggplot(grid, aes(x1, x2)) + 
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~ model)

# That doesn’t suggest that the models are very different! But that’s partly an
# illusion: our eyes and brains are not very good at accurately comparing shades
# of colour. Instead of looking at the surface from the top, we could look at it
# from either side, showing multiple slices:
  
ggplot(grid, aes(x1, pred, colour = x2, group = x2)) + 
  geom_line() +
  facet_wrap(~ model)
ggplot(grid, aes(x2, pred, colour = x1, group = x1)) + 
  geom_line() +
  facet_wrap(~ model)

# 23.4.4 Transformations --------------------------------------------------

df <- tribble(
  ~y, ~x,
  1,  1,
  2,  2, 
  3,  3
)

df <- tribble(
  ~y, ~x,
  1,  1, 
  2,  2,
  3,  3
)
df
model_matrix(df, y ~ x^2 + x)
model_matrix(df, y ~ I(x ^ 2) + x)

model_matrix(df, y ~ poly(x, 2))

library(splines)
model_matrix(df, y ~ ns(x, 2))

# Let’s see what that looks like when we try and approximate a non-linear function:
  
sim5 <- tibble(
    x = seq(0, 3.5 * pi, length = 50),
    y = 4 * sin(x) + rnorm(length(x))
)
sim5

ggplot(sim5, aes(x, y)) +
  geom_point()

mod1 <- lm(y ~ ns(x, 1), data = sim5)
mod2 <- lm(y ~ ns(x, 2), data = sim5)
mod3 <- lm(y ~ ns(x, 3), data = sim5)
mod4 <- lm(y ~ ns(x, 4), data = sim5)
mod5 <- lm(y ~ ns(x, 5), data = sim5)

grid <- sim5 %>% 
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>% 
  gather_predictions(mod1, mod2, mod3, mod4, mod5, .pred = "y")
seq_range(1:10, 19)
grid
ggplot(sim5, aes(x, y)) + 
  geom_point() +
  geom_line(data = grid, colour = "red") +
  facet_wrap(~ model)

# 23.4.5 Exercises
#1
sim2
mod2 <- lm(y ~ x, data = sim2)
mod2a <- lm(y ~ x - 1, data = sim2)
summary(mod2)
summary(mod2a)

#2
model_matrix(mod2$call)


# 23.5 Missing values -----------------------------------------------------

df <- tribble(
  ~x, ~y,
  1, 2.2,
  2, NA,
  3, 3.5,
  4, 8.3,
  NA, 10
)

mod <- lm(y ~ x, data = df)
nobs(mod)
