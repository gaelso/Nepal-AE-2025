
library(readxl)
library(tidyverse)
library(nlme)
library(systemfit)

#stem <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "04-stem-portion_corr", na = "NA")
plot_init <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "01-general-info", na = "NA")

tree_init <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "03-tree-info", na = "NA") 

plot <- plot_init |>
  select(tree_code_old, physiographic.region, province, district)

tree <- tree_init |>
  filter(!is.na(stem_B)) |>
  left_join(plot, by = "tree_code_old") |>
  mutate(
    tree_d2h = tree_dbh^2 * tree_total_height,
    fake_group = "a"
    )

ggplot(tree) +
  geom_point(aes(x = tree_dbh, y = stem_B))

ggplot(tree) +
  geom_point(aes(x = stem_V, y = stem_B))

ggplot(tree) +
  geom_point(aes(x = tree_d2h, y = stem_V, color = species_name))

ggplot(tree) +
  geom_point(aes(x = tree_d2h, y = stem_V)) +
  facet_wrap(~species_name)

ggplot(tree) +
  geom_point(aes(x = tree_d2h, y = stem_V)) +
  facet_wrap(~physiographic.region)


## Test model stem B

lm_bd <- lm(log(stem_B) ~ log(tree_dbh), data = tree)
summary(lm_bd)
coef(lm_bd)

coef_bd <- c(exp(coef(lm_bd)[1]), coef(lm_bd)[2])
names(coef_bd) <- c("a", "b")

nlme_bd <- nlme(
  model = stem_B ~ a * tree_dbh^b,
  data = tree, 
  start = coef_bd,
  fixed = a + b ~ 1,
  groups = ~species_name, 
  weights = varPower(form = ~tree_dbh)
  )

summary(nlme_bd)
AIC(nlme_bd)
fixef(nlme_bd)
ranef(nlme_bd)

tree_res <- tree |>
  mutate(
    pred_bd = predict(nlme_bd),
    res_bd  = residuals(nlme_bd)
  )

tree_res |>
  ggplot(aes(x = pred_bd, y = res_bd)) +
  geom_point()

tree_res |>
  ggplot(aes(x = pred_bd, y = stem_B)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

tree_res |>
  ggplot(aes(x = tree_dbh)) +
  geom_point(aes(y = stem_B)) +
  geom_line(aes(y = pred_bd, color = species_name)) +
  theme(legend.position = "bottom")




