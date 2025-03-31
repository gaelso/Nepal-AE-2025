

library(readxl)
library(nlme)
library(systemfit)
library(tidyverse)

theme_set(theme_bw())

#stem <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "04-stem-portion_corr", na = "NA")
plot_init <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "01-general-info", na = "NA")

tree_init <- read_xlsx("data/AE-raw-2025-03-25.xlsx", sheet = "03-tree-info", na = "NA") 

plot <- plot_init |>
  select(tree_code_old, physiographic_region, province, district, altitude, forest_type)


table(plot$physiographic_region, useNA = "ifany")


tree_join <- tree_init |>
  filter(!is.na(stem_B)) |>
  left_join(plot, by = "tree_code_old") |>
  mutate(
    tree_d2h = tree_dbh^2 * tree_total_height,
    tree_d2hwd = tree_dbh^2 * tree_total_height * stem_wd
  ) 



table(tree_join$species_name, useNA = "ifany")

table(tree_join$physiographic_region, useNA = "ifany")

check <- tree_join |>
  filter(is.na(physiographic_region)) |>
  pull(tree_code)
check


## Check shorea
tt <- tree_join |> filter(species_name == "Shorea robusta")

ggplot(tt) +
  geom_point(aes(x = tree_dbh, y = stem_V)) +
  facet_wrap(~physiographic_region)


tree_clean <- tree_join |>
  filter(
    tree_dbh < 80,
    !is.na(physiographic_region), 
    !is.na(forest_type), 
    physiographic_region != "High mountain")
