
library(readxl)
library(nlme)
library(systemfit)
library(tidyverse)

theme_set(theme_bw())

plot_init <- read_xlsx("data/AE-raw-2025-03-30.xlsx", sheet = "01-info", na = "NA")
tree_init <- read_xlsx("data/AE-raw-2025-03-30.xlsx", sheet = "03-tree", na = "NA") 
stem_init <- read_xlsx("data/AE-raw-2025-03-30.xlsx", sheet = "04-stem", na = "NA", guess_max = 10000)

## Subset of cols for plot
plot <- plot_init |>
  select(updated_tree_code, physiographic_region, province, district, altitude, forest_type)

tree_select <- tree_init |> select(updated_tree_code, tree_dbh, tree_total_height)

## Split data by log measurements, abnormal measurements and tree top measurements

## Normal measurement
stem_sub <- stem_init |>
  filter(!is.na(log_diam_ob)) |>
  mutate(
    measurement_type = "normal",
    log_base_pom = if_else(is.na(log_base_pom) & !is.na(log_disk_pom), log_disk_pom, log_base_pom)
    ) |>
  select(updated_tree_code, log_base_pom, log_diam_ob, log_diam_ub, measurement_type)

## Check
check <- stem_sub |> filter(is.na(log_base_pom))

## Abnormal
stem_ab <- stem_init |> 
  filter(!is.na(log_diam_ob_abnormal)) |>
  mutate(
    measurement_type = "abnormal",
    log_base_pom_abnormal = if_else(is.na(log_base_pom_abnormal) & !is.na(log_base_pom), log_base_pom, log_base_pom_abnormal)
    ) |>
  select(updated_tree_code, ends_with("_abnormal"), -log_length_abnormal, measurement_type) |>
  rename_with(.cols = ends_with("_abnormal"), str_remove, "_abnormal") |>
  filter(!is.na(log_base_pom))
  
## Check
check <- stem_ab |> filter(is.na(log_base_pom))


## Make a function to get non-NA top measurements and bind rows
vec_end <- c("_below20", "_mid20", "_above20", "_below10", "_mid10", "_above10")

stem_top <- map(vec_end, function(x){
  
  stem_init |>
    select(updated_tree_code, ends_with(x)) |>
    filter(if_all(ends_with(x), ~!is.na(.))) |>
    rename_with(.cols = ends_with(x), str_remove, pattern = x) |>
    mutate(measurement_type = "top")
  
}) |> list_rbind()

## Check
check <- stem_top |> filter(is.na(log_base_pom))


## Group all
stem_all <- bind_rows(stem_sub, stem_ab, stem_top)

## Check NA
check <- stem_all |> filter(is.na(log_base_pom))
check <- stem_all |> filter(is.na(log_diam_ob))

## 
stem_clean <- stem_all |>
  #filter(!is.na(log_base_pom), !is.na(log_diam_ob)) |>
  distinct(updated_tree_code, log_base_pom, .keep_all = T) |>
  arrange(updated_tree_code, log_base_pom) |>
  group_by(updated_tree_code) |>
  mutate(log_no = row_number()) |>
  ungroup() |>
  left_join(tree_select, by = "updated_tree_code") |>
  mutate(
    dr = round(log_diam_ob / tree_dbh, 3),
    hr = round(log_base_pom / tree_total_height, 3)
  )


ggplot(stem_clean) +
  geom_point(aes(x = log_base_pom, y = log_diam_ob), size = 0.1)

ggplot(stem_clean) +
  geom_point(aes(x = hr, y = dr), size = 0.1)


## Stem taper data cleaning




## Volume calculation
log_top <- stem_clean |>
  group_by(updated_tree_code) |>
  summarise(log_top = max(log_no))


stem_v <- stem_clean |>
  left_join(log_top, by = "updated_tree_code") |>
  mutate(
    log_no_top = case_when(
    log_no == log_top ~ "TOP",
    log_no < 10 ~ paste0("0", log_no),
    TRUE ~ as.character(log_no)
    )
    )


