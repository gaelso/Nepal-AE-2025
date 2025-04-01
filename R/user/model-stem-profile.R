

tmp <- list()


data_md <- data_clean$stem_taper |> mutate(no_group = "a")

# group <- "no_group"
# group <- "species_group"
# tmp$col <- data_md |> pull(as_name(group))
# data_md <- data_clean$stem_taper |> mutate(group = tmp$col)


#start <- c(1.02, 0.62, 0.15, 38, 4.48)
start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:5]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^e), 
  data = data_md,
  fixed = a + b + c + d + e ~ 1,
  groups = ~ species_group,
  start = start, 
  weights = varPower(form = ~hr),
  control = nlmeControl(maxIter = 100)
)

md_out <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
md_out

