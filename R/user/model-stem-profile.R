

tmp <- list()


data_md <- data_clean$stem_taper |> mutate(no_group = "a")

# group <- "no_group"
# group <- "species_group"
# tmp$col <- data_md |> pull(as_name(group))
# data_md <- data_clean$stem_taper |> mutate(group = tmp$col)



##
## MODEL ALL PARAMS ####
##

start <- c(1.02, 0.62, 0.15, 38, 4.48)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:5]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^e), 
  data = data_md,
  fixed = a + b + c + d + e ~ 1,
  groups = ~ no_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_5params_nogp <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
md_5params_nogp


##
## M. 4 PARAMS: no e ####
##

## + No group ####

start <- c(1.02, 0.62, 0.15, 38)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:4]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c + d ~ 1,
  groups = ~ no_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_4params_nogp <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
md_4params_nogp

## + group_species ####

start <- c(1.02, 0.62, 0.15, 38)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:4]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-d * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c + d ~ 1,
  groups = ~ species_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_4params_species_group <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
md_4params_species_group



##
## M. 3 PARAMS: no d, e ####
##

## + No group ####

start <- c(1.02, 0.62, 0.15)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:3]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-24.7 * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~ no_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3params_nogp <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
md_3params_nogp

md_3params_nogp$md_info$m_ranef


## + species group ####

start <- c(1.02, 0.62, 0.15)
#start <- c(1.0, 0.3, 0.3, 24.7, 1.8)
names(start) <- letters[1:3]

md <- nlme(
  model = dr ~ a * ((1 - b * hr) * (1 + c * exp(-24.7 * hr)) - (1 - b) * hr^1.8235), 
  data = data_md,
  fixed = a + b + c ~ 1,
  groups = ~ species_group,
  start = start, 
  #weights = varPower(form = ~hr),
  #control = nlmeControl(maxIter = 100)
)

AIC(md)

md_3params_species_group <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
md_3params_species_group

md_3params_species_group$md_info$m_ranef



##
## M. 4 PARAMS: no b ####
##

## NO CONVERGENCE

# start <- c(1.0, 0.3, 24.7, 1.8)
# start <- c(10, 10, 100, 10)
# names(start) <- c("a", "c", "d", "e")
# 
# md <- nlme(
#   model = dr ~ a * ((1 - hr) * (1 + c * exp(-d * hr)) - hr^e), 
#   data = data_md,
#   fixed = a + c + d + e ~ 1,
#   groups = ~ no_group,
#   start = start, 
#   #weights = varPower(form = ~hr),
#   control = nlmeControl(maxIter = 100)
# )
# 
# AIC(md)
# 
# md_out <- nlme_out(.data = data_md, .start = start, .md = md, .name_dev = "gs")
# md_out
# tt <- md_out$parameters


## 
## More graphs - best model ####
##

md_4params_group_species$data |>
  ggplot(aes(x = hr, y = dr)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = pred, color = species_group)) +
  theme(legend.position = "bottom") +
  labs(color = "") +
  facet_wrap(~species_group)

md_4params_group_species$data |>
  ggplot(aes(x = hr, y = dr)) +
  geom_point(size = 0.1) +
  geom_line(aes(y = pred, color = species_group)) +
  theme(legend.position = "none") +
  labs(color = "") +
  facet_wrap(~species_group)
