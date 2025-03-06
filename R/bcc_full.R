## test Bayesian Consensus Clustering
pacman::p_load(BCClong, ggplot2)
# source data prep file
# source("./R/data_prep.R")
brain_vol <- readRDS("./data-ext/brain_vol_long_20241219.rds")
bcc_fits <- readRDS(file = "./data-ext/bcc_models_k2-4_hemispheres.rds")

alpha_adjust <- lapply(bcc_fits, function(x) x$alpha.adjust) |>  unlist()
bcc_k <- which.max(alpha_adjust) + 1


set.seed(9853)
fit_frontal_l <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Frontal_L_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)

fit_frontal_r <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Frontal_R_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)
fit_parietal_l <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Parietal_L_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)
fit_parietal_r <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Parietal_R_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)
fit_temporal_l <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Temporal_L_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)
fit_temporal_r <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Temporal_R_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)
fit_occipital_l <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Occipital_L_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)
fit_occipital_r <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Occipital_R_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 1000,
  thin = 1,
  per = 1000,
  max.iter = 2000
)

set.seed(98743)
fit_bcc <- BCClong::BCC.multi(
  mydat = list(
    brain_vol$Type2.L3.Frontal_L_prop,
    brain_vol$Type2.L3.Frontal_R_prop,
    brain_vol$Type2.L3.Parietal_L_prop,
    brain_vol$Type2.L3.Parietal_R_prop,
    brain_vol$Type2.L3.Temporal_L_prop,
    brain_vol$Type2.L3.Temporal_R_prop,
    brain_vol$Type2.L3.Occipital_L_prop,
    brain_vol$Type2.L3.Occipital_R_prop
  ),
  dist = c("gaussian"),
  id = list(
    brain_vol$subj_id_numeric
  ),
  time = list(
    brain_vol$visit
  ),
  formula = list(
    y ~ time + (1|id)
  ),
  num.cluster = bcc_k,
  initial.cluster.membership = "input",
  input.initial.local.cluster.membership = list(
    fit_frontal_l$cluster.global |> as.numeric(),
    fit_frontal_r$cluster.global |> as.numeric(),
    fit_parietal_l$cluster.global |> as.numeric(),
    fit_parietal_r$cluster.global |> as.numeric(),
    fit_temporal_l$cluster.global |> as.numeric(),
    fit_temporal_r$cluster.global |> as.numeric(),
    fit_occipital_l$cluster.global |> as.numeric(),
    fit_occipital_r$cluster.global |> as.numeric()
  ),
  input.initial.global.cluster.membership = fit_frontal_l$cluster.global |> as.numeric(),
  center = 0,
  burn.in = 10000,
  thin = 10,
  per = 10000,
  max.iter = 20000
)

saveRDS(fit_bcc, file = "./data-ext/bcc_final_model.rds")