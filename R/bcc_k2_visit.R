## run BCC on cluster: k = 2 ##
if (!require("pacman")) install.packages("pacman")
pacman::p_load(BCClong)

# load data
brain_vol <- readRDS("~/app-gain-clustering/brain_vol_long_20241219.rds")

# set seed
set.seed(61824)

bcc_fit <- BCClong::BCC.multi(
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
  num.cluster = 2,
  initial.cluster.membership = "random",
  center = 0,
  burn.in = 2000,
  thin = 10,
  per = 100,
  max.iter = 12000
)

saveRDS(bcc_fit, file = "~/app-gain-clustering/bcc_k2_visit_proportions.rds")