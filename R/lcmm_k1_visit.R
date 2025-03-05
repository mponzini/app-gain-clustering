#### Run LCMM using K=2 ####
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lcmm)

# load data
brain_vol <- readRDS("~/app-gain-clustering/brain_vol_long_20241219.rds")

## using summed brain region volumes ##
lcmm_k1 <- lcmm::multlcmm(
  Type2.L3.Frontal_L_prop + Type2.L3.Frontal_R_prop + Type2.L3.Parietal_L_prop + 
    Type2.L3.Parietal_R_prop + Type2.L3.Temporal_L_prop + Type2.L3.Temporal_R_prop + 
    Type2.L3.Occipital_L_prop + Type2.L3.Occipital_R_prop ~ visit,
  random = ~ visit,
  subject = 'subj_id_numeric',
  data = brain_vol,
  randomY = TRUE,
  ng = 1
)

saveRDS(lcmm_k1, file = "~/app-gain-clustering/lcmm_k1_visit_proportions.rds")