## run k=1 and k=2:8 to find optimal number of clusters ##
library(ggplot2)
library(lcmm)

# load data
brain_vol <- readRDS("~/app-gain-clustering/brain_vol_long.rds")


## using summed brain region volumes ##
# fit lcmm with K = 1 to obtain initial values
lcmm_k1 <- lcmm::multlcmm(
  Type2.L3.Frontal_L + Type2.L3.Frontal_R + Type2.L3.Parietal_L + 
    Type2.L3.Parietal_R + Type2.L3.Temporal_L + Type2.L3.Temporal_R + 
    Type2.L3.Limbic_L + Type2.L3.Limbic_R + Type2.L3.Occipital_L + 
    Type2.L3.Occipital_R + ventricular_csf + brainstem_cerebellum ~ scan_age,
  random = ~ scan_age,
  subject = 'subj_id_numeric',
  data = brain_vol,
  randomY = TRUE,
  ng = 1
)

# fit lcmm with K = 2:8 to determine optimal number of clusters
BIC <- NULL
fit_k2_k8 <- vector(mode = 'list', length = 7)
for(kk in 2:8){
  # fit model
  fit <- lcmm::multlcmm(
    Type2.L3.Frontal_L + Type2.L3.Frontal_R + Type2.L3.Parietal_L + 
      Type2.L3.Parietal_R + Type2.L3.Temporal_L + Type2.L3.Temporal_R + 
      Type2.L3.Limbic_L + Type2.L3.Limbic_R + Type2.L3.Occipital_L + 
      Type2.L3.Occipital_R + ventricular_csf + brainstem_cerebellum ~ scan_age,
    mixture = ~ scan_age,
    random = ~ scan_age,
    subject = 'subj_id_numeric',
    data = brain_vol,
    randomY = TRUE,
    ng = kk,
    B = lcmm_k1
  )
  # store model and BIC
  fit_k2_k8[[kk-1]] <- fit
  BIC <- c(BIC, fit$BIC)
}


## save results
saveRDS(fit_k2_k8, file = "~/app-gain-clustering/lcmm_k_2-8.rds")