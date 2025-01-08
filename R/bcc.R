## test Bayesian Consensus Clustering
pacman::p_load(BCClong, ggplot2)
# source data prep file
# source("./R/data_prep.R")
brain_vol <- readRDS("./data-ext/brain_vol_long_20240919.rds")
# set seed
set.seed(61824)


# determine number of clusters, test k = 2 through 4
alpha.adjust <- NULL
fit_k2_k4 <- vector(mode = 'list', length = 3)

for(k in 2:4){
  fit <- BCClong::BCC.multi(
    mydat = list(
      brain_vol$Frontal_scaled,
      brain_vol$Parietal_scaled,
      brain_vol$Temporal_scaled,
      brain_vol$Occipital_scaled
    ),
    dist = c("gaussian"),
    id = list(
      brain_vol$subj_id_numeric
    ),
    time = list(
      brain_vol$scan_age
    ),
    formula = list(
      y ~ time + (1|id)
    ),
    num.cluster = k,
    initial.cluster.membership = "random",
    center = 0,
    burn.in = 2000,
    thin = 10,
    per = 100,
    max.iter = 12000
  )
  
  # store model and alpha adjust
  fit_k2_k4[[k - 1]] <- fit
  alpha.adjust <- c(alpha.adjust, fit$alpha.adjust)
}
## Note: inv() matrix singular at k = 5, only evaluated 

# select number of clusters that maximizes mean adjusted adherence 
plot(alpha.adjust)
numb.k <- which.max(alpha.adjust) + 1

# save fit_k1_k4
saveRDS(
  object = fit_k2_k4,
  file = "./data-ext/bcc_models.rds"
)


## repeat using hemisphere volumes, not summed volumes
# determine number of clusters, test k = 1 through 4
alpha.adjust <- NULL
fit_k2_k4 <- vector(mode = 'list', length = 3)

set.seed(7915)

for(k in 2:4){
  fit <- BCClong::BCC.multi(
    mydat = list(
      brain_vol$Type2.L3.Frontal_L_scaled,
      brain_vol$Type2.L3.Frontal_R_scaled,
      brain_vol$Type2.L3.Parietal_L_scaled,
      brain_vol$Type2.L3.Parietal_R_scaled,
      brain_vol$Type2.L3.Temporal_L_scaled,
      brain_vol$Type2.L3.Temporal_R_scaled,
      brain_vol$Type2.L3.Occipital_L_scaled,
      brain_vol$Type2.L3.Occipital_R_scaled
    ),
    dist = c("gaussian"),
    id = list(
      brain_vol$subj_id_numeric
    ),
    time = list(
      brain_vol$scan_age
    ),
    formula = list(
      y ~ time + (1|id)
    ),
    num.cluster = k,
    initial.cluster.membership = "random",
    center = 0,
    burn.in = 2000,
    thin = 10,
    per = 100,
    max.iter = 12000
  )
  
  # store model and alpha adjust
  fit_k2_k4[[k - 1]] <- fit
  alpha.adjust <- c(alpha.adjust, fit$alpha.adjust)
}
## Note: inv() matrix singular at k = 5, only evaluated 

# select number of clusters that maximizes mean adjusted adherence 
plot(alpha.adjust)
numb.k <- which.max(alpha.adjust) + 1

saveRDS(
  object = fit_k2_k4,
  file = "./data-ext/bcc_models_k2-4_hemispheres.rds"
)