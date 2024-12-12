## test Bayesian Consensus Clustering
pacman::p_load(BCClong, ggplot2)
# source data prep file
# source("./R/data_prep.R")
brain_vol <- readRDS("./data-ext/brain_vol_long_20240919.rds")
# set seed
set.seed(61824)


# determine number of clusters, test k = 2 through 8
alpha.adjust <- NULL
fit_k2_k8 <- vector(mode = 'list', length = 7)

for(k in 2:8){
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
  fit_k2_k8[[k - 1]] <- fit
  alpha.adjust <- c(alpha.adjust, fit$alpha.adjust)
}
## Note: inv() matrix singular at k = 5, only evaluated 

# select number of clusters that maximizes mean adjusted adherence 
plot(alpha.adjust)
numb.k <- which.max(alpha.adjust) + 1

# save fit_k2_k8
saveRDS(
  object = fit_k2_k8,
  file = "./data-ext/bcc_models.rds"
)
