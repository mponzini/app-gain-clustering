# script to test Latent Class Mixed Effect Model (LCMM)
pacman::p_load(lcmm, ggplot2)


# using all scaled brain regions #
# fit lcmm with K = 1 to obtain initial values
lcmm_k1 <- lcmm::multlcmm(
  Type2.L3.Frontal_L_prop + Type2.L3.Frontal_R_prop + Type2.L3.Parietal_L_prop + 
    Type2.L3.Parietal_R_prop + Type2.L3.Temporal_L_prop + Type2.L3.Temporal_R_prop + 
    Type2.L3.Occipital_L_prop + Type2.L3.Occipital_R_prop ~ scan_age,
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


plot(BIC)
# store number of optimal clusters
opt.k <- which.min(BIC) + 1 # opt.k = 5
opt.k <- 5


# fit final lcmm
lcmm_opt_k <- lcmm::multlcmm(
  Frontal + Parietal + Occipital + Temporal ~ scan_age,
  mixture = ~ scan_age,
  random = ~ scan_age,
  subject = 'subj_id_numeric',
  data = brain_vol,
  randomY = TRUE,
  ng = opt.k,
  B = lcmm_k1
)

clustprob <- apply(lcmm_opt_k$pprob[, -c(1, 2)], 1, max)

# cluster labels
cluster_re <- lcmm_opt_k$pprob$class

dat <- brain_vol[!duplicated(brain_vol$subj_id_numeric, fromLast = TRUE), ] |>
  mutate(
    PostProb = clustprob,
    Cluster = cluster_re |>
      factor(
        levels = c('1', '2', '3', '4', '5')
      )
  )

# plot posterior cluster probability
ggplot(
  dat,
  aes(
    x = Cluster,
    y = PostProb
  )
) +
  geom_boxplot() +
  labs(
    x = "Cluster",
    y = "Posterior Cluster Probability",
    title = "LCMM"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::percent
  ) +
  scale_x_discrete(drop = FALSE) +
  theme_bw()


#
N <- brain_vol |>
  pull(subj_id) |>
  unique() |>
  length()

per <- paste(
  round(
    100 * table(cluster_re) / N, 1
  ),
  "%",
  sep = ""
)

cluster_lcmm <- factor(
  cluster_re,
  levels = c('1', '2', '3', '4', '5'),
  labels = c(
    "Cluster 1 (0%)",
    paste0("Cluster ", 2:opt.k, " (", per, ")")
  )
)

dat_cluster <- data.frame(
  "id" = lcmm_opt_k$pprob$subj_id_numeric,
  "cluster" = cluster_lcmm
)

brain_clusters <- merge(
  brain_vol,
  dat_cluster,
  by.x = "subj_id_numeric",
  by.y = "id",
  sort = FALSE
)

plot_feature <- function(data, y_var, y_lab, legend = "none"){
  ggplot(
    data,
    aes(
      x = scan_age,
      y = .data[[y_var]],
      color = cluster,
      linetype = cluster,
      fill = cluster
    )
  ) +
    geom_smooth(method = 'loess', se = FALSE, formula = y ~ x) +
    labs(
      x = "Visit",
      y = paste0(y_lab, " Lobe"),
      color = "Cluster",
      fill = "Cluster",
      linetype = "Cluster"
    ) +
    theme_bw() +
    theme(
      legend.position = legend
    )
}


cowplot::plot_grid(
  cowplot::plot_grid(
    plot_feature(brain_clusters, y_var = "Frontal", y_lab = "Frontal"),
    plot_feature(brain_clusters, y_var = "Temporal", y_lab = "Temporal"),
    plot_feature(brain_clusters, y_var = "Parietal", y_lab = "Parietal"),
    plot_feature(brain_clusters, y_var = "Occipital", y_lab = "Occipital"),
    nrow = 2, ncol = 2
  ),
  cowplot::get_legend(
    plot_feature(
      brain_clusters, y_var = "Frontal", y_lab = "Frontal", legend = "bottom"
    )
  ),
  nrow = 2,
  rel_heights = c(1, 0.1)
)