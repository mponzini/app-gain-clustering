# script to test KMeans
pacman::p_load(kml3d, ggplot2)

# fit model
kmean_test <- kml3d::cld3d(
  traj = brain_vol_wide,
  idAll = brain_vol_wide$subj_id,
  time = c(1, 2, 3),
  timeInData = list(
    Frontal = c(2, 16, 9),
    Parietal = c(3, 17, 10),
    Temproal = c(4, 18, 11),
    Occipital = c(6, 20, 13)
  ),
  varNames = brain_regions[c(1:3, 5)]
)

# check k=2 - k=8
kml3d::kml3d(kmean_test, nbClusters = 2:8)

# extract BIC to determine # clusters to use
BIC <- rbind(
  BIC_K2 = kmean_test@c2[[1]]@criterionValues[6],
  BIC_K3 = kmean_test@c3[[1]]@criterionValues[6],
  BIC_K4 = kmean_test@c4[[1]]@criterionValues[6],
  BIC_K5 = kmean_test@c5[[1]]@criterionValues[6],
  BIC_K6 = kmean_test@c6[[1]]@criterionValues[6],
  BIC_K7 = kmean_test@c7[[1]]@criterionValues[6],
  BIC_K8 = kmean_test@c8[[1]]@criterionValues[6]
)

numb_k <- which.min(BIC) + 1

# extract cluster using getClusters
clusters_kmeans <- as.numeric(kml::getClusters(kmean_test, numb_k))

sum(table(clusters_kmeans))

# process cluster assignment and merge with data
perc <- paste0(
  round(
    100 * table(clusters_kmeans) / length(unique(brain_vol_wide$subj_id)),
    1
  ),
  "%"
)

clusters_kmeans <- factor(
  clusters_kmeans,
  labels = paste0("Cluster ", 1:numb_k, " (", perc, ")")
)


dat_new <- data.frame(
  subj_id = brain_vol_wide$subj_id,
  cluster = clusters_kmeans
)

brain_clusters <- merge(
  brain_vol,
  dat_new,
  by = "subj_id",
  sort = FALSE
)


p1.kmeans <- plot_kmean_feature(
  data = brain_clusters,
  var = brain_use[1]
)
p2.kmeans <- plot_kmean_feature(
  data = brain_clusters,
  var = brain_use[2]
)
p3.kmeans <- plot_kmean_feature(
  data = brain_clusters,
  var = brain_use[3]
)
p4.kmeans <- plot_kmean_feature(
  data = brain_clusters,
  var = brain_use[4]
)
kmean.legend <- cowplot::get_plot_component(
  p1.kmeans, 'guide-box-bottom', return_all = TRUE
)


cowplot::plot_grid(
  cowplot::plot_grid(
    p1.kmeans + theme(legend.position = "none"), 
    p2.kmeans + theme(legend.position = "none"), 
    p3.kmeans + theme(legend.position = "none"), 
    p4.kmeans + theme(legend.position = "none"),
    nrow = 2, ncol = 2
  ),
  kmean.legend,
  nrow = 2,
  rel_heights = c(1, 0.1)
)
