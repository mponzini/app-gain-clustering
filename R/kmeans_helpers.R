## functions to help with K-Means ##
# select number of clusters
kmeans_clusters <- function(
    dataset, id_var = "subj_id_numeric", k_min = 2, k_max = 5, 
    regions = c(
      "Type2.L3.Frontal_L_prop", "Type2.L3.Frontal_R_prop", 
      "Type2.L3.Parietal_L_prop", "Type2.L3.Parietal_R_prop", 
      "Type2.L3.Temporal_L_prop", "Type2.L3.Temporal_R_prop", 
      "Type2.L3.Limbic_L_prop", "Type2.L3.Limbic_R_prop", 
      "Type2.L3.Occipital_L_prop", "Type2.L3.Occipital_R_prop"
    )
) {
  # fit model
  kmean_test <<- kml3d::cld3d(
    traj = dataset,
    idAll = dataset[[id_var]],
    time = c(1, 2, 3),
    timeInData = list(
      Frontal_L = c(2:4),
      Parietal_L = c(5:7),
      Temproal_L = c(8:10),
      Occipital_L = c(11:13),
      Frontal_R = c(17:19),
      Parietal_R = c(20:22),
      Temproal_R = c(23:25),
      Occipital_R = c(26:28)
    ),
    varNames = regions[c(1, 3, 5, 9, 2, 4, 6, 10)]
  )
  
  # check k=2 - k=8
  kml3d::kml3d(kmean_test, nbClusters = k_min:k_max)
  
  # extract BIC to determine # clusters to use
  BIC <- rbind(
    BIC_K2 = kmean_test@c2[[1]]@criterionValues[6],
    BIC_K3 = kmean_test@c3[[1]]@criterionValues[6],
    BIC_K4 = kmean_test@c4[[1]]@criterionValues[6],
    BIC_K5 = kmean_test@c5[[1]]@criterionValues[6]
  )
  
  k <- which.min(BIC) + 1
  
  # extract cluster using getClusters
  clusters_kmeans <- kml::getClusters(kmean_test, k) |>
    as.numeric()
  
  # process cluster assignment and merge with data
  perc <- paste0(
    round(
      100 * table(clusters_kmeans) / length(unique(dataset[[id_var]])),
      1
    ),
    "%"
  )
  
  clusters_kmeans <- factor(
    clusters_kmeans,
    labels = paste0("Cluster ", 1:k, " (", perc, ")")
  )
  
  
  dat_cluster <- data.frame(
    subj_id = dataset[[id_var]],
    cluster = clusters_kmeans
  )
  
  dataset_cluster <- merge(
    dataset,
    dat_cluster,
    by.x = id_var, by.y = "subj_id",
    sort = FALSE
  )
  
  return(dataset_cluster)
  
}

# plot features by cluster
plot_kmean_feature <- function(
    data, 
    time_var = "visit", 
    cluster_var = "cluster", 
    var, 
    var_label = var
){
  ggplot(
    data = data,
    aes(
      x = visit,
      y = .data[[var]],
      color = .data[[cluster_var]],
      fill = .data[[cluster_var]],
      linetype = .data[[cluster_var]]
    )
  ) +
    geom_smooth(
      method = 'loess',
      linewidth = 3,
      se = FALSE,
      span = 2
    ) +
    labs(
      x = "Visit",
      y = var_label
    ) +
    scale_x_continuous(breaks = c(1, 2, 3)) +
    guides(
      color = guide_legend(title = "Cluster"),
      fill = guide_legend(title = "Cluster"),
      linetype = guide_legend(title = "Cluster")
    ) +
    theme_bw() +
    theme(
      legend.position = "bottom"
    )
}
