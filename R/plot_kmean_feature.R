# function to plot k-mean clusters
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
