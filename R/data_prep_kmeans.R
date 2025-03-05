## data prep file for Andy ##
# install pacman if not already
if(!require(pacman)) { install.packages("pacman")}
# load packages, will automatically install any missing
pacman::p_load(openxlsx, dplyr, tibble, tidyr, kml3d, ggplot2, cowplot)

# source script with helper functions
source("kmeans_helpers.R")

# import data: csv file is located on Box
brain_vol_raw <- read.csv("longitudinal_clustering_brain_volume_dataset_2024-12-19.csv")

brain_vol_raw <- brain_vol_raw |>
  dplyr::arrange(subj_id, visit) |>
  # rename variables to match previous version
  dplyr::rename(
    Type2.L3.Frontal_L = mori_total_l3_frontal_l, 
    Type2.L3.Frontal_R = mori_total_l3_frontal_r, 
    Type2.L3.Parietal_L = mori_total_l3_parietal_l, 
    Type2.L3.Parietal_R = mori_total_l3_parietal_r,
    Type2.L3.Temporal_L = mori_total_l3_temporal_l, 
    Type2.L3.Temporal_R = mori_total_l3_temporal_r,
    Type2.L3.Limbic_L = mori_total_l3_limbic_l, 
    Type2.L3.Limbic_R = mori_total_l3_limbic_r,
    Type2.L3.Occipital_L = mori_total_l3_occipital_l, 
    Type2.L3.Occipital_R = mori_total_l3_occipital_r
  )

# vectors to store variable names
covariates <- c("sex", "app_diagnosis")
brain_regions <- c("Frontal", "Parietal", "Temporal", "Limbic", "Occipital")

sub_regions <- c("Type2.L3.Frontal_L", "Type2.L3.Frontal_R", 
                 "Type2.L3.Parietal_L", "Type2.L3.Parietal_R", 
                 "Type2.L3.Temporal_L", "Type2.L3.Temporal_R", 
                 "Type2.L3.Limbic_L", "Type2.L3.Limbic_R", 
                 "Type2.L3.Occipital_L", "Type2.L3.Occipital_R")
prop_vol_sub_regions <- paste0(sub_regions, "_prop")
# process data
brain_vol <- brain_vol_raw |> 
  # remove duplicate rows
  unique() |>
  mutate(
    # calc brain vol by region
    Frontal = Type2.L3.Frontal_L + Type2.L3.Frontal_R,
    Parietal = Type2.L3.Parietal_L + Type2.L3.Parietal_R,
    Temporal = Type2.L3.Temporal_L + Type2.L3.Temporal_R,
    Limbic = Type2.L3.Limbic_L + Type2.L3.Limbic_R,
    Occipital = Type2.L3.Occipital_L + Type2.L3.Occipital_R,
    # create numeric subj id
    subj_id_numeric = as.numeric(gsub("-", "", subj_id))
  ) |>
  # scale regions of interest
  mutate(
    across(
      .cols = any_of(c(sub_regions, brain_regions)),
      ~ scale(.x) |> as.vector(), # default will center (mean = 0) and scale (sd = 1)
      .names = "{.col}_scaled"
    )
  ) |>
  ## to tease out total size differences, convert from volume to proportion of total volume ##
  dplyr::mutate(
    dplyr::across(
      .cols = c(Type2.L3.Frontal_L, Type2.L3.Parietal_L, Type2.L3.Occipital_L,
                Type2.L3.Temporal_L, Type2.L3.Limbic_L),
      ~ .x / mori_total_l1_hemisphere_l,
      .names = "{.col}_prop"
    ),
    dplyr::across(
      .cols = c(Type2.L3.Frontal_R, Type2.L3.Parietal_R, Type2.L3.Occipital_R,
                Type2.L3.Temporal_R, Type2.L3.Limbic_R),
      ~ .x / mori_total_l1_hemisphere_r,
      .names = "{.col}_prop"
    ),
    dplyr::across(
      .cols = c(Frontal, Parietal, Occipital,
                Temporal, Limbic),
      ~ .x / mori_total_l1_total_volume,
      .names = "{.col}_prop"
    )
  )


# kmeans requires data to be in a wide format
brain_vol_prop_wide_nas <- brain_vol |>
  # count number of observations by subject
  dplyr::mutate(
    n = dplyr::n(),
    .by = subj_id
  ) |>
  # keep subjects with more than 1 observation
  dplyr::filter(n > 1) |>
  # reduce data to necessary variables
  dplyr::select(subj_id_numeric, visit, contains("prop")) |>
  # convert to wide format
  reshape(
    idvar = "subj_id_numeric", timevar = "visit", direction = "wide", sep = "_"
  ) |>
  # reorder to help fit kmeans
  dplyr::select(
    subj_id_numeric, 
    # Left Hemisphere: Frontal, Parietal, Temporal, Occipital, Limbic
    Type2.L3.Frontal_L_prop_1, Type2.L3.Frontal_L_prop_2, Type2.L3.Frontal_L_prop_3, 
    Type2.L3.Parietal_L_prop_1, Type2.L3.Parietal_L_prop_2, Type2.L3.Parietal_L_prop_3,
    Type2.L3.Temporal_L_prop_1, Type2.L3.Temporal_L_prop_2, Type2.L3.Temporal_L_prop_3,
    Type2.L3.Occipital_L_prop_1, Type2.L3.Occipital_L_prop_2, Type2.L3.Occipital_L_prop_3,
    Type2.L3.Limbic_L_prop_1, Type2.L3.Limbic_L_prop_2, Type2.L3.Limbic_L_prop_3,
    # Right Hemisphere: Frontal, Parietal, Temporal, Occipital, Limbic
    Type2.L3.Frontal_R_prop_1, Type2.L3.Frontal_R_prop_2, Type2.L3.Frontal_R_prop_3, 
    Type2.L3.Parietal_R_prop_1, Type2.L3.Parietal_R_prop_2, Type2.L3.Parietal_R_prop_3,
    Type2.L3.Temporal_R_prop_1, Type2.L3.Temporal_R_prop_2, Type2.L3.Temporal_R_prop_3,
    Type2.L3.Occipital_R_prop_1, Type2.L3.Occipital_R_prop_2, Type2.L3.Occipital_R_prop_3,
    Type2.L3.Limbic_R_prop_1, Type2.L3.Limbic_R_prop_2, Type2.L3.Limbic_R_prop_3
  )


## kml3d has 3 built-in imputation methods:
##    copyMean: locf, global, local, bisector
##    linearInterpol: locf, global, local, bisector
##    traj: Mean, Median, HotDeck
##    cross: Mean. Median, HotDeck
##    locf (last obs forward) and nocb (next obs backward): DO NOT USE

brain_wide_matrix <- brain_vol_prop_wide_nas |> as.matrix()

## copyMean.bisector ##
brain_vol_copyMean <- longitudinalData::imputation(
  brain_wide_matrix, method="copyMean.bisector"
) |>
  tibble::as_tibble()

# run kmeans using brain_vol_copyMean
brain_vol_copyMean_clusters <- kmeans_clusters(
  dataset = brain_vol_copyMean, 
) # kmeans_clusters() will try k=2:5. returns dataset with cluster assignment

# merge cluster assignment with long format data
brain_vol_clusters_copyMean <- merge(
  brain_vol,
  brain_vol_copyMean_clusters |> dplyr::select(subj_id_numeric, cluster),
  by = "subj_id_numeric",
  sort = FALSE
)
# plot features by cluster
plot_kmean_feature(brain_vol_clusters_copyMean, var = prop_vol_sub_regions[1])
# repeat for each sub-region


## sample code from Rmd on combining features into a single plot ##
p1_L.kmeans <- plot_kmean_feature(
  data = brain_clusters_sub,
  var = prop_vol_sub_regions[1]
) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 1))
p2_L.kmeans <- plot_kmean_feature(
  data = brain_clusters_sub,
  var = prop_vol_sub_regions[3]
) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 1))
p3_L.kmeans <- plot_kmean_feature(
  data = brain_clusters_sub,
  var = prop_vol_sub_regions[5]
) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 1))
p4_L.kmeans <- plot_kmean_feature(
  data = brain_clusters_sub,
  var = prop_vol_sub_regions[9]
) +
  ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 1))
kmean_L.legend <- cowplot::get_plot_component(
  p1_L.kmeans, 'guide-box-bottom', return_all = TRUE
)


cowplot::plot_grid(
  cowplot::plot_grid(
    p1_L.kmeans + theme(legend.position = "none"), 
    p2_L.kmeans + theme(legend.position = "none"), 
    p3_L.kmeans + theme(legend.position = "none"), 
    p4_L.kmeans + theme(legend.position = "none"),
    nrow = 2, ncol = 2
  ),
  kmean_L.legend,
  nrow = 2,
  rel_heights = c(1, 0.1)
)


## linearInterpol.bisector ##




## crossHotDeck


