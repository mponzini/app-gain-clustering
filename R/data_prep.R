# data prep
pacman::p_load(openxlsx, dplyr, tibble, tidyr)

# load brain data
brain_vol_raw <- read.csv(
  paste0(
    "S:/MIND/IDDRC Cores/",
    "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
    "Nordahl_R01MH10443801/MultivariateModeling/Data/",
    "longitudinal_clustering_brain_volume_dataset_2024-12-19.csv"
  )
)


brain_vol_raw <- brain_vol_raw |>
  # dplyr::select(subj_id, visit, sex, app_diagnosis, scan_age, 
  #               scan_scanner_system, scan_head_coil, 
  #               mori_total_l3_frontal_l, mori_total_l3_frontal_r, 
  #               mori_total_l3_parietal_l, mori_total_l3_parietal_r,
  #               mori_total_l3_temporal_l, mori_total_l3_temporal_r,
  #               mori_total_l3_limbic_l, mori_total_l3_limbic_r,
  #               mori_total_l3_occipital_l, mori_total_l3_occipital_r,
  #               ventricular_csf, brainstem_cerebellum) |>
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

# names(brain_vol_raw)
covariates <- c("sex", "app_diagnosis")
brain_regions <- c("Frontal", "Parietal", "Temporal", "Limbic", "Occipital")
                    # , "ventricular_csf", "brainstem_cerebellum")

sub_regions <- c("Type2.L3.Frontal_L", "Type2.L3.Frontal_R", 
                 "Type2.L3.Parietal_L", "Type2.L3.Parietal_R", 
                 "Type2.L3.Temporal_L", "Type2.L3.Temporal_R", 
                 "Type2.L3.Limbic_L", "Type2.L3.Limbic_R", 
                 "Type2.L3.Occipital_L", "Type2.L3.Occipital_R")#,
                 # "ventricular_csf", "brainstem_cerebellum")

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
      ~ scale(.x) |> as.vector(),
      .names = "{.col}_scaled"
    )
  ) |>
  ## to tease out total size differences, convert from volume to proportion of total volume ##
  ## update: convert volume to proportion of total hemisphere volume ##
  dplyr::mutate(
    # Total.Volume = rowSums(dplyr::across(.cols = c(Type2.L3.Frontal_L:brainstem_cerebellum))),
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
brain_vol_wide <- brain_vol |>
  dplyr::select(subj_id, visit, contains("scaled")) |>
  reshape(
    idvar = "subj_id", timevar = "visit", direction = "wide", sep = "_"
  ) |>
  # filter: select obs with complete brain data
  filter(
    if_all(.cols = contains("scaled"), ~ !is.na(.x))
  )

brain_vol_prop_wide <- brain_vol |>
  dplyr::select(subj_id, visit, contains("prop")) |>
  reshape(
    idvar = "subj_id", timevar = "visit", direction = "wide", sep = "_"
  ) |>
  # filter: select obs with complete brain data
  filter(
    if_all(.cols = contains("prop"), ~ !is.na(.x))
  )


## NOTE: k-means requires complete data -> impute? ##
set.seed(61724)
# imputation method of choice



# store vector of analytic brain regions
scaled_regions <- paste(brain_regions, "_scaled", sep = "")
scaled_sub_regions <- paste(sub_regions, "_scaled", sep = "")
prop_vol_regions <- paste0(brain_regions, "_prop")
prop_vol_sub_regions <- paste0(sub_regions, "_prop")

## save data
saveRDS(brain_vol_raw, "./data-ext/brain_vol_raw_20241219.rds")
saveRDS(brain_vol, "./data-ext/brain_vol_long_20241219.rds")
saveRDS(brain_vol_wide, "./data-ext/brain_vol_wide_20241219.rds")
saveRDS(brain_vol_prop_wide, "./data-ext/brain_vol_prop_wide_20241219.rds")

## save variable vectors
saveRDS(prop_vol_regions, "./data-ext/prop_vol_regions.rds")
saveRDS(prop_vol_sub_regions, "./data-ext/prop_vol_sub_regions.rds")