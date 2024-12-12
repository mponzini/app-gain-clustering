# data prep
pacman::p_load(openxlsx, dplyr, tibble, tidyr)

# load brain data
brain_vol_raw <- read.csv(
  paste0(
    "S:/MIND/IDDRC Cores/",
    "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
    "Nordahl_R01MH10443801/MultivariateModeling/Data/",
    "longitudinal_clustering_brain_volume_dataset_2024-09-19.csv"
  )
)

# names(brain_vol_raw)
covariates <- c("sex", "app_diagnosis")
brain_regions <- c("Frontal", "Parietal", "Temporal", "Limbic", "Occipital",
                   "ventricular_csf", "brainstem_cerebellum")

sub_regions <- c("Type2.L3.Frontal_L", "Type2.L3.Frontal_R", 
                 "Type2.L3.Parietal_L", "Type2.L3.Parietal_R", 
                 "Type2.L3.Temporal_L", "Type2.L3.Temporal_R", 
                 "Type2.L3.Limbic_L", "Type2.L3.Limbic_R", 
                 "Type2.L3.Occipital_L", "Type2.L3.Occipital_R",
                 "ventricular_csf", "brainstem_cerebellum")

# process data
brain_vol <- brain_vol_raw |> 
  dplyr::select(-X) |> 
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
  mutate(
    # convert to numeric
    across(
      .cols = c(Frontal, Parietal, Temporal, Limbic, Occipital, 
                ventricular_csf, brainstem_cerebellum),
      ~ as.numeric(.x)
    )
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
  dplyr::mutate(
    Total.Volume = rowSums(dplyr::across(.cols = c(Type2.L3.Frontal_L:brainstem_cerebellum))),
    dplyr::across(
      .cols = c(Type2.L3.Frontal_L:brainstem_cerebellum),
      ~ .x / Total.Volume,
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
prop_vol_sub_regions <- paste0(sub_regions, "_prop")
