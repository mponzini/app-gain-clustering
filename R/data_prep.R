# data prep
pacman::p_load(openxlsx, dplyr, tibble, tidyr)

# load brain data
brain_vol_raw <- read.csv(
  paste0(
    "S:/MIND/IDDRC Cores/",
    "Core F_Biostatistics Bioinformatics and Research Design (BBRD)/",
    "Nordahl_R01MH10443801/MultivariateModeling/Data/",
    "longitudinal_clustering_brain_volume_dataset_2024-05-01.csv"
  )
)

# process data
brain_vol <- brain_vol_raw |> 
  dplyr::select(-X) |> 
  # remove duplicate rows
  unique() |>
  # calc brain vol by region
  mutate(
    Frontal = Type2.L3.Frontal_L + Type2.L3.Frontal_R,
    Parietal = Type2.L3.Parietal_L + Type2.L3.Parietal_R,
    Temporal = Type2.L3.Temporal_L + Type2.L3.Temporal_R,
    Limbic = Type2.L3.Limbic_L + Type2.L3.Limbic_R,
    Occipital = Type2.L3.Occipital_L + Type2.L3.Occipital_R,
    subj_id_numeric = as.numeric(gsub("-", "", subj_id))
  ) |>
  mutate(
    across(
      .cols = c(Frontal, Parietal, Temporal, Limbic, Occipital, 
                ventricular_csf, brainstem_cerebellum),
      ~ as.numeric(.x)
    )
  )

# names(brain_vol_raw)
covariates <- c("sex", "app_diagnosis", "scan_age")
brain_regions <- c("Frontal", "Parietal", "Temporal", "Limbic", "Occipital",
                   "ventricular_csf", "brainstem_cerebellum")

sub_regions <- c("Type2.L3.Frontal_L", "Type2.L3.Frontal_R", 
                 "Type2.L3.Parietal_L", "Type2.L3.Parietal_R", 
                 "Type2.L3.Temporal_L", "Type2.L3.Temporal_R", 
                 "Type2.L3.Limbic_L", "Type2.L3.Limbic_R", 
                 "Type2.L3.Occipital_L", "Type2.L3.Occipital_R",
                 "ventricular_csf", "brainstem_cerebellum")