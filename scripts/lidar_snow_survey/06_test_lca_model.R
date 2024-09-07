# Script to test the vector based LCA model
# Overview: pulls in the observed LCA at the trajectory angle and then also
# models the LCA for the given trajectory

cor_smry <- cor_stats |>
  group_by(plot_name, group) |>
  summarise(peak_r2 = max(r2),
            phi_at_peak_r2 = phi_d[which.max(r2)])

pwl_best_phi <- cor_smry$phi_at_peak_r2[cor_smry$plot_name == 'PWL' & cor_smry$group == 'Integrated Zenith']
ft_best_phi <- cor_smry$phi_at_peak_r2[cor_smry$plot_name == 'FT' & cor_smry$group == 'Integrated Zenith']

vox_config_id <- "23_072_vox_len_0.25m_sa_gridgen_v2.0.0_sa"

# observed leaf contacts
pwl_obs_lca_rast <- rast(paste0(
  '../../analysis/lidar-processing/data/dsm_cpy_metrics/',
  vox_config_id,
  '_',
  'PWL_E',
  'integrated_p',
  pwl_best_phi,
  '_lca_0.25m_crop_mask.tif'
))
pwl_lca_vb <- pwl_obs_lca_rast |> values() |> mean(na.rm = T)

ft_obs_lca_rast <- rast(paste0(
  '../../analysis/lidar-processing/data/dsm_cpy_metrics/',
  vox_config_id,
  '_',
  'FSR_S',
  'integrated_p',
  ft_best_phi,
  '_lca_0.25m_crop_mask.tif'
))
ft_lca_vb <- ft_obs_lca_rast |> values() |> mean(na.rm = T)

# mod leaf contacts
pwl_cc_nadir_025 <- readRDS('../../analysis/lidar-processing/data/hemi_stats/pwl_lca_avg_event_theta_for_each_phi.rds') |>
  as.data.frame() |>
  filter(phi_d == 0)

pwl_cc_nadir <- pwl_cc_nadir_025 |>
  pull(lca) |>
  mean(na.rm = T)

ft_cc_nadir_025 <- readRDS('../../analysis/lidar-processing/data/hemi_stats/ft_lca_avg_event_theta_for_each_phi.rds') |>
  as.data.frame() |>
  filter(phi_d == 0)

ft_cc_nadir <- ft_cc_nadir_025 |>
  pull(lca) |>
  mean(na.rm = T)

# Calculate the resulting increase in leaf contact area based on trajectory angle ----

ft_nls_coefs <-
  readRDS('../../analysis/lidar-processing/data/models/ta_vs_lca_nls_coefs_ft.rds')

ft_lca_inc <- logistic_origin(x = ft_event_ta,
                              Asym = ft_nls_coefs['Asym'],
                              xmid = ft_nls_coefs['xmid'],
                              scal = ft_nls_coefs['scal'])  |> as.numeric()

pwl_nls_coefs <-
  readRDS('../../analysis/lidar-processing/data/models/ta_vs_lca_nls_coefs_pwl.rds')

pwl_lca_inc <- logistic_origin(x = pwl_event_ta,
                               Asym = pwl_nls_coefs['Asym'],
                               xmid = pwl_nls_coefs['xmid'],
                               scal = pwl_nls_coefs['scal']) |> as.numeric()

ft_lca_vb_mod <- ft_cc_nadir + ft_lca_inc

pwl_lca_vb_mod <- pwl_cc_nadir + pwl_lca_inc

# Compare mod vs obs
ft_lca_vb_mod
ft_lca_vb

pwl_lca_vb
pwl_lca_vb_mod
