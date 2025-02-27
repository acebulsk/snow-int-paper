# This script sequence creates some processed datasets requed for the
# scl_snowfall_event analysis. First it runs a script to zero weighed tree at
# start of specified events, and then it bins met data.

# setup ----
source('scripts/00_define_global_attributes.R')
source('scripts/01_load_processed_data.R')

quality_th <- 3

# load_suffix <- 'fsd_closed_0.88'
# load_suffix <- 'fsd_cal_for_each_trough'
load_suffix <- 'fsd_cal_for_each_trough_vza_60'


to_long_short <- function(from, to, class, quality, notes, event_id){
  datetime <- seq(from, to, 900)

  out <- data.frame(datetime, class, quality, notes, event_id)

  return(out)
}

# load data ----
canopy_snow_events_pre_post <- read.csv(paste0(paper_lysimeter_data_path,
                                               'snow_in_canopy_pre_and_post_snowfall.csv'))  |>
  mutate(from =  as.POSIXct(from, tz = 'Etc/GMT+6'),
         to = as.POSIXct(to, tz = 'Etc/GMT+6'),
         event_id = as.Date(from, tz = 'Etc/GMT+6'))

w_tree_raw <- readRDS(paste0(
  paper_lysimeter_data_path,
  'treefort_weighed_tree_cal_kg_m2_plv_',
                             load_suffix,
                             '.rds'))

canopy_snow_long_pre_post <- purrr::pmap_dfr(canopy_snow_events_pre_post, to_long_short)

bad_events <- c(NA)

canopy_snow_long_pre_post <- canopy_snow_long_pre_post |> filter(!event_id %in% bad_events)
cnpy_snow_events <- canopy_snow_long_pre_post$event_id |> unique()

cnpy_snow_events <- cnpy_snow_events[!cnpy_snow_events %in% bad_events]

weighed_tree_df_fltr <- w_tree_raw |>
  left_join(canopy_snow_long_pre_post) |> # this is the df I created to select periods of time where snow is in the canopy and no above canopy precip is occuring
  filter(is.na(event_id) == F,
         event_id %in% cnpy_snow_events)

# zero the tree at the start of each event

event_start_vals <- weighed_tree_df_fltr |>
  group_by(event_id, name, tree_cal_cc) |>
  summarise(start_time = min(datetime, na.rm = T),
            start_val = nth(value, which.min(datetime))) # grab instrument value at begining of event |>

weighed_tree_zeroed_pre_post_cnpy_snow <- weighed_tree_df_fltr |>
  left_join(event_start_vals, by = c('event_id', 'name', 'tree_cal_cc')) |>
  mutate(value = value - start_val,
         hours = as.numeric(difftime(datetime, start_time, 'hours'))/(60*60),
         value = case_when(
           value < 0 ~ 0,
           T ~ value
         )) |>
  select(-start_val) |>
  # rename(tree_mm = value) |>
  # rbind(inc_snow) |>
  filter(is.na(event_id) == F)

# fix some noisey points

good1 <- as.POSIXct('2023-06-15 11:15:00', tz = 'Etc/GMT+6')
good2 <- as.POSIXct('2023-06-15 11:45:00', tz = 'Etc/GMT+6')

bad1 <- as.POSIXct('2023-06-15 11:30:00', tz = 'Etc/GMT+6')
bad2 <- as.POSIXct('2023-06-15 12:00:00', tz = 'Etc/GMT+6')

fill_val1 <- weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == good1,]$value
fill_val2 <- weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == good2,]$value

weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == bad1,]$value <- fill_val1
weighed_tree_zeroed_pre_post_cnpy_snow[weighed_tree_zeroed_pre_post_cnpy_snow$datetime == bad2,]$value <- fill_val2

# ggplot(weighed_tree_zeroed_pre_post_cnpy_snow, aes(datetime, value, colour = name, group = name)) +
#   geom_line() +
#   facet_grid(rows = vars(tree_cal_cc))
# plotly::ggplotly()

saveRDS(
  weighed_tree_zeroed_pre_post_cnpy_snow,
  paste0(
    'data/lysimeter-data/processed/zero_weighed_tree_kg_m2_pre_post_cnpy_snow_',
    load_suffix,
    '.rds'
  )
)
