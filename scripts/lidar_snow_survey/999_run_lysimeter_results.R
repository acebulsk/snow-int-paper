# Script to run lysimeter results sequence

source('scripts/00_define_global_attributes.R')
source('scripts/01_load_processed_data.R')

source('scripts/scl_snowfall_events/01_plot_event_timeseries_and_windrose.R')
source('scripts/scl_snowfall_events/02_event_met_stats_histogram_and_table.R')
source('scripts/scl_snowfall_events/03_plot_cml_event_snowload.R')
source('scripts/scl_snowfall_events/04_plot_avg_event_met_scl_IP.R')
source('scripts/scl_snowfall_events/05_scatterplot_15min_binned_met_trough_IP.R')
source('scripts/scl_snowfall_events/06_stats_on_15min_met_vs_ip.R')
