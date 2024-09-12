# plot wind rose over all interception obs

p <- weatherdash::wind_rose(ffr_met_wnd,
                            'datetime',
                            'wind_speed',
                            'wind_dir',
                            dir_res = 30,
                            ws_res = 1,
                            ws_max = 5,
                            # plot_title = 'FT',
                            spd_unit = 'm/s'
)

p

plotly::save_image(p, paste0('figs/automated_snowfall_event_periods/ft_wind_rose_allevents_snowing.png'), width = 2, height = 5)
