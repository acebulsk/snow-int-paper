# calculate interception efficiency (I/P) for fresh snow survey periods

fsd_all <- fsd_all |>
  inner_join(ffr_met_avg_event |> select(event_id, del_sf), by = 'event_id') |>
  mutate(I = del_sf - del_tf,
         IP = I/del_sf) |>
  filter(IP > 0,
         IP < 1)

fsd_all_avg_event <- fsd_all |>
  group_by(event_id) |>
  summarise(
    mean_del_tf = mean(del_tf),
    sd_del_tf = sd(del_tf),
    mean_I = mean(I),
    sd_I = sd(I),
    mean_IP = mean(IP),
    sd_IP = sd(IP)) |>
  left_join(ffr_met_avg_event)

ggplot(fsd_all_avg_event |> pivot_longer(c(mean_u, med_u)), aes(value, mean_IP, colour = name)) + geom_point()

ggplot(fsd_all_avg_event, aes(t, IP)) + geom_point()
