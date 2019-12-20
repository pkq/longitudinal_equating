library(tidyverse)
library(data.table)
needs(tidyverse, RColorBrewer)

dfs <- lapply(compare_equate_out2, data.frame, stringsAsFactors = FALSE)

equate_flat <- bind_rows(dfs) %>%
  arrange(raw) %>%
  distinct(raw, .keep_all = TRUE) %>%
  mutate(quart = ntile(raw, 4))

matched <- ttl_g %>%
  rename(raw = total) %>%
  left_join(equate_flat) %>%
  # shouldn't every raw score be represented?
  filter(!is.na(quart))

equate_sample <- matched %>%
  filter(benchmark == 1) %>%
  add_count(anon_id) %>%
  filter(season == 1, n == 3) %>%
  distinct(anon_id, quart) %>%
  rename(quart_fall = quart) %>%
  group_by(quart_fall) %>%
  sample_n(5) %>%
  mutate(keep = 1)

equate_plot <- matched %>%
  left_join(equate_sample) %>%
  filter(keep == 1, benchmark == 1) %>%
  gather(type, score, raw, starts_with("m.eq")) %>%
  mutate(wrap = str_c("quartile", quart_fall, " ", anon_id))


ggplot(data = equate_plot,
       aes(x = season,
           y = score,
           group = type,
           color = type,
           shape = type,
           linetype = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~wrap) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

