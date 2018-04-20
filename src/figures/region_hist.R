p5 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  theme_pub() +
  facet_wrap(~region, ncol = 3, scales = 'free')

mx_cnt <- ggplot_build(p5)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(y, x, count, PANEL) %>%
  group_by(PANEL) %>%
  summarize(max_count = max(count)) %>%
  ungroup()
mx_info <- ggplot_build(p5)$data[[1]] %>%
  tbl_df %>%
  left_join(., mx_cnt, by = 'PANEL') %>%
  filter(y == max_count)

# check to see where the min. diffs fall in plot
firefreq_cent <- as.data.frame(extraction_df) %>%
  filter(region ==  "Central") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  #transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  ggtitle("Central") +
  theme_pub() +
  # facet_wrap(~region, ncol = 3, scales = 'free') +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "2"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "2"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 2 + x, y = 170, colour="red"), size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_east <- as.data.frame(extraction_df) %>%
  filter(region ==  "East") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  #transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  ggtitle("East") +
  theme_pub() +
  # facet_wrap(~region, ncol = 3, scales = 'free') +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "1"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "1"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 2 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

firefreq_west <- as.data.frame(extraction_df) %>%
  filter(region ==  "West") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  #transform(region = factor(region, levels=c("East", "Central", "West"))) %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  ggtitle("West") +
  theme_pub() +
  # facet_wrap(~region, ncol = 3, scales = 'free') +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "3"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "3"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 2 + x, y = 150, colour="red"), size = 4) +
  theme(legend.position = "none")

g <- arrangeGrob(firefreq_east, firefreq_cent, firefreq_west, ncol = 3)

ggsave("results/region/bui/bui_hist_region.pdf", g, width = 8, height = 4, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))
