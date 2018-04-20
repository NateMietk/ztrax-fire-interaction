p4 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  theme_pub() +
  facet_wrap(~fire_bidecadal)

mx_cnt <- ggplot_build(p4)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(y, x, count, PANEL) %>%
  group_by(PANEL) %>%
  summarize(max_count = max(count)) %>%
  ungroup()
mx_info <- ggplot_build(p4)$data[[1]] %>%
  tbl_df %>%
  left_join(., mx_cnt, by = 'PANEL') %>%
  filter(y == max_count)

# check to see where the min. diffs fall in plot
firefreq_90 <- as.data.frame(extraction_df) %>%
  filter(fire_bidecadal ==  "1990") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(-6, 8)) +
  ggtitle("1990") +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "1"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "1"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 4 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_95 <- as.data.frame(extraction_df) %>%
  filter(fire_bidecadal ==  "1995") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(-6, 8)) +
  ggtitle("1995") +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "2"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "2"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 4 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_00 <- as.data.frame(extraction_df) %>%
  filter(fire_bidecadal ==  "2000") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(-6, 8)) +
  ggtitle("2000") +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "3"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "3"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 4 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_05 <- as.data.frame(extraction_df) %>%
  filter(fire_bidecadal ==  "2005") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(-6, 8)) +
  ggtitle("2005") +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "4"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "4"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 4 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_10 <- as.data.frame(extraction_df) %>%
  filter(fire_bidecadal ==  "2010") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(-6, 8)) +
  ggtitle("2010") +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "5"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "5"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 4 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

# check to see where the min. diffs fall in plot
firefreq_15 <- as.data.frame(extraction_df) %>%
  filter(fire_bidecadal ==  "2015") %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  scale_y_continuous(limits = c(0, 250)) +
  scale_x_continuous(limits = c(-6, 8)) +
  ggtitle("2015") +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = subset(mx_info, PANEL == "6"), linetype = "dashed", color  = "red") +
  geom_text(data=subset(mx_info, PANEL == "6"),
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 4 + x, y = 250, colour="red"), size = 4) +
  theme(legend.position = "none")

g <- arrangeGrob(firefreq_90, firefreq_95, firefreq_00, firefreq_05, firefreq_10, firefreq_15, nrow = 1)
ggsave("results/bui/bui_hist_per5yr.pdf", g, width = 12, height = 4, dpi=600, scale = 3, units = "cm") #saves g



