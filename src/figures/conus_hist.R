# Plot the histogram of built-up area
# The burned area/fire was long tailed (range 0.01-3000) - had to log transform
p4 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  theme_pub()

mx_cnt <- ggplot_build(p4)$data[[1]] %>%
  tbl_df %>%
  dplyr::select(y, x, count) %>%
  mutate(max_count = max(count)) %>%
  filter(y == max_count)

p4 <- as.data.frame(extraction_df) %>%
  mutate(bui_ha = BUI*0.0001000000884) %>%
  as_tibble() %>%
  ggplot(aes(x = log(bui_ha))) +
  geom_histogram(binwidth = 0.5) +
  ylab("Counts") +
  xlab('log Built-up Intentsity (ha)') +
  theme_pub() +
  geom_vline(aes(xintercept = x), data = mx_cnt,
             linetype = "dashed", color  = "red") +
  geom_text(data = mx_cnt,
            aes(label=paste(round(exp(x),3), "ha", sep = " "), x = 3 + x, y = 590, colour="red"), size = 4) +
  theme(legend.position = "none")

ggsave("results/bui/bui_hist.pdf", p4, width = 4, height = 4, dpi=600, scale = 3, units = "cm") #saves g
system(paste0("aws s3 sync results ", s3_results))

