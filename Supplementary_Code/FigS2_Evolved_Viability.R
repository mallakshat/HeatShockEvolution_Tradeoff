data <- read.csv("Supplementary_Data/Viability_HeatShock_EvolvedIsolates.csv")

#based on the dilutions in your data set
dilution_cols <- paste0("D", 0:5)

#Get true counts from data
processed <- data %>%
  pivot_longer(
    cols = all_of(dilution_cols),
    names_to = "Dilution",
    values_to = "raw_counts"
  ) %>%
  filter(raw_counts != "") %>%
  mutate(dilution_factor = 10 ^ as.numeric(str_remove(Dilution, "D"))) %>%
  separate_rows(raw_counts, sep = ",") %>%   # 🔑 keep individual measurements
  mutate(raw_counts = as.numeric(raw_counts),true_count = raw_counts * dilution_factor * 100) %>%
  group_by(Isolate, Condition) %>%
  summarise(mean_true_count = mean(true_count),sd_true_count = sd(true_count),.groups = "drop")
#  mutate(mean_true_count = mean_true_count * 100,sd_true_count = sd_true_count * 100)



#get fraction viable - names specific to your data set
fraction_df <- processed %>%
  pivot_wider(names_from  = Condition,values_from = c(mean_true_count, sd_true_count)) %>%
  mutate(fraction = mean_true_count_HS / mean_true_count_Control,
    fraction_sd = fraction * sqrt((sd_true_count_HS / mean_true_count_HS)^2 +(sd_true_count_Control / mean_true_count_Control)^2))

fraction_df <- fraction_df %>%
  mutate(Isolate = factor(Isolate, levels = c("Anc", setdiff(Isolate, "Anc"))))

plot_df <- fraction_df %>%
  filter(Isolate != "Anc")

tiff("Figures/Evolved_Viability.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(fraction_df, aes(x = Isolate, y = fraction)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.43, ymax = 0.69),fill = "gray80", alpha = 0.5) +
  geom_hline(yintercept = 0.56, linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_bar(stat = "identity", aes(fill = Isolate == "Anc"), color = "black", width = 0.8,linewidth=1) +
  #geom_point(size = 5,color="black") + geom_point(size=3,color="gray70")+
  scale_fill_manual(values = c("TRUE" = "white", "FALSE" = "gray70")) +
  geom_errorbar(aes(ymin = fraction - fraction_sd,ymax = fraction + fraction_sd),width = 0,linewidth = 1) +
  theme_classic()+theme(text = element_text(size = 26),axis.text.x = element_text(size = 12),legend.position = "none")+
  xlab("Isolate") + ylab("Fraction viable")+ 
  scale_y_continuous(limits = c(0, 1.3),breaks = seq(0, 1.24, by = 0.5)) 
dev.off()
