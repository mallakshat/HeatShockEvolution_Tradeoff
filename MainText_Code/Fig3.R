##Figure 3 - Lag time and growth rate for evolved isolates.
data = read.csv("CsvFilesForFigures/T20_Isolates_Qurve_Analysis.csv")

summary_df <- data %>%
  filter(Isolate != "0A") %>%
  group_by(Isolate) %>%
  summarise(mean_gr_rate = mean(gr_rate, na.rm = TRUE),
    #se_gr_rate = sd(gr_rate, na.rm = TRUE) / sqrt(n()),
    se_gr_rate = sd(gr_rate, na.rm = TRUE),
    mean_lag = mean(lag, na.rm = TRUE),
    #se_lag = sd(lag, na.rm = TRUE) / sqrt(n()))
    se_lag = sd(lag, na.rm = TRUE))


#Lag
tiff("Figures/Evolved_Lag.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(summary_df, aes(x = Isolate, y = mean_lag)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 16.9, ymax = 18.5),fill = "gray80", alpha = 0.5) +
  geom_hline(yintercept = 17.7, linetype = "dashed", color = "black", linewidth = 0.6) +
  #geom_hline(yintercept = 18.5, linetype = "dashed", color = "gray90", linewidth = 0.6) +
  #geom_hline(yintercept = 16.9, linetype = "dashed", color = "gray90", linewidth = 0.6) +
  #geom_point(size = 5,color="black") + geom_point(size=3,color="gray70")+
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.8,linewidth=1) +
  geom_errorbar(aes(ymin = mean_lag - se_lag, ymax = mean_lag + se_lag), width = 0, linewidth = 1) +
  geom_jitter(data = data %>% filter(Isolate != "0A"), aes(x = Isolate, y = lag),width = 0.15, size = 2.5, color = "black", alpha = 0.7) +
  theme_classic()+theme(text = element_text(size = 26),axis.text.x = element_text(size = 12),legend.position = "none")+
  ylim(0,20)+
  ylab("Lag (hrs)") + xlab("Isolate")
dev.off()


#Growth rate
tiff("Figures/Evolved_GrowthRate.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(summary_df, aes(x = Isolate, y = mean_gr_rate)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.20011, ymax = 0.21389),fill = "gray80", alpha = 0.5) +
  geom_hline(yintercept = 0.207, linetype = "dashed", color = "black", linewidth = 0.6) +
  #geom_hline(yintercept = 0.21389, linetype = "dashed", color = "gray90", linewidth = 0.6) +
  #geom_hline(yintercept = 0.20011, linetype = "dashed", color = "gray90", linewidth = 0.6) +
  geom_bar(stat = "identity", fill = "gray70", color = "black", width = 0.8,linewidth=1) +
  geom_errorbar(aes(ymin = mean_gr_rate - se_gr_rate, ymax = mean_gr_rate + se_gr_rate), width = 0, linewidth = 1) +
  geom_jitter(data = data %>% filter(Isolate != "0A"), aes(x = Isolate, y = gr_rate),width = 0.15, size = 2.5, color = "black", alpha = 0.7) +
  #geom_point(size = 5,color="black") + geom_point(size=3,color="gray70")+
  theme_classic()+theme(text = element_text(size = 26),axis.text.x = element_text(size = 12),legend.position = "none")+
  ylim(0,0.25)+
  xlab("Isolate")+ylab(expression("Growth Rate (hr"^{-1}*")"))
dev.off()


#Statistics
data$Isolate <- factor(data$Isolate)
data$Isolate <- relevel(data$Isolate, ref = "0A")

ttest_vs_0A <- function(var) {
  isolates <- unique(data$Isolate)
  isolates <- isolates[isolates != "0A"]
  
  results <- lapply(isolates, function(iso) {
    t_out <- t.test(data[[var]][data$Isolate == iso],
                    data[[var]][data$Isolate == "0A"])
    data.frame(Isolate = iso,
               p_value = t_out$p.value,
               mean_diff = mean(data[[var]][data$Isolate == iso], na.rm=TRUE) -
                 mean(data[[var]][data$Isolate == "0A"], na.rm=TRUE))
  })
  
  do.call(rbind, results)
}

lag_results <- ttest_vs_0A("lag")
gr_results <- ttest_vs_0A("gr_rate")
