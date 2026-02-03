###30C tradeoff figure
data30 = read.csv("Supplementary/30C tradeoff/QurveAnalysis_Evolved_30C.csv")
data55 = read.csv("Supplementary/30C tradeoff/QurveAnalysis_Evolved_HS.csv")
sem <- function(x) sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
#sem <- function(x) sd(x, na.rm = TRUE) 
summary30 <- data30 %>%
  group_by(Isolate) %>%
  summarize(mean_gr_rate = mean(gr_rate, na.rm = TRUE),sd_gr_rate = sem(gr_rate),
            mean_lag = mean(lag, na.rm = TRUE),sd_lag = sem(lag))
summary55 <- data55 %>%
  group_by(Isolate) %>%
  summarize(mean_gr_rate = mean(gr_rate, na.rm = TRUE),sd_gr_rate = sem(gr_rate),
            mean_lag = mean(lag, na.rm = TRUE),sd_lag = sem(lag))
combined <- merge(summary30, summary55, by = "Isolate", suffixes = c("_30", "_55"))
combined$color <- ifelse(combined$Isolate == "0A", "black", "gray")

tiff("Supplementary/30C tradeoff/30CTradeoff.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(combined, aes(x = mean_lag_55, y = mean_gr_rate_30)) +
  #geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_errorbarh(aes(xmin = mean_lag_55 - sd_lag_55,xmax = mean_lag_55 + sd_lag_55),height = 0.001, color = "gray50") +
  geom_errorbar(aes(ymin = mean_gr_rate_30 - sd_gr_rate_30,ymax = mean_gr_rate_30 + sd_gr_rate_30),width = 0.1, color = "gray50") +  
  geom_point(size=5, color="black")+geom_point(aes(color = color), size = 3)+ 
  scale_color_identity() +
  theme_classic() + theme(text = element_text(size = 24), legend.position = "none")+
  ylab("Growth Rate (30˚C)") + xlab("Lag (after Heat Shock)")+
  xlim(0,20)+ylim(0.15,0.25)
dev.off()

model <- lm(mean_gr_rate_30 ~ mean_lag_55, data = combined)
summary(model)$coefficients[2, 1] 
summary(model)$r.squared           
