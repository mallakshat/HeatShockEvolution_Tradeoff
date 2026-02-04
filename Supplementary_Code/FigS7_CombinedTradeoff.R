##From the Scripts for Fig 5B and 5C, the summary_df variables were saved into isolates_combined and methanol_combined respectively
##These variables are used here to make the combined tradeoff figure and for the ancova calculations

cols <- c("norm_mean_35", "norm_mean_55", "Source")
combined <- rbind(isolates_combined[,cols], methanol_combined[,cols])
combined$Source <- factor(combined$Source)

ancova_model <- lm(
  norm_mean_35 ~ norm_mean_55 * Source,
  data = combined
)

summary(ancova_model)

tiff("Figures/Combined_Tradeoff.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(combined, aes(x = norm_mean_55, y = norm_mean_35, fill = Source, shape = Source)) +
  geom_smooth(aes(color=Source),method = "lm", se = FALSE, linewidth=1) +
  geom_point(size = 6, stroke = 1.25) +
  theme_classic()+theme(text = element_text(size = 26),axis.text.x = element_text(size = 26),legend.position = "none")+
  xlab("Performance Heat Shock") + ylab("Performance 35 ˚C")+
  scale_fill_manual(values = c("Evolution" = "gray70","Methanol" = "red")) +
  scale_shape_manual(values = c("Evolution" = 21, "Methanol" = 23))+
  scale_color_manual(values = c("Evolution" = "gray70", "Methanol" = "red")) +
  xlim(0.15,1.05)+ylim(0.35,1.25)
dev.off()

anova(model)
