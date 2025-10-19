data = read.csv("csv files/NaturalStrains_Plate01_30C_29Sep2025.csv")
xtime1 = data$Time/60
od30 = data$E2 - blank
data = read.csv("csv files/NaturalStrains_Plate01_35C_29Sep2025.csv")
xtime2 = data$Time/60
od35 = data$E2 - blank
data = read.csv("csv files/NaturalStrains_Plate01_post55C_29Sep2025.csv")
xtime3 = data$Time/60
od55 = data$E2 - blank

#combined figure for concept of performance
tiff("Figures/Example_Performance.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot() +
  geom_hline(yintercept = 0.195, linetype = "dashed", linewidth=1.25) +
  geom_segment(aes(x = 21.5, xend = 21.5, y = -0.02, yend = 0.195),color="#74c476", linetype = "solid", linewidth = 1.5)+
  geom_segment(aes(x = 39.75, xend = 39.75, y = -0.02, yend = 0.195),color="purple", linetype = "solid", linewidth = 1.5)+
  geom_segment(aes(x = 28, xend = 28, y = -0.02, yend = 0.195),color="red", linetype = "solid", linewidth = 1.5)+
  geom_point(aes(x = xtime1, y = od30, color = "OD30"), size = 3,color="black") + geom_point(aes(x = xtime1, y = od30, color = "OD30"), size = 1.75) +
  geom_point(aes(x = xtime2, y = od35, color = "OD35"), size = 3, color="black") + geom_point(aes(x = xtime2, y = od35, color = "OD35"), size = 1.75) +
  geom_point(aes(x = xtime3, y = od55, color = "OD55"), size = 3, color="black") + geom_point(aes(x = xtime3, y = od55, color = "OD55"), size = 1.75) +
  theme_classic() + scale_color_manual(values = c("OD30" = "#74c476", "OD55" = "purple", "OD35" = "red")) +
  theme(text = element_text(size = 26), legend.position = "none")+
  xlab("Time (hrs)") + ylab(expression(OD[600]))+
  ylim(-0.03,0.4) + xlim(0,45)
dev.off()
