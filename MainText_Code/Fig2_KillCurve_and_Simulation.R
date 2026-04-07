##Figure 2A - Heat Shock Kill Curve
data = read.csv("CsvFilesForFigures/HS_DeathRate.csv")

summary_data <- data %>%
  group_by(Time) %>%
  #summarise(mean_count = mean(Count),sd = sd(Count),.groups = "drop")%>%
  summarise(mean_count = mean(Count),se = sd(Count) / sqrt(n()),.groups = "drop")
  #mutate(ymin = pmax(mean_count - sd, 1),ymax = mean_count + sd)

tiff("Figures/HS_DeathRate.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(summary_data, aes(x = Time, y = log10(mean_count))) +
  #geom_errorbar(aes(ymin = log10(mean_count - se), ymax = log10(mean_count + se)), width = 0.2,linewidth=1) +
  geom_errorbar(aes(ymin = log10(sd_lower), ymax = log10(sd_upper)),width = 0.2, linewidth = 1) +
  #geom_errorbar(aes(ymin = mean_count+ymin, ymax = mean_count+ymax, width = 0.2,linewidth=1)) +
  geom_line(linetype = "dashed",linewidth = 0.75) + 
  geom_point(size = 5,color="black") + geom_point(size=3,color="gray70")+
  theme_classic()+theme(text = element_text(size = 26))+ xlab("Time (mins)")+ylab("Viability (CFU/ml)")+
  scale_y_continuous(breaks = 2:9,labels = parse(text = paste0("10^", 2:9)), limits=c(3,9))
dev.off()

##Figure 2B - Heat Shock Growth Curve
data = read.csv("CsvFilesForFigures/GrowthCurves_Ancestor.csv")
metadata = read.csv("CsvFilesForFigures/GrowthCurves_Ancestor_metadata.csv")
data$Time = data$Time/60
##Add simulated data from only death
simulation = numeric(length(data$Time))
frac = 0.5645331
simulation[1] = 0.01*frac
lag = 0
gr_rate = 0.21
for(i in 2:length(data$Time))
{
  if(data$Time[i]<=5)
  {
    simulation[i] = simulation[i-1]
  } else {
    simulation[i] = simulation[i-1]+gr_rate*(data$Time[i]-data$Time[i-1])*simulation[i-1]
  }
  if(simulation[i]>0.1501)
  {
    simulation[i]=0.15
  }
}
data$XX = simulation
data$XX = data$XX+0.08599405

data_long <- data %>%
  pivot_longer(cols = -Time, names_to = "Well", values_to = "OD") %>%
  left_join(metadata, by = "Well")  # Merge with metadata

blank <- data_long %>%
  filter(Condition == "Blank") %>%
  summarize(blank = mean(OD, na.rm = TRUE)) %>%
  pull(blank)

data_long <- data_long %>%
  mutate(OD = OD - blank)

data_long_plot <- data_long %>%
  filter(Replicate == "AA", Condition %in% c("Heat Shock", "Not")) %>%
  group_by(Condition, Well) %>%
  mutate(ColorGroup = paste(Condition, cur_group_id())) %>%
  ungroup()

color_values <- c("Heat Shock 3" = "#fbb4b9","Heat Shock 2" = "#f768a1","Heat Shock 1" = "#c51b8a",
                  "Not 7"= "#bae4b3","Not 6"= "#74c476","Not 5"= "#31a354",
                  "Heat Shock 4" = "gray50")

tiff("Figures/Ancestor_GrowthCurves.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(data_long_plot, aes(x = Time, y = OD, group = Well, color = ColorGroup)) +
  #geom_point(aes(fill = ColorGroup), shape = 21, size = 2, color = "black", stroke = 0.75) +
  geom_line(data = subset(data_long_plot, ColorGroup == "Heat Shock 4"),aes(color = ColorGroup), linewidth = 2) +
  geom_point(data = subset(data_long_plot, ColorGroup != "Heat Shock 4"),aes(fill = ColorGroup), shape = 21, size = 2, color = "black", stroke = 0.75) +
  scale_y_log10(limits=c(0.005,0.18) ,breaks = c(0.01, 0.02, 0.05, 0.1, 0.18),labels = scales::label_number()) +
  scale_fill_manual(values = color_values) +
  scale_color_manual(values = color_values) +
  xlab("Time (hrs)") + ylab(expression(OD[600])) + xlim(0,35)+ 
  theme_classic() + theme(text = element_text(size = 26), legend.position = "none")
dev.off()
