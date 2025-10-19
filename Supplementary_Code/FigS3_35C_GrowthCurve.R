#S3 - evolved vs ancestor at 35C
data = read.csv("CsvFilesForFigures/Evolved_GrowthCurve_35C.csv")
metadata = read.csv("CsvFilesForFigures/Evolved_GrowthCurve_35C_metadata.csv")

data$Time = data$Time/60
data_long <- data %>%
  pivot_longer(cols = -Time, names_to = "Well", values_to = "OD") %>%
  left_join(metadata, by = "Well")

blank <- data_long %>%
  filter(Isolate == "Blank") %>%
  summarize(blank = mean(OD, na.rm = TRUE)) %>%
  pull(blank)

data_long <- data_long %>%
  mutate(OD = OD - blank)

data_long <- data_long %>%
  filter(Isolate != "Blank")

plot_concentration <- function(env) {
  ggplot() +
    geom_point(data = data_long %>% filter(Isolate == "Anc"),aes(x = Time, y = OD, group = Well),color = "black", alpha = 0.5,size=2) +
    geom_point(data = data_long %>% filter(Isolate == "Anc"),aes(x = Time, y = OD, group = Well),color = "darkgray", alpha = 0.5,size=1) +
    geom_point(data = data_long %>% filter(Isolate == env),aes(x = Time, y = OD, group = Well),color="black",size=2) +
    geom_point(data = data_long %>% filter(Isolate == env),aes(x = Time, y = OD, color = Strain, group = Well),color="deepskyblue",size=1) +
    #scale_color_brewer(name = "Isolate", palette = "Set1")+
    theme_classic() + theme(text = element_text(size = 30))+
    ggtitle(env)+xlab(NULL)+ylab(NULL)+ylim(0,0.22)+xlim(0,35)
  #guides(color = guide_legend(override.aes = list(size = 7)))+xlim(0,40) 
}

tiff("Supplementary/35C growth curves/J1_HS.tiff", width = 5, height = 5, units = "in", res = 300)
plot_concentration("J1")
dev.off()
