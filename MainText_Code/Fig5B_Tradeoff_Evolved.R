##Figure 4B - Evolved isolates tradeoff
library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)

ODt=0.07
## After heat shock data
data = read.csv("CsvFilesForFigures/Evolved_GrowthCurve_HS.csv")
metadata = read.csv("CsvFilesForFigures/Evolved_GrowthCurve_HS_metadata.csv")

# Handle blank columns and subtract the blank from OD
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%
  mutate(across(-Time, ~ .x - blank))
data$Time = data$Time/60
metadata <- metadata %>%
  filter(Isolate != "Blank")

df_hs <- data.frame(Isolate = character(44),Replicate = character(44),Time = numeric(44),Condition = character(44))

for(i in 1:44)
{
  df_hs$Isolate[i] = metadata$Isolate[i]
  df_hs$Replicate[i] = metadata$Replicate[i]
  df_hs$Condition[i] = "HS"
  well = metadata$Well[i]
  OD = data[[well]]
  for(j in 1:length(OD))
  {
    if(OD[j]>ODt)
    {
      df_hs$Time[i] = data$Time[j]
      break
    }
  }
}

## 35C data
data = read.csv("CsvFilesForFigures/Evolved_GrowthCurve_35C.csv")
metadata = read.csv("CsvFilesForFigures/Evolved_GrowthCurve_35C_metadata.csv")

blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%
  mutate(across(-Time, ~ .x - blank))
data$Time = data$Time/60
metadata <- metadata %>%
  filter(Isolate != "Blank")

df_35 <- data.frame(Isolate = character(44),Replicate = character(44),Time = numeric(44),Condition = character(44))

for(i in 1:44)
{
  df_35$Isolate[i] = metadata$Isolate[i]
  df_35$Replicate[i] = metadata$Replicate[i]
  df_35$Condition[i] = "35"
  well = metadata$Well[i]
  OD = data[[well]]
  for(j in 1:length(OD))
  {
    if(OD[j]>ODt)
    {
      df_35$Time[i] = data$Time[j]
      break
    }
  }
}

##Relative to Ancestor Plot!
df_55 = df_hs
df_55 <- df_55[df_55$Time != 0, ]
df_55$Time = (11/df_55$Time)
df_high = df_35
df_high <- df_high[df_high$Time != 0, ]
df_high$Time = (11/df_high$Time)
#sem <- function(x) sd(x) / sqrt(length(x))
sem <- function(x) sd(x) 

summary_55 <- df_55 %>%
  group_by(Isolate) %>%
  summarize(mean_time_55 = mean(Time, na.rm = TRUE),
            sem_time_55 = sem(Time),.groups = 'drop')

summary_35 <- df_high %>%
  group_by(Isolate) %>%
  summarize(mean_time_35 = mean(Time, na.rm = TRUE),
            sem_time_35 = sem(Time),.groups = 'drop')

summary_combined <- left_join(summary_55, summary_35, by = "Isolate")
print(summary_combined)

tiff("Figures/Tradeoff_Isolates.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(summary_combined, aes(y = mean_time_35, x = mean_time_55)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #geom_errorbar(aes(ymin = mean_time_55 - sem_time_55,ymax = mean_time_55 + sem_time_55), width = 0,linewidth=1) +
  #geom_errorbarh(aes(xmin = mean_time_35 - sem_time_35,xmax = mean_time_35 + sem_time_35), height = 0,linewidth=1) +
  geom_point(aes(fill = ifelse(Isolate == "Anc", "black", "gray70")),shape = 21, size = 5, color = "black", stroke = 2) +
  scale_fill_identity()+
  ylab("Performance 35°C") + xlab("Performance Heat Shock") +
  theme_classic() + theme(text = element_text(size = 24), legend.position = "none")+
  ylim(0.35,0.95)+xlim(0.2,0.8)
dev.off()
