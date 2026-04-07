library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyverse)

##Time to reach ODt post heat shock
data = read.csv("csv files/NaturalStrains_Plate01_post55C_29Sep2025.csv")
metadata = read.csv("csv files/NaturalStrainsPlate01_metadata.csv")
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
iterations = dim(metadata)[1]
df_55 <- data.frame(Strain = character(iterations),Replicate = character(iterations),Time = numeric(iterations))
ODt = 0.195
for(i in 1:(dim(metadata)[1]))
{
  df_55$Strain[i] = metadata$Strain[i]
  df_55$Replicate[i] = metadata$Replicate[i]
  well = metadata$Well[i]
  OD = data[[well]]
  for(j in 2:length(OD))
  {
    if(OD[j-1]>0.05)
    {
      rate = (log(OD[j-1]) - log(OD[j-11])) / (data$Time[j-1] - data$Time[j-11])
      OD[j] = OD[j-1]*exp(rate*(1/6))
    }
    if(OD[j]>ODt)
    {
        df_55$Time[i] = data$Time[j]
        break
    }
  }
}
df_55 <- df_55[df_55$Time != 0, ]
##Time to reach ODt for 35C
data = read.csv("csv files/NaturalStrains_Plate01_35C_29Sep2025.csv")
df_35 <- data.frame(Strain = character(iterations),Replicate = character(iterations),Time = numeric(iterations))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:iterations)
{
  df_35$Strain[i] = metadata$Strain[i]
  df_35$Replicate[i] = metadata$Replicate[i]
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
df_35 <- df_35[df_35$Time != 0, ]
#For 30C Methanol - 
data = read.csv("csv files/NaturalStrains_Plate01_30C_29Sep2025.csv")
df_s <- data.frame(Strain = character(iterations),Replicate = character(iterations),Time = numeric(iterations))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:iterations)
{
  df_s$Strain[i] = metadata$Strain[i]
  df_s$Replicate[i] = metadata$Replicate[i]
  well = metadata$Well[i]
  OD = data[[well]]
  if(is.null(OD))
  {
    next
  }
  for(j in 1:length(OD))
  {
    if(OD[j]>ODt)
    {
        df_s$Time[i] = data$Time[j]
        break
    }
  }
}
df_s <- df_s[df_s$Time != 0, ]

summary_55 <- df_55 %>%
  group_by(Strain) %>%
  summarize(mean_time_55 = mean(Time, na.rm = TRUE),sd_time_55   = sd(Time, na.rm = TRUE),.groups = 'drop')
summary_35 <- df_35 %>%
  group_by(Strain) %>%
  summarize(mean_time_35 = mean(Time, na.rm = TRUE),sd_time_35   = sd(Time, na.rm = TRUE),.groups = 'drop')
summary_s <- df_s %>%
  group_by(Strain) %>%
  summarize(mean_time_s = mean(Time, na.rm = TRUE),sd_time_s   = sd(Time, na.rm = TRUE),.groups = 'drop')
summary_combined <- summary_55 %>%
  left_join(summary_35, by = "Strain") %>%
  left_join(summary_s,  by = "Strain")
summary_combined <- summary_combined %>%
  mutate(norm_mean_55 =  mean_time_s/mean_time_55, norm_mean_35 = mean_time_s/mean_time_35,
         norm_sd_55 = norm_mean_55 * sqrt((sd_time_55 / mean_time_55)^2 + (sd_time_s / mean_time_s)^2),
         norm_sd_35 = norm_mean_35 * sqrt((sd_time_35 / mean_time_35)^2 + (sd_time_s / mean_time_s)^2))
summary_set01 = summary_combined


##Set 02
##Time to reach ODt post heat shock
data = read.csv("csv files/NaturalStrains_Plate02_post55C_02Oct2025.csv")
metadata = read.csv("csv files/NaturalStrainsPlate02_metadata.csv")
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
iterations = dim(metadata)[1]
df_55 <- data.frame(Strain = character(iterations),Replicate = character(iterations),Time = numeric(iterations))
for(i in 1:(dim(metadata)[1]))
{
  df_55$Strain[i] = metadata$Strain[i]
  df_55$Replicate[i] = metadata$Replicate[i]
  well = metadata$Well[i]
  OD = data[[well]]
  
  for(j in 1:length(OD))
  {
    if(OD[j]>ODt)
    {
      df_55$Time[i] = data$Time[j]
      break
    }
  }
}
df_55 <- df_55[df_55$Time != 0, ]
##Time to reach ODt for 35C
data = read.csv("csv files/NaturalStrains_Plate02_35C_02Oct2025.csv")
df_35 <- data.frame(Strain = character(iterations),Replicate = character(iterations),Time = numeric(iterations))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:iterations)
{
  df_35$Strain[i] = metadata$Strain[i]
  df_35$Replicate[i] = metadata$Replicate[i]
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
df_35 <- df_35[df_35$Time != 0, ]
#For 30C Methanol - 
data = read.csv("csv files/NaturalStrains_Plate02_30C_02Oct2025.csv")
df_s <- data.frame(Strain = character(iterations),Replicate = character(iterations),Time = numeric(iterations))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:iterations)
{
  df_s$Strain[i] = metadata$Strain[i]
  df_s$Replicate[i] = metadata$Replicate[i]
  well = metadata$Well[i]
  OD = data[[well]]
  if(is.null(OD))
  {
    next
  }
  for(j in 1:length(OD))
  {
    if(OD[j]>ODt)
    {
      df_s$Time[i] = data$Time[j]
      break
    }
  }
}
df_s <- df_s[df_s$Time != 0, ]

summary_55 <- df_55 %>%
  group_by(Strain) %>%
  summarize(mean_time_55 = mean(Time, na.rm = TRUE),sd_time_55   = sd(Time, na.rm = TRUE),.groups = 'drop')
summary_35 <- df_35 %>%
  group_by(Strain) %>%
  summarize(mean_time_35 = mean(Time, na.rm = TRUE),sd_time_35   = sd(Time, na.rm = TRUE),.groups = 'drop')
summary_s <- df_s %>%
  group_by(Strain) %>%
  summarize(mean_time_s = mean(Time, na.rm = TRUE),sd_time_s   = sd(Time, na.rm = TRUE),.groups = 'drop')
summary_combined <- summary_55 %>%
  left_join(summary_35, by = "Strain") %>%
  left_join(summary_s,  by = "Strain")
summary_combined <- summary_combined %>%
  mutate(norm_mean_55 =  mean_time_s/mean_time_55, norm_mean_35 = mean_time_s/mean_time_35,
         norm_sd_55 = norm_mean_55 * sqrt((sd_time_55 / mean_time_55)^2 + (sd_time_s / mean_time_s)^2),
         norm_sd_35 = norm_mean_35 * sqrt((sd_time_35 / mean_time_35)^2 + (sd_time_s / mean_time_s)^2))
summary_set02 = summary_combined



combined <- bind_rows(summary_set01, summary_set02)
print(combined)
combined <- combined %>% 
  filter(!Strain %in% c("populii"))

tiff("Figures/Tradeoff_NaturalStrains_Methanol.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(combined, aes(y = norm_mean_35, x = norm_mean_55)) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_errorbarh(aes(xmin = norm_mean_55 - norm_sd_55,xmax = norm_mean_55 + norm_sd_55), width = 0.01, linewidth=1) +
  geom_errorbar(aes(ymin = norm_mean_35 - norm_sd_35,ymax = norm_mean_35 + norm_sd_35), width = 0.01, linewidth=1) +
  geom_point(shape = 23, size = 5, fill = "gray70", color = "black", stroke = 2) +
  ylab("Performance 35°C") + xlab("Performance Heat Shock") +
  theme_classic() + theme(text = element_text(size = 24), legend.position = "none")+
  ylim(0.4,1.3)+xlim(0.1,1)
dev.off()

methanol_combined = combined
