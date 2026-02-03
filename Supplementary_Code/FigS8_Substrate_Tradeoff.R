library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyverse)

data = read.csv("Jun14/NaturalStrains_14Jun2025_post55C_Succinate.csv")
metadata = read.csv("Jun14/NaturalStrains_02_metadata.csv")
data$Time = data$Time/60
df_55 <- data.frame(Strain = character(28),Replicate = character(28),Time = numeric(28))
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
ODt = 0.3
for(i in 1:28)
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
data = read.csv("Jun16/NaturalStrains_16Jun2025_35C_Succinate.csv")
df_35 <- data.frame(Strain = character(28),Replicate = character(28),Time = numeric(28))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:28)
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
#For 30C Succinate - 
data = read.csv("Jun16/NaturalStrains_16Jun2025_30C_Succinate.csv")
df_s <- data.frame(Strain = character(28),Replicate = character(28),Time = numeric(28))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:28)
{
  df_s$Strain[i] = metadata$Strain[i]
  df_s$Replicate[i] = metadata$Replicate[i]
  well = metadata$Well[i]
  OD = data[[well]]
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

##Plate 02
##Time to reach ODt post heat shock
data = read.csv("July17/NaturalStrainsSet02_17July_post55C_30C_Succinate.csv")
metadata = read.csv("July17/NaturalStrainsSet02_metadata.csv")
data$Time = data$Time/60
df_55 <- data.frame(Strain = character(28),Replicate = character(28),Time = numeric(28))
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:28)
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
##Time to ODt for 35C
data = read.csv("July17/NaturalStrainsSet02_17July_35C_Succinate.csv")
df_35 <- data.frame(Strain = character(28),Replicate = character(28),Time = numeric(28))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:28)
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
#For 30C Succinate - 
data = read.csv("July17/NaturalStrainsSet02_17July_30C_Succinate.csv")
df_s <- data.frame(Strain = character(28),Replicate = character(28),Time = numeric(28))
data$Time = data$Time/60
blank_cols <- grep("^(A1|A8|F1|F8)$", names(data))
blank <- mean(unlist(data[, blank_cols]), na.rm = TRUE)
data <- data %>%mutate(across(-Time, ~ .x - blank))
metadata <- metadata %>%filter(Strain != "Blank")
for(i in 1:28)
{
  df_s$Strain[i] = metadata$Strain[i]
  df_s$Replicate[i] = metadata$Replicate[i]
  well = metadata$Well[i]
  OD = data[[well]]
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


#PLOT
combined <- bind_rows(summary_set01, summary_set02)
combined <- combined[-which(combined$Strain == "PA1")[1], ]
succinate_combined = combined

##The dataset methanol_combined is generated in the code for Fig 4C! Please see MainText_Code folder.
methanol_combined$Source <- "Methanol"
succinate_combined$Source <- "Succinate"
colnames(isolates_combined)[colnames(isolates_combined) == "mean_time_55"] <- "norm_mean_55"
colnames(isolates_combined)[colnames(isolates_combined) == "mean_time_35"] <- "norm_mean_35"
combined_df <- bind_rows(methanol_combined, succinate_combined)

tiff("Figures/Tradeoff_NaturalStrains_Combined.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(combined_df, aes(x = norm_mean_55, y = norm_mean_35, color = Source)) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point(size=6,color="black",shape=18)+geom_point(size = 4,shape=18) +
  theme_classic() + theme(text = element_text(size = 24), legend.position = "none")+
  scale_color_manual(values = c("Methanol" = "red", "Succinate" = "deepskyblue")) +
  ylab("Performance 35°C") + xlab("Performance Heat Shock") + 
  xlim(0.1,1.1)+ylim(0.4, 1.4)
dev.off()
