##Conceptual figure in Introduction - Fig 1
x_time = seq(1,100)

amp <- 1
freq <- 50

amp_noise <- amp + cumsum(rnorm(length(x_time), mean = 0, sd = 0.5))   # noisy amplitude
freq_noise <- freq + cumsum(rnorm(length(x_time), 0, 0.01))  # slow drift

y_temp <- 32 + amp_noise * sin(2 * pi * x_time / freq_noise)

df <- data.frame(x_time, y_temp)

tiff("Figures/VariableHeat.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(df, aes(x = x_time, y = y_temp)) +
  #geom_line(color = "blue", alpha = 0.6) +   # raw noisy line
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 27, ymax = 32, fill = "gray70", alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, color = "deepskyblue", linewidth = 2) +
  theme_classic() + xlab("Time") + ylab("Temperature") + 
  theme(text = element_text(size = 26))+geom_hline(yintercept = 30, linewidth=2, linetype = "dashed")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) + 
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + ylim(27,40)
dev.off()

x_time = seq(1,100)
y_temp <- rep(31.7, length(x_time))
y_temp[1] = 30
df <- data.frame(x_time, y_temp)
tiff("Figures/ConstantHeat.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(df, aes(x = x_time, y = y_temp))+
  geom_line(color="deepskyblue",linewidth=2)+ 
  geom_hline(yintercept = 30, linewidth=2, linetype = "dashed")+
  theme_classic() + xlab("Time") + ylab("Temperature") + 
  theme(text = element_text(size = 26))+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) + 
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + 
  ylim(27,40)
dev.off()


x_time = seq(1,100)
y_temp <- numeric(length(x_time))
for(i in 1:100)
{
  y_temp[i] = 30.2
  if(i >20 && i < 30)
  {
    y_temp[i]=37
  }
  if(i >50 && i < 60)
  {
    y_temp[i]=37
  }
  if(i >80 && i < 90)
  {
    y_temp[i]=37
  }
}

df <- data.frame(x_time, y_temp)
tiff("Figures/HeatShock.tiff", width = 5, height = 5, units = "in", res = 300)
ggplot(df, aes(x = x_time, y = y_temp))+
  geom_line(color="deepskyblue",linewidth=2)+ 
  geom_hline(yintercept = 30, linewidth=2, linetype = "dashed")+
  theme_classic() + xlab("Time") + ylab("Temperature") + 
  theme(text = element_text(size = 26))+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank()) + 
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank()) + 
  ylim(27,40)
dev.off()
