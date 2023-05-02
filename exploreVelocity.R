library(tidyverse); library(scales)

vel <- read.csv('velocity.csv', header = T)
vel$rfl <- as.factor(vel$rfl)
vel$rfl <- recode(vel$rfl, '1' = 'Rifle 1', '2' = 'Rifle 2')

ggplot(vel, aes(x = vel, fill = amo)) + 
  geom_histogram(binwidth = 20, aes(fill = amo), 
                 color = 'black', alpha = 0.7, 
                 position = 'identity') + 
  scale_fill_manual(values = c('green', 'orange', 'yellow', 'purple')) +
  scale_y_continuous(limits = c(0,12), breaks = seq(0, 14, by = 2),
                     expand = c(0,0)) +
  scale_x_continuous(label = comma) +
  ggtitle('Histogram of Recorded Velocities by Ammunition \n') +
  xlab('\n Velocity (ft/s)') +
  ylab('Count \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 18),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 14),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.text = element_text(color = 'black', face = 'bold', 
                               hjust = 0.5, size = 12),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.9, 0.7)
  )
  
  
  
p <- ggplot(vel, aes(amo, vel)) + 
  geom_jitter(width = .15, shape = 21, 
              aes(fill = rfl), size = 3) +
  scale_fill_manual(values = c('red', 'blue')) +
  geom_boxplot(fill = 'white', color = 'black', alpha = 0.5) +
  scale_y_continuous(label = comma) +
  ggtitle('Ammunition vs Velocity\n') +
  xlab('\n Ammunition') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.text = element_text(color = 'black', face = 'bold', 
                               hjust = 0.5, size = 5),
    legend.background = element_rect(color = 'black'),
    legend.position = "right" # c(0.95, 0.1)
  )
bmp(file="amo_vs_vel_boxplot.bmp",
    width=5, height=4, units="in", res=200)
p
dev.off()

p2 <- ggplot(vel, aes(rfl, vel)) + 
  geom_jitter(width = .15, shape = 21, 
              aes(fill = amo), size = 5) +
  scale_fill_manual(values = c('green', 'orange', 'yellow', 'purple')) + 
  geom_boxplot(fill = 'white', color = 'black', alpha = 0.5) +
  scale_y_continuous(label = comma) +
  ggtitle('Rifle vs Velocity\n') +
  xlab('\n ') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 14),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.text = element_text(color = 'black', face = 'bold', 
                               hjust = 0.5, size = 7),
    legend.background = element_rect(color = 'black'),
    legend.position = "right" #c(0.9, 0.7)
  )
bmp(file="rfl_vs_vel_boxplot.bmp",
    width=5, height=4, units="in", res=200)
p2
dev.off()

p3 <- ggplot(vel, aes(cnt, vel)) + 
  geom_point(shape = 21, 
              aes(fill = amo), size = 5) +
  scale_fill_manual(values = c('green', 'orange', 'yellow', 'purple')) +
  scale_y_continuous(label = comma) +
  ggtitle('Shot Number vs Velocity \n') +
  xlab('\n Shot Number') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 14),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.text = element_text(color = 'black', face = 'bold', 
                               hjust = 0.5, size = 7),
    legend.background = element_rect(color = 'black'),
    legend.position = "right" # c(0.95, 0.6)
  )

bmp(file="sht_num_vs_vel.bmp",
    width=5, height=4, units="in", res=200)
p3
dev.off()

p_qq <- ggplot(vel, aes(sample = vel, fill = amo)) + 
  geom_qq(shape = 21, size = 5, col = 'black') + 
  scale_fill_manual(values = c('green', 'orange', 'yellow', 'purple')) +
  geom_qq_line(size = 1) +
  scale_y_continuous(label = comma) +
  ggtitle('QQ Plot of Velocities by Ammunition \n') +
  xlab('\n Normal Z-score') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 12),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 14),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.text = element_text(color = 'black', face = 'bold', 
                               hjust = 0.5, size = 7),
    legend.background = element_rect(color = 'black'),
    legend.position = "right" #c(0.1, 0.85)
  )

bmp(file="vel_qq_amo.bmp",
    width=5, height=4, units="in", res=200)
p_qq
dev.off()

# shapiro wilk test on each factor
do.call("rbind", 
        with(vel, 
             tapply(vel, 
                    amo,
                    function(x) unlist(shapiro.test(x)[c("statistic", 
                                                         "p.value")]))))

mod <- aov(vel ~ rfl + amo * cnt , data = vel)
# Tukey test HSD
# tukey.test <- TukeyHSD(mod)
# plot(tukey.test)
# marginal <- lsmeans::lsmeans(mod, pairwise ~ amo, adjust = 'tukey')

v <- data.frame(Mean = c(2406,2434, 2570, 2727),
                Variance = c(299.2, 361.7, 726.2, 1546.1),
                Ammo = c('B', 'C', 'A', 'D'))
v$Ammo <- factor(v$Ammo, levels = v$Ammo)

#v_plot <- 
  ggplot(v, aes(x=Mean, y=Variance, fill = Ammo)) + 
  geom_point(color = 'black', shape = 21, size = 6) +
  #geom_smooth(method = 'lm', formula = (y ~ x)) +
  scale_fill_manual(values = c('orange', 'yellow', 'green', 'purple')) + 
  scale_y_continuous(label = comma) +
  scale_x_continuous(label = comma) +
  ggtitle('Mean Velocity vs Variance \n') +
  xlab('\n Mean Velocity (fps)') +
  ylab('Velocity Variance \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 16),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 14),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 14),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank(),
    legend.text = element_text(color = 'black', face = 'bold', 
                               hjust = 0.5, size = 7),
    legend.background = element_rect(color = 'black'),
    legend.position = "right" #c(0.1, 0.85)
  )


