library(tidyverse); library(scales)

vel <- read.csv('velocity.csv', header = T)
vel$rfl <- as.factor(vel$rfl)
vel$rfl <- recode(vel$rfl, '1' = 'Rifle 1', '2' = 'Rifle 2')

p <- ggplot(vel, aes(amo, vel)) + 
  geom_jitter(width = .15, shape = 21, 
              aes(fill = rfl), size = 5) +
  geom_boxplot(fill = 'white', color = 'black', alpha = 0.5) +
  scale_y_continuous(label = comma) +
  ggtitle('Ammunition vs Velocity Boxplot \n') +
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
                               hjust = 0.5, size = 12),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.9, 0.1)
  )
print(p)

p2 <- ggplot(vel, aes(rfl, vel)) + 
  geom_jitter(width = .15, shape = 21, 
              aes(fill = amo), size = 5) +
  geom_boxplot(fill = 'white', color = 'black', alpha = 0.5) +
  scale_y_continuous(label = comma) +
  ggtitle('Rifle vs Velocity Boxplot \n') +
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
                               hjust = 0.5, size = 12),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.9, 0.85)
  )
print(p2)

p3 <- ggplot(vel, aes(cnt, vel)) + 
  geom_point(shape = 21, 
              aes(fill = amo), size = 3) +
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
                               hjust = 0.5, size = 12),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.95, 0.6)
  )
print(p3)

p_qq <- ggplot(vel, aes(sample = vel, fill = amo)) + 
  geom_qq(shape = 21, size = 5, col = 'black') + 
  geom_qq_line(size = 1) +
  scale_y_continuous(label = comma) +
  ggtitle('QQ Plot of Velocities \n') +
  xlab('\n Normal Z-score') +
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
                               hjust = 0.5, size = 12),
    legend.background = element_rect(color = 'black'),
    legend.position = c(0.1, 0.85)
  )
print(p_qq)

mod <- aov(vel ~ rfl+amo , data = vel)
# Tukey test HSD
tukey.test <- TukeyHSD(mod)
plot(tukey.test)
marginal <- lsmeans::lsmeans(mod, pairwise ~ amo, adjust = 'tukey')


