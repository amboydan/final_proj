library(tidyverse); library(scales)

vel <- read.csv('velocity.csv', header = T)
vel$rfl <- as.factor(vel$rfl)
vel$rfl <- recode(vel$rfl, '1' = 'Rifle 1', '2' = 'Rifle 2')

p <- ggplot(vel, aes(amo, vel)) + 
  geom_jitter(width = .2, shape = 21, 
              aes(fill = rfl), size = 3) +
  geom_boxplot(fill = 'white', color = 'black', alpha = 0.5) +
  scale_y_continuous(label = comma) +
  ggtitle('Ammunition vs Velocity Boxplot \n') +
  xlab('\n Ammunition') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 12),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 10),
    axis.text = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 10),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in')
  )
print(p)

p2 <- ggplot(vel, aes(rfl, vel)) + 
  geom_jitter(width = .2, shape = 21, 
              aes(fill = amo), size = 3) +
  geom_boxplot(fill = 'white', color = 'black', alpha = 0.5) +
  scale_y_continuous(label = comma) +
  ggtitle('Rifle vs Velocity Boxplot \n') +
  xlab('\n ') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 12),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 10),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 10),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in'),
    legend.title = element_blank()
  )
print(p2)

p3 <- ggplot(vel, aes(cnt, vel)) + 
  geom_point(shape = 21, 
              aes(fill = amo), size = 3) +
  scale_y_continuous(label = comma) +
  ggtitle('Shot Count vs Velocity \n') +
  xlab('\n Shot Count') +
  ylab('Velocity (fps) \n') +
  theme_bw() +
  theme(
    plot.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 12),
    axis.title = element_text(color = 'black', face = 'bold', 
                              hjust = 0.5, size = 10),
    axis.text = element_text(color = 'black', face = 'bold', 
                             hjust = 0.5, size = 10),
    plot.margin = margin(c(.2,.2,.2,.2), unit = 'in')
  )
print(p3)

mod <- aov(vel ~ rfl+amo , data = vel)
# Tukey test HSD
tukey.test <- TukeyHSD(mod)
plot(tukey.test)
marginal <- lsmeans::lsmeans(mod, pairwise ~ rfl+amo, adjust = 'tukey')


