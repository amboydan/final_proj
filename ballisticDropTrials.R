library(ggplot2)

# inputs for the PointBlank ballistics software
# 1) actual, 2) 10% low, 3) 10% high
bc <- c(0.462, 0.416, 0.508)
wt <- c(168, 151, 185)
alt <- c(200, 180, 220)
tmp <- c(30, 27, 33)
vel <- c(2600, 2340, 2860)

vars <- data.frame(bc, wt, alt, tmp, vel)

# all permutations 
input_permutations <- expand.grid(vars)
total_perms <- nrow(input_permutations)

# total is large. sample from df.
n <- 20
#-----DO NOT TOUCH THE SEED-------#
set.seed(254896)
#---------------------------------#
df <- input_permutations[sample(total_perms, n),]

# 500yrd drops associated with the samples
b_drop <- c(-85.82, -83.15, -86.19, -68.19, -85.86,
            -104.92, -111.94, -111.98, -89.14, -68.34,
            -83.51, -68.35, -69.80, -68.17, -111.71,
            -83.19, -104.74, -85.84, -104.58, -89.60)

# add drops to df
df$drop <- b_drop


