library("dplyr")
library("ggpubr")
library("effsize")

df <- data.frame("sub" = c(1,2,3,4,5), "standard" = c(367.772362962, 568.247914835, 680.181506412, 520.060783303, 519.920311497), "auto" = c(139.242582853, 264.241236468, 224.606199274, 238.021592199, 144.426603395))
df[, -1] <- df[, -1]/60

plot(df$sub, df$auto, pch=15, xlab = "Subjects", ylab = "Time taken (in mins)", xlim=c(0.5,5.5), xaxt="n", ylim=c(0,20))
points(df$sub, df$standard, pch=17)
axis(1, at = 1:5, labels = 1:5)
legend(4.5,20, legend = c('Autocorrect', 'Standard'), pch=c(15,17))
legend(2,20, legend = c(,), pch=c(15,17))

# test for normality 
shapiro.test(df$standard)
shapiro.test(df$auto)

# paired t-test
res_ttest = t.test(df$standard,df$auto, paired = TRUE, alternative = 'two.sided')
res_ttest

# plot paired data
ggpaired(df,'standard','auto')

# Cohen's d test for effect size
cohen.d(df$standard,df$auto)

setwd("/home/manvi/Documents/user models")

erblin <- read.csv('erblin.csv')
manvi <- read.csv('manvi.csv')
rene <- read.csv('rene.csv')
sharbat <- read.csv('sharbat.csv')
tim <- read.csv('tim.csv')

dfList <- list(erblin,manvi,rene,sharbat,tim)

for (i in 1:length(dfList)) #populate existing df with weighted standard and auto
{
  dfList[[i]]$wt_standard <- dfList[[i]]$weight * dfList[[i]]$standard
  dfList[[i]]$wt_auto <- dfList[[i]]$weight * dfList[[i]]$auto
}

men_dem <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard", "wt_auto"))
phys_dem <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard", "wt_auto"))
temp_dem <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard", "wt_auto"))
perf <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard", "wt_auto"))
eff <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard", "wt_auto"))
frust <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard", "wt_auto"))

nasatlxList <- list(men_dem, phys_dem, temp_dem, perf, eff, frust)

for (i in 1:length(nasatlxList)) #new df with each of the 6 factors
{
  for (j in 1:length(dfList))
  {
    nasatlxList[[i]][j,] = c(j, dfList[[j]][i,4], dfList[[j]][i,5])
  }
}

# graphs
plot(nasatlxList[[1]]$sub, nasatlxList[[1]]$wt_auto, pch=15, xlab = "Subjects", ylab = "Weighted Score", xlim=c(0.5,5.5), xaxt="n", ylim = c(15,75))
points(nasatlxList[[1]]$sub, nasatlxList[[1]]$wt_standard, pch=17)
axis(1, at = 1:5, labels = 1:5)

plot(nasatlxList[[5]]$sub, nasatlxList[[5]]$wt_auto, pch=15, xlab = "Subjects", ylab = "Weighted Score", xlim=c(0.5,5.5), xaxt="n", ylim = c(10,70))
points(nasatlxList[[5]]$sub, nasatlxList[[5]]$wt_standard, pch=17)
axis(1, at = 1:5, labels = 1:5)

plot(nasatlxList[[6]]$sub, nasatlxList[[6]]$wt_auto, pch=15, xlab = "Subjects", ylab = "Weighted Score", xlim=c(0.5,5.5), xaxt="n", ylim = c(10,100))
points(nasatlxList[[6]]$sub, nasatlxList[[6]]$wt_standard, pch=17)
axis(1, at = 1:5, labels = 1:5)



# check for normality
for (i in 1:length(nasatlxList))
{
  stan_test = shapiro.test(nasatlxList[[i]]$wt_standard)
  auto_test = shapiro.test(nasatlxList[[i]]$wt_auto)
  print(c(i,stan_test,auto_test,"done"))
}

# paired t-test
for (i in 1:length(nasatlxList))
{
  res_ttest = t.test(nasatlxList[[i]]$wt_standard,nasatlxList[[i]]$wt_auto, paired = TRUE, alternative = 'two.sided')
  print(res_ttest)
}

# cohen's d test
for (i in 1:length(nasatlxList))
{
  res_cohendtest = cohen.d(nasatlxList[[i]]$wt_standard,nasatlxList[[i]]$wt_auto)
  print(res_cohendtest)
}
