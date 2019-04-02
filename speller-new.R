library(dplyr)
library(ggpubr)
library(effsize)
library(reshape)
library(Rmisc)
library(ggpubr)

# response times for each subject
df <- data.frame("sub" = c(0,1,2,3,4), "standard" = c(367.772362962, 568.247914835, 680.181506412, 520.060783303, 519.920311497), "auto" = c(139.242582853, 264.241236468, 224.606199274, 238.021592199, 144.426603395))
df[, -1] <- df[, -1]/60

# summary statistics for mean response time
mean(df$auto)
sd(df$auto)
mean(df$standard)
sd(df$standard)

# t-test for response time
t.test(df$standard, df$auto, paired=TRUE)


# information transfer rate
itr <- df
itr$standard <- (log(29,2)/itr$standard)*12
itr$auto <- (log(29,2)/itr$auto)*12

# summary statistics for ITR
mean(itr$auto)
sd(itr$auto)
mean(itr$standard)
sd(itr$standard)

# t-test for ITR
t.test(itr$standard, itr$auto, paired=TRUE)



plot(df$sub, df$auto, pch=15, xlab = "Subjects", ylab = "Time taken (in mins)", xlim=c(-0.5,4.5), xaxt="n", ylim=c(0,20))
points(df$sub, df$standard, pch=17)
axis(1, at = 0:4, labels = 0:4)
legend(1.7,20, legend = c('Autocorrect', 'Standard'), pch=c(15,17))
legend(2,20, legend = c(,), pch=c(15,17))

df_long <- melt(df,
                 id.vars = "sub",
                 measure.vars = c("standard","auto"),
                 variable.name = "condition")

itr_long <- melt(itr,
                id.vars = "sub",
                measure.vars = c("standard","auto"),
                variable.name = "condition")

p <- ggbarplot(df_long, x = "variable", y = "value", 
          add =  "mean_sd",
          xlab = "Condition",
          ylab="Time (in mins)",
          ylim = c(0,12),
          fill=c("#EFAAC4", "#5CB8A5"))
p + scale_y_continuous(expand = c(0,0), breaks = seq(0, 12, by=1), limits=c(0,12)) +
  scale_x_discrete(labels=c("Standard", "Autocorrect")) +
  geom_point(show.legend=FALSE,aes(alpha=0.65), size=7, shape=18) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

p <- ggbarplot(itr_long, x = "variable", y = "value", 
               add =  "mean_sd",
               xlab = "Condition",
               ylab="ITR (in bits per min)",
               ylim = c(0,26),
               fill=c("#EFAAC4", "#5CB8A5"))
p + scale_y_continuous(expand=c(0,0),breaks = seq(0, 26, by=2), limits=c(0,26)) +
  scale_x_discrete(labels=c("Standard", "Autocorrect")) +
  geom_point(show.legend=FALSE,aes(alpha=0.65), size=7, shape=18) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

#flipped graphs 

p <- ggbarplot(df_long, x = "variable", y = "value", 
               add =  "mean_sd",
               xlab = "Condition",
               ylab="Time (in mins)",
               ylim = c(0,12),
               fill=c("#EFAAC4", "#5CB8A5"))
p + scale_y_continuous(expand = c(0,0), breaks = seq(0, 13, by=2), limits=c(0,13)) +
  scale_x_discrete(labels=c("Standard", "Auto-\ncomplete")) +
  geom_point(show.legend=FALSE,aes(alpha=0.65), size=7, shape=18) +
  theme(axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#EEE9E9", colour = "#EEE9E9",size = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(size=1, linetype = "solid", colour="white"),
        panel.grid.minor.x = element_line(size=0.5, linetype = "solid", colour="white"),
        axis.text.y = element_text(angle=-90, hjust = 0.5, vjust=0.5,),
        axis.title=element_text(size=25,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  coord_flip()

p <- ggbarplot(itr_long, x = "variable", y = "value", 
               add =  "mean_sd",
               xlab = "Condition",
               ylab="ITR (in bits per min)",
               ylim = c(0,26),
               fill=c("#EFAAC4", "#5CB8A5"))
p + scale_y_continuous(expand=c(0,0),breaks = seq(0, 28, by=3), limits=c(0,28)) +
  scale_x_discrete(labels=c("Standard", "Auto-\ncomplete")) +
  geom_point(show.legend=FALSE,aes(alpha=0.65), size=7, shape=18) +
  theme(axis.text=element_text(size=20),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#EEE9E9", colour = "#EEE9E9",size = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(size=1, linetype = "solid", colour="white"),
        panel.grid.minor.x = element_line(size=0.5, linetype = "solid", colour="white"),
        axis.text.y = element_text(angle=-90, hjust = 0.5, vjust=0.5,),
        axis.title=element_text(size=25,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  coord_flip()



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

weighted_nasatlx <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard_total", "wt_auto_total"))

# combined nasatlx
weighted_nasatlx <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("sub", "wt_standard_total", "wt_auto_total"))

for (i in 1:5)
{
  weight_stand = 0
  weight_auto = 0
  for (j in 1:length(nasatlxList))
  {
    weight_stand = weight_stand + nasatlxList[[j]][i,2]
    weight_auto = weight_auto + nasatlxList[[j]][i,3]
  }
  weighted_nasatlx[i,] = c(i, weight_stand/15, weight_auto/15)
}

plot(weighted_nasatlx$sub, weighted_nasatlx$wt_auto_total, pch=15, xlab = "Subjects", ylab = "NASA-TLX Score", xlim=c(0.5,5.5), xaxt="n", ylim = c(5,20))
points(weighted_nasatlx$sub, weighted_nasatlx$wt_standard_total, pch=17)
axis(1, at = 1:5, labels = 1:5)


shapiro.test(weighted_nasatlx$wt_standard_total)
shapiro.test(weighted_nasatlx$wt_auto_total)

t.test(weighted_nasatlx$wt_standard_total,weighted_nasatlx$wt_auto_total,paired=TRUE)

wilcox.test(weighted_nasatlx$wt_standard_total,weighted_nasatlx$wt_auto_total)