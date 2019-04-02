library(ggplot2)
library(reshape)
library(dplyr)

setwd("/home/manvi/Documents/user models")

s1 <- read.csv('erblin.csv')
s1$standardW <- s1$weights * s1$standard * 5
s1$autoW <- s1$weights * s1$auto * 5
s2 <- read.csv('manvi.csv')
s2$standardW <- s2$weights * s2$standard * 5
s2$autoW <- s2$weights * s2$auto * 5
s3 <- read.csv('rene.csv')
s3$standardW <- s3$weights * s3$standard * 5
s3$autoW <- s3$weights * s3$auto * 5
s4 <- read.csv('sharbat.csv')
s4$standardW <- s4$weights * s4$standard * 5
s4$autoW <- s4$weights * s4$auto * 5
s5 <- read.csv('tim.csv')
s5$standardW <- s5$weights * s5$standard * 5
s5$autoW <- s5$weights * s5$auto * 5


subj <- c(1,2,3,4,5)
auto <- c(sum(s1$autoW), sum(s2$autoW), sum(s3$autoW), sum(s4$autoW), sum(s5$autoW))
standard <- c(sum(s1$standardW), sum(s2$standardW), sum(s3$standardW), sum(s4$standardW), sum(s5$standardW))
totals <- data.frame(subj,auto, standard)
totals$auto <- totals$auto/15
totals$standard <- totals$standard/15

totals_long <- melt(totals,
                id.vars = "subj",
                measure.vars = c("standard","auto"),
                variable.name = "Condition")

p <- ggbarplot(totals_long, x = "variable", y = "value", 
               add =  "mean_sd",
               xlab = "Condition",
               ylab="Overall Weighted \n Workload Score",
               ylim = c(0,85),
               fill="#779ECB")
p + scale_y_continuous(expand=c(0,0),breaks = seq(0, 85, by=10), limits=c(0,85)) +
  scale_x_discrete(labels=c("Standard", "Autocorrect")) +
  geom_point(show.legend=FALSE,aes(alpha=0.65), size=7, shape=18) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

# flipped graph

p <- ggbarplot(totals_long, x = "variable", y = "value", 
               add =  "mean_sd",
               xlab = "Condition",
               ylab="Task Load Index",
               ylim = c(0,85),
               fill=c("#EFAAC4", "#5CB8A5"))
p + scale_y_continuous(expand=c(0,0),breaks = seq(0, 85, by=10), limits=c(0,85)) +
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



addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

men <- bind_rows(s1[1,],s2[1,],s3[1,],s4[1,],s5[1,])
men$subj = c(1,2,3,4,5)

men_long <- melt(men,
                 id.vars = "subj",
                 measure.vars = c("standardW","autoW"),
                 variable.name = "condition")
men_long$subscale <- 1

phy <- bind_rows(s1[2,],s2[2,],s3[2,],s4[2,],s5[2,])
phy$subj = c(1,2,3,4,5)

phy_long <- melt(phy,
                 id.vars = "subj",
                 measure.vars = c("standardW","autoW"),
                 variable.name = "condition")
phy_long$subscale <- 2

temp <- bind_rows(s1[3,],s2[3,],s3[3,],s4[3,],s5[3,])
temp$subj = c(1,2,3,4,5)
temp_long <- melt(temp,
                 id.vars = "subj",
                 measure.vars = c("standardW","autoW"),
                 variable.name = "condition")
temp_long$subscale <- 3

perf <-bind_rows(s1[4,],s2[4,],s3[4,],s4[4,],s5[4,])
perf$subj = c(1,2,3,4,5)
perf_long <- melt(perf,
                 id.vars = "subj",
                 measure.vars = c("standardW","autoW"),
                 variable.name = "condition")
perf_long$subscale <- 4

effort <-bind_rows(s1[5,],s2[5,],s3[5,],s4[5,],s5[5,])
effort$subj = c(1,2,3,4,5)
effort_long <- melt(effort,
                 id.vars = "subj",
                 measure.vars = c("standardW","autoW"),
                 variable.name = "condition")
effort_long$subscale <- 5

frust <-bind_rows(s1[6,],s2[6,],s3[6,],s4[6,],s5[6,])
frust$subj = c(1,2,3,4,5)
frust_long <- melt(frust,
                 id.vars = "subj",
                 measure.vars = c("standardW","autoW"),
                 variable.name = "condition")
frust_long$subscale <- 6

subscales <- rbind(men_long,phy_long,temp_long,perf_long,effort_long,frust_long)

#subs2$subscale <- factor(subs2$subscale)
summarySubs <- summarySE(subscales, measurevar = "value", groupvars = c("variable", "subscale"))
summarySubs$Condition <- c(rep("Standard", each=6), rep("Autocomplete", each=6))
#summarySubs$subscale <- factor(summarySubs$subscale)

summarySubs1 <- subset(summarySubs, subscale<4)
summarySubs1$subscale <- factor(summarySubs1$subscale)
summarySubs2 <- subset(summarySubs, subscale>3)
summarySubs2$subscale <- factor(summarySubs2$subscale)

# Error bars represent standard error of the mean
ggplot(summarySubs, aes(x=subscale, y=value, fill=Variable)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,  # Width of the error bars
                position=position_dodge(.9)) +
  theme(axis.text=element_text(size=20),
        axis.text.x = element_text(angle=90,hjust=1,vjust=0.5),
        axis.title=element_text(size=25,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels=c("Mental Demand", "Physical Demand", "Temporal Demand", "Performance","Effort","Frustration"))

# Error bars represent standard error of the mean
ggplot(summarySubs1, aes(x=subscale, y=value, fill=Condition)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,  # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Subscale", y="Weighted Score") +
  theme(axis.text=element_text(size=20),
        axis.text.x = element_text(hjust=0.5,vjust=0.5),
        axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = c(0.8,0.85),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels=addline_format(c("Mental Demand", "Physical Demand", "Temporal Demand")))

# Error bars represent standard error of the mean
ggplot(summarySubs2, aes(x=subscale, y=value, fill=Condition)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,  # Width of the error bars
                position=position_dodge(.9)) +
  labs(x="Subscale", y="Weighted Score") +
  theme(axis.text=element_text(size=20),
        axis.text.x = element_text(hjust=0.5,vjust=0.5),
        axis.title=element_text(size=25,face="bold"),
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        legend.position = c(0.22,0.85),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) +
  scale_x_discrete(labels=c("Performance","Effort","Frustration"))

sb1 <- subscales[1:30,]
sb2 <- subscales[31:60,]

subscales$Condition <- ifelse(subscales$variable == "standardW", "Standard", "Autocomplete")

sbc <- summarySE(subscales, measurevar = "value", groupvars = c("Condition", "subscale"))

auto_df <- subscales[subscales$Condition == "Autocomplete", ]
stan_df <- subscales[subscales$Condition == "Standard", ]

ggplot(sbc, aes(x=factor(subscale), y=value, fill=Condition)) + 
  geom_bar(position=position_dodge(-0.9), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  scale_fill_manual(values = c("#5CB8A5","#EFAAC4")) +
  scale_x_discrete(labels = c("Mental\nDemand", "Physical\nDemand", "Temporal\nDemand","Performance","Effort","Frustration")) +
  scale_y_continuous(expand=c(0,0),breaks = seq(0, 575, by=50), limits = c(0,575)) +
  geom_errorbar(aes(ymin=ifelse(value-sd<0,0,value-sd), ymax=value+sd),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(-0.9),
                show.legend = FALSE) +
  geom_point(data=auto_df, position = position_nudge(x=-0.22), size=5, alpha=0.6, shape=18, show.legend = FALSE) +
  geom_point(data=stan_df, position = position_nudge(x=0.22), size=5, alpha=0.6, shape=18, show.legend = FALSE) +
  coord_flip() +
  theme(legend.position = c(0.82,0.3),
        legend.title = element_text(size=20,face="bold"),
        legend.text = element_text(size=16),
        axis.title=element_text(size=25,face="bold"),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "#EEE9E9", colour = "#EEE9E9",size = 0.5, linetype = "solid"),
        panel.grid.major.x = element_line(size=1, linetype = "solid", colour="white"),
        panel.grid.minor.x = element_line(size=0.5, linetype = "solid", colour="white"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text=element_text(size=20),axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(angle=-90,hjust = 0.5,vjust = 0.5)) +
  labs(x="Subscale", y="Weighted Score")
