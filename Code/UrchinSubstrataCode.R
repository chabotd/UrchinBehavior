library(tidyverse)
library(dplyr)
library(ggplot2)
library(GGally)
library(titanic)
library(RColorBrewer)


################################################################################
#Set Up data frame 
################################################################################
urchin <- read.csv("Data/WorkingUrchinBehavior.csv")

#rename column(s)
colnames(urchin)[colnames(urchin) == "TimeLeftBrick.min."]=
  "TimeToLeave"

#create mean spine column
urchin <- urchin %>%
  mutate(MeanSpine= rowMeans(urchin[,c("Spine1","Spine2","Spine3","Spine4", "Spine5")]))

###############################################################################
# proporations of urchins by type (percentage of urchins having left the brick)
###############################################################################
#for urchin type (pit/nonpit)
urchin %>%
  group_by(UrchinType) %>%
  summarise(n_of_observations = n(), prop = sum(Left.1.DidNotLeave.0. == 1)/n())
# no diffs

#for substrate 
urchin %>%
  group_by(SubstrateType) %>%
  summarise(n_of_observations = n(), prop = sum(Left.1.DidNotLeave.0. == 1)/n())
#only half left the sand

# control vs kelp
urchin %>%
  group_by(TrialType) %>%
  summarise(n_of_observations = n(), prop = sum(Left.1.DidNotLeave.0. == 1)/n())



###############################################################################
# visualizations
###############################################################################
#substrata
p1<- ggplot(urchin, aes(x=SubstrateType, y=TimeToLeave, fill=SubstrateType)) + 
  geom_boxplot(aes(fill = SubstrateType), show.legend = FALSE)  +
  labs(title="Effect of Substrate on Initial Movement of Urchins",x="Substrate Type", y = "Time to Leave Brick (min)")+
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00"))

ggsave(filename = "Figures/SubstrateTime.png", 
       plot = p1 , width = 8, height = 6, dpi = 300)

#kelp 
p2 <- ggplot(urchin, aes(x=TrialType, y=TimeToLeave, fill=TrialType)) + 
  geom_boxplot(aes(fill = TrialType), show.legend = FALSE) +
  labs(title="Effect of Drift Kelp Presence on Initial Movement of Urchins",x="", y = "Time to Leave Brick (min)")+
  scale_fill_manual(values=c("#999999", "green4"))

ggsave(filename = "Figures/KelpTime.png", 
       plot = p2 , width = 8, height = 6, dpi = 300)

#pit v nonpit
p3 <- ggplot(urchin, aes(x=UrchinType, y=TimeToLeave, fill=UrchinType)) + 
  geom_boxplot(aes(fill = UrchinType), show.legend = FALSE) +
  labs(title="Effect of Pitted Urchins on Initial Movement of Urchins",x="", y = "Time to Leave Brick (min)")+
  scale_fill_manual(values=c("mediumorchid4", "mediumpurple3"))

ggsave(filename = "Figures/UrchinTime.png", 
       plot = p3 , width = 8, height = 6, dpi = 300)


###############################################################################
#Analyses - Time to Leave
###############################################################################
#anova
urchins <- aov(TimeToLeave ~ SubstrateType, data = urchin)
summary(urchins)

#Tukey
TukeyHSD(urchins, conf.level=.95)

#t-tests
t.test(TimeToLeave ~ UrchinType, data = urchin, alternative="greater")
t.test(TimeToLeave ~ TrialType, data = urchin, alternative="greater")

###############################################################################
#Analyses - Sizes and Spines
###############################################################################

#t.test
t.test(MeanSpine ~ UrchinType, data = urchin, alternative="greater")
t.test(UrchinSize ~ UrchinType, data = urchin, alternative = "greater")

#pit v nonpit
p4 <- ggplot(urchin, aes(x=UrchinType, y=MeanSpine, fill=UrchinType)) + 
  geom_boxplot(aes(fill = UrchinType), show.legend = FALSE) +
  labs(title="Spine Lengths of Pitted and Nonpitted Urchins",x="", y = "Mean Spine Length (cm)")+
  scale_fill_manual(values=c("mediumorchid4", "mediumpurple3"))

ggsave(filename = "Figures/UrchinSpines.png", 
       plot = p4 , width = 8, height = 6, dpi = 300)

#pit v nonpit
p5 <- ggplot(urchin, aes(x=UrchinType, y=UrchinSize, fill=UrchinType)) + 
  geom_boxplot(aes(fill = UrchinType), show.legend = FALSE) +
  labs(title="Test Sizes of Pitted and Nonpitted Urchins",x="", y = "Mean Test Size (cm)")+
  scale_fill_manual(values=c("mediumorchid4", "mediumpurple3"))

ggsave(filename = "Figures/UrchinTests.png", 
       plot = p5 , width = 8, height = 6, dpi = 300)