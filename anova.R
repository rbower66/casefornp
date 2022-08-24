################################################################################
################################################################################
###################### R Code: A Case for Nonparametrics #######################
######################          One-way ANOVA            #######################
################################################################################
################################################################################

### Important packages
library(tidyverse)     # Data Tools
library(xtable)        # Create latex tables

### Load the data
### Tip: Use the "read_csv()" function from tidyverse
mfap4.df <- read_csv("mfap4.csv")

### Rename variables
mfap4.df <- mfap4.df %>%
  rename(PID=`Patient ID`,
         YOB=`Year of Birth`,
         DOS=`Date of sampling`,
         Fibrosis.Stage=`Fibrosis Stage`,
         HCV=`HCV Genotype`,
         MFAP4=`MFAP4 U/mL`)

### Recode the fibrosis variable for more informative labels
### Force R to read it as a factor
mfap4.df <- mfap4.df %>% 
  mutate(Fibrosis.Stage =  case_when(Fibrosis.Stage == 0 ~ "Stage 0",
                                     Fibrosis.Stage == 1 ~ "Stage 1",
                                     Fibrosis.Stage == 2 ~ "Stage 2",
                                     Fibrosis.Stage == 3 ~ "Stage 3",
                                     TRUE ~ "Stage 4")) %>% 
  mutate(Fibrosis.Stage = as.factor(Fibrosis.Stage))

### Creating the new log_2 transformed MFAP4 variable
mfap4.df <- mfap4.df %>% mutate(log2MFAP4 = log(mfap4.df$MFAP4, 2))

### One-way ANOVA 
MFAP4.log.anova <- aov(log2MFAP4 ~ Fibrosis.Stage, data = mfap4.df)
summary(MFAP4.log.anova)

### LaTeX code for table
xtable(summary(MFAP4.log.anova))

### Tukey-adjusted p-values and confidence intervals 
(tukey.intervals <- TukeyHSD(MFAP4.log.anova, conf.level = 0.95)$Fibrosis.Stage)

### LaTeX code for table
xtable(tukey.intervals, digits = 4)

### Plotting the Tukey-adjusted confidence intervals
ggdat <- data.frame(tukey.intervals) %>% mutate(comparison = row.names(tukey.intervals))
ggdat <- ggdat %>% mutate(lwr = ifelse(ggdat$p.adj<0.05, lwr, lwr),
                          upr = ifelse(ggdat$p.adj<0.05, upr, upr))
ggdat$comparison <- gsub(':', ' - ', ggdat$comparison)
ggplot(data = ggdat,aes(x = comparison,y=diff)) +
  geom_pointrange(aes(ymin = lwr, ymax = upr), position = position_dodge(0.25))+
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, position = position_dodge(0.25))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()+
  xlab("Pairwise Difference")+
  labs(y = expression("Estimated Difference in "*log[2]*" mean MFAP4 Values")) +
  coord_flip()
