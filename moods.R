################################################################################
################################################################################
###################### R Code: A Case for Nonparametrics #######################
######################          Mood's Median            #######################
################################################################################
################################################################################

### Important packages
library(tidyverse)     # Data Tools
library(xtable)        # Create latex tables
library(RVAideMemoire) # For Mood's median test
library(rcompanion)    # For adjusted p-values pairwise comparisons Mood's median
library(pairwiseCI)    # For bootstrapping median intervals 

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

### Mood's median test
mood.medtest(MFAP4 ~ Fibrosis.Stage, data = mfap4.df)

### Pairwise Mood's median hypothesis tests 
### Benjamini-Hochberg adjustment 
(PT <- pairwiseMedianTest(MFAP4 ~ Fibrosis.Stage, data = mfap4.df, method = "BH"))

### LaTex code for table
print(xtable(PT, digits = 4), include.rownames = FALSE)

### Pairwise Mood's median bootstrap confidence intervals 
### Benjamini-Hochberg adjustment
alpha <- 0.05
m <- 10
bh.conf <- 1-4*(alpha/m)
(BH.conf.intervals<- pairwiseCI(MFAP4 ~ Fibrosis.Stage, data = mfap4.df, method = "Median.diff", conf.level = bh.conf, R = 5000))

### Plotting the Benjamini-Hochberg adjusted bootstrap confidence intervals
BH.conf.intervals <- data.frame(BH.conf.intervals) %>%
  mutate(lower = ifelse(PT$p.adjust<0.05, lower, NA),
         upper = ifelse(PT$p.adjust<0.05, upper, NA))
ggdat<-data.frame(BH.conf.intervals)
ggdat$comparison <- gsub(':', '-', ggdat$comparison)
ggplot(data = ggdat,aes(x = comparison, y = estimate)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), position = position_dodge(0.25))+
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, position = position_dodge(0.25))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_bw()+
  xlab("Pairwise Difference")+
  ylab("Estimated Difference in Median MFAP4 values")+
  coord_flip()
