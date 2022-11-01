################################################################################
################################################################################
###################### R Code: A Case for Nonparametrics #######################
######################         Kruskal-Wallis            #######################
################################################################################
################################################################################

### Important packages
library(tidyverse)     # Data Tools
library(xtable)        # Create latex tables
library(rstatix)       # Dunn's test

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

### Kruskal-Wallis test
kruskal.test(MFAP4 ~ Fibrosis.Stage, data = mfap4.df)

### Dunn's test
### Multiple hypothesis tests 
### Benjamini-Hochberg adjustment 
dunn_test(MFAP4 ~ Fibrosis.Stage, data=mfap4.df, p.adjust.method = "BH")

### Kruskal-Wallis test statistic "by hand"
### Pairwise intervals
### Benjamini-Hochberg adjustment 
mfap4.df <- mfap4.df %>%
  mutate(r = rank(MFAP4))
overall.mean.rank <- mean(mfap4.df$r)
N <- nrow(mfap4.df)
(h.obs<-mfap4.df %>% group_by(Fibrosis.Stage) %>%
    dplyr::summarize(mean.rank = mean(r),
              n = n()) %>%
    dplyr::summarize(h = (h.obs<-(N-1)*sum(n*(mean.rank-overall.mean.rank)^2)/
                     sum((mfap4.df$r - overall.mean.rank)^2))) %>%
    as.numeric())
t<-5

n<-nrow(mfap4.df)
tab<-table(mfap4.df$r)
group.stat <- mfap4.df %>% group_by(Fibrosis.Stage) %>%
  dplyr::summarize(mean.rank = mean(r),
            n = n())

results <- dunn_test(MFAP4 ~ Fibrosis.Stage, data = mfap4.df, p.adjust.method = "BH")
alpha <- 0.05
m <- 10  
R <- length(which(results$p.adj < alpha)) 
bh.conf <- 1-R*(alpha/m)

results$estimate <- rep(NA, nrow(results))
results$lwr <- rep(NA, nrow(results))
results$upr <- rep(NA, nrow(results))
for(i in 1:nrow(results)){
  # Subset data
  dunn.dat <- mfap4.df %>%
    filter(Fibrosis.Stage %in% c(results$group1[i],results$group2[i])) %>%
    droplevels()
  # Run Confidence Interval
  group.stat <- dunn.dat %>% group_by(Fibrosis.Stage) %>%
    dplyr::summarize(mean.rank = mean(r),
              n = n())
  if(results$p.adj[i]<alpha){
    s <- sqrt((n*(n+1)/12 - sum((tab^3-tab)/(12*(n-1)))) * (1/group.stat$n[1] + 1/group.stat$n[2]))
    results$lwr[i] <- -((group.stat$mean.rank[1] - group.stat$mean.rank[2]) + qnorm(bh.conf)*s)
    results$upr[i] <- -((group.stat$mean.rank[1] - group.stat$mean.rank[2]) - qnorm(bh.conf)*s)
  }
  results$diff[i] <- paste(c(results$group2[i], results$group1[i]), collapse=":")
  results$estimate[i] <- -(group.stat$mean.rank[1] - group.stat$mean.rank[2])
}
results$diff <- gsub(':', '-', results$diff)
results

### Plotting the Benjamini-Hochberg adjusted pairwise intervals
ggplot(data = results, aes(x = diff, y = estimate))+
  geom_pointrange(aes(ymin = lwr, ymax = upr))+
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2)+
  geom_hline(yintercept = 0, linetype ="dashed")+
  theme_bw()+
  xlab("Pairwise Difference")+
  ylab("Estimated Difference in Mean Ranks")+
  coord_flip()
