################################################################################
################################################################################
###################### R Code: A Case for Nonparametrics #######################
######################         Power Simulations         #######################
################################################################################
################################################################################

library(RVAideMemoire)
library(tidyverse)

#####################################
# Simulation for Normal Populations #
#####################################

sims <- 1000
n <- 100
mus <- seq(0,1.5,by=0.05)

pwr <-data.frame(anova=rep(NA,length(mus)),
                 kw=rep(NA,length(mus)),
                 mood=rep(NA,length(mus)))

for(mui in 1:length(mus)){
  mu <- mus[mui]
  res <- data.frame(anova=rep(NA,sims),
                    kw=rep(NA,sims),
                    mood=rep(NA,sims))
  
  for(i in 1:sims){
    x<-rnorm(n,0,1)
    y<-rnorm(n,mu,1)
    dat <- data.frame(x=c(x,y),
                      g=c(rep("x",n),rep("y",n)))
    
    res$anova[i]<-summary(aov(x~g, data=dat))[[1]][["Pr(>F)"]][1]
    
    res$kw[i]<-kruskal.test(x~g, data=dat)$p.value
    res$mood[i]<-mood.medtest(x~g, data=dat)$p.value
  }
  pwr$anova[mui] <- mean(res$anova<0.05)
  
  pwr$kw[mui] <- mean(res$kw<0.05)
  pwr$mood[mui] <- mean(res$mood<0.05)
}

pwr <- pwr %>% 
  add_column(mu = mus) %>%
  pivot_longer(cols=1:3,
               names_to="Model",
               values_to="Power")%>%
  mutate(Model = case_when(Model=="anova"~"ANOVA",
                           Model=="kw" ~ "Kruskal-Wallis",
                           Model=="mood"~ "Mood's Median"))

ggplot(data=pwr, aes(x=mu, y=Power))+
  geom_point(aes(shape=Model))+
  geom_line(aes(linetype=Model))+
  theme_bw()+
  xlab(bquote(mu))+
  ggtitle(bquote("Power Via Simulation"~(n==.(n))),
          subtitle=bquote("Comparing N(0,1) and N("*mu*",1) Populations"))


##########################################
# Simulation for Exponential Populations #
##########################################

sims <- 100
n <- 100
mus <- seq(1,2.5,by=0.05)

pwr <- data.frame(anova=rep(NA,length(mus)),
                 kw=rep(NA,length(mus)),
                 mood=rep(NA,length(mus)))

for(mui in 1:length(mus)){
  mu <- mus[mui]
  res <- data.frame(anova=rep(NA,sims),
                    kw=rep(NA,sims),
                    mood=rep(NA,sims))
  
  for(i in 1:sims){
    x<-rexp(n,1)
    y<-rexp(n,1/mu)
    dat <- data.frame(x=c(x,y),
                      g=c(rep("x",n),rep("y",n)))
    
    res$anova[i]<-summary(aov(x~g, data=dat))[[1]][["Pr(>F)"]][1]
    
    res$kw[i]<-kruskal.test(x~g, data=dat)$p.value
    res$mood[i]<-mood.medtest(x~g, data=dat)$p.value
  }
  pwr$anova[mui] <- mean(res$anova<0.05)
  
  pwr$kw[mui] <- mean(res$kw<0.05)
  pwr$mood[mui] <- mean(res$mood<0.05)
}

pwr <- pwr %>% 
  add_column(mu = mus) %>%
  pivot_longer(cols=1:3,
               names_to="Model",
               values_to="Power")%>%
  mutate(Model = case_when(Model=="anova"~"ANOVA",
                           Model=="kw" ~ "Kruskal-Wallis",
                           Model=="mood"~ "Mood's Median"))

ggplot(data=pwr, aes(x=mu, y=Power))+
  geom_point(aes(shape=Model))+
  geom_line(aes(linetype=Model))+
  geom_hline(yintercept = 0.05, linetype="dotted", color="grey")+
  theme_bw()+
  xlab(bquote(mu))+
  ggtitle(bquote("Power Via Simulation"~(n=.(n))),
          subtitle=bquote("Comparing Exp(1) and Exp("*1/mu*") Populations"))







