################################################################################
################################################################################
###################### R Code: A Case for Nonparametrics #######################
######################      Exploratory Data Analysis    #######################      
################################################################################
################################################################################
change
### Important packages
library(tidyverse)     # Data Tools
library(lubridate)     # Dealing with Dates
library(patchwork)     # Plot placement

### Load the data
### Tip: Use the "read_csv()" function from tidyverse
mfap4.df <- read_csv("mfap4.csv")

### Data dimension and data structure
mfap4.df %>% dim()
mfap4.df %>% str()

### View the first and last six rows
mfap4.df %>% head()
mfap4.df %>% tail()

### Rename variables
mfap4.df <- mfap4.df %>%
  rename(PID=`Patient ID`,
         YOB=`Year of Birth`,
         DOS=`Date of sampling`,
         Fibrosis.Stage=`Fibrosis Stage`,
         HCV=`HCV Genotype`,
         MFAP4=`MFAP4 U/mL`)

### Creating the new age variable
dos <- mdy(mfap4.df$DOS)       # Gives "year - month - day" format
dos.year <- year(dos)          # Extracts just the year
age <- dos.year - mfap4.df$YOB # Creates the age variable
mfap4.df <- mfap4.df %>% mutate(Age = age) # Attach "Age" to the data frame

### Check to make sure Age is attached
mfap4.df %>% head()

### Creating the new log_2 transformed MFAP4 variable
mfap4.df <- mfap4.df %>% mutate(log2MFAP4 = log(mfap4.df$MFAP4, 2))

### Check to make sure log2MFAP4 is attached
mfap4.df %>% head()

### Recode the fibrosis variable for more informative labels
### Force R to read it as a factor
mfap4.df <- mfap4.df %>% 
  mutate(Fibrosis.Stage =  case_when(Fibrosis.Stage == 0 ~ "Stage 0",
                                     Fibrosis.Stage == 1 ~ "Stage 1",
                                     Fibrosis.Stage == 2 ~ "Stage 2",
                                     Fibrosis.Stage == 3 ~ "Stage 3",
                                     TRUE ~ "Stage 4")) %>% 
  mutate(Fibrosis.Stage = as.factor(Fibrosis.Stage))

### Check to make sure the fibrosis variable is recoded
mfap4.df %>% head()

### Export a new and clean csv file
write.csv(x = mfap4.df, file = "cleaned-data.csv")

################################################################################
############################## Graphical Displays ############################## 
################################################################################

### Violin boxplot of MFAP4 values by stage
ggplot(data = mfap4.df, aes(x = Fibrosis.Stage, y = MFAP4)) +
  geom_violin(fill = "lightgray",
              trim = FALSE, 
              alpha = 0.5) +
  geom_boxplot(width = 0.25) +
  theme_bw() + 
  ylab("MFAP4 Values") +
  xlab("")

### Normal q-q plot of MFAP4 values by stage
ggplot(data = mfap4.df, aes(sample = MFAP4)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  facet_wrap(~Fibrosis.Stage, ncol = 5) +
  theme(panel.spacing = unit(0.5, "cm"))

### Violin boxplot of log2 MFAP4 values by stage
(violin.plot.log2 <- ggplot(data = mfap4.df, aes(x = Fibrosis.Stage, y = log2MFAP4)) +
  geom_violin(fill="lightgray",
              trim = FALSE, 
              alpha = 0.5) +
  geom_boxplot(width = 0.25) +
  theme_bw() + 
  labs(y = expression(Log[2]*" MFAP4 Values")) +
  xlab(""))

### Normal q-q plot of log2 MFAP4 values by stage
(qqplot.log2 <- ggplot(data = mfap4.df, aes(sample = log2MFAP4)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  facet_wrap(~Fibrosis.Stage, ncol = 5) +
  theme(panel.spacing = unit(0.5, "cm")))

### Side-by-side plots
violin.plot.log2/qqplot.log2




