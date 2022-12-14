# A Case for Nonparametrics
The following page provides the dataset (mfap4.csv) and .R files for the code corresponding to the manuscript, "A Case for Nonparametrics," an article currently under review. We have divided the code into four different .R files. The eda.R file enumerates preparing the data and a brief exploratory data analysis. The anova.R file details the code for the one-way ANOVA procedure, Tukey post-hoc analysis, and accompanying graphical display of the Tukey-adjusted confidence intervals. The moods.R file details the code for Mood's median test, pairwise Mood's median tests using a Benjamini-Hochberg correction, bootstrap pairwise intervals with a Benjamini-Hochberg correction, and accompanying graphical display of the adjusted intervals. The kw.R file details the code for the Kruskal-Wallis test (using the appropriate base R function and coding it "by hand"), Dunn's pairwise hypothesis tests using a Benjamini-Hochberg correction, pairwise intervals with a Benjamini-Hochberg correction, and accompanying graphical display of the adjusted intervals. An additional file, powersim.R, outlines power simulations used to compare ANOVA, Mood's median, and Kruskal-Wallis.

You can find point-and-click R shiny applications for conducting the analyses with detailed tutorials via the Data Science Collaboratory:

ANOVA: [https://shiny.colgate.edu/apps/Collaboratory-Apps/ANOVA-Test/](https://shiny.colgate.edu/apps/Collaboratory-Apps/ANOVA-Test/)

Mood's Median: [https://shiny.colgate.edu/apps/Collaboratory-Apps/MoodsMedian-Test/](https://shiny.colgate.edu/apps/Collaboratory-Apps/MoodsMedian-Test/)

Kruskal-Wallis: [https://shiny.colgate.edu/apps/Collaboratory-Apps/KruskalWallis-Test/](https://shiny.colgate.edu/apps/Collaboratory-Apps/KruskalWallis-Test/)
