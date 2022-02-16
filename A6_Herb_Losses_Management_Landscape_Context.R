# Johnson, P.P., S. Kobal, W. Leonard, & E.S. Minor. 2022. 
# Herbaceous plant richness increases with surrounding 
# habitat and management burns over 30-years in suburban 
# forest understories. 

########################################################

# Analysis 6, Which best explains native species losses
# over 30-years in the forest understory: whether the 
# plot was burned, the amount of surrounding habitat, the 
# configuration of that habitat, or a combination of these 
# factors?

########################################################

# Loading necessary R packages
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(AICcmodavg)
library(stringr)

################## Analysis code #######################

# Loading the landscape context data
data_land <- read_csv('D4_Habitat_Amount_Configuration.csv')

# Loading the turnover data
data_turn = read_csv("D3_Herb_Turnover.csv")

# Renaming columns in the turnover data
data_turn = data_turn %>% dplyr::rename(HerbGain = Gains, 
                                        HerbLost = Losses,
                                        SiteID = PlotCode, 
                                        Treatment = Management)

# Converting the SiteID column to lower case to match the 
# landscape context data
data_turn = data_turn %>% dplyr::mutate(SiteID = tolower(SiteID))

# Creating a 'Preserve' column to be used later in the models
data_turn = data_turn %>% dplyr::mutate(Preserve = str_sub(SiteID, 1, 4))

# Correcting the 'wypl2' SiteID to 'wypl1' to match the landscape data
data_turn = data_turn %>% 
  dplyr::mutate(SiteID = ifelse(SiteID == "wypl2", "wypl1", SiteID))

# Combining the landscape context and native herb turnover data 
# into a combined data frame, Data
Data = data_turn %>% dplyr::left_join(data_land, by = c("SiteID" = "Site"))

# Changing 'Treatment' to a factor
Data <- mutate(Data, Treatment = as.factor(Treatment),
               Preserve = as.factor(Preserve))

# Releving the factors
Data$Treatment <- relevel(Data$Treatment, ref = "NotBurned")  

# Retaining a version of the unscaled data
UnSData <- Data

# Scaling the landscape predictors
Data[,c(7:13)] <- scale(Data[,c(7:13)], center = TRUE, scale = TRUE)

# Multimodel approach for herb losses, creating the set of candidate 
# models

# Null model
LostMod0 <- glmer(HerbLost ~
                    + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management only
LostMod1 <- glmer(HerbLost ~ Treatment
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat amount at 300 meters
LostMod2 <- glmer(HerbLost ~ Treatment 
                  + CA.300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat amount at 900 meters
LostMod3 <- glmer(HerbLost ~ Treatment 
                  + CA.900
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat amount at 3400 meters
LostMod4 <- glmer(HerbLost ~ Treatment 
                  +CA.3400
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 300 meters
LostMod5 <- glmer(HerbLost ~ Treatment 
                  +CLUMPY.300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 600 meters
LostMod6 <- glmer(HerbLost ~ Treatment 
                  +CLUMPY.600
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 2700 meters
LostMod7 <- glmer(HerbLost ~ Treatment 
                  +CLUMPY.2700
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 3300 meters
LostMod8 <- glmer(HerbLost ~ Treatment 
                  +CLUMPY.3300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management, habitat amount at 300 meters, and
# habitat configuration at 300 meters
LostMod9 <- glmer(HerbLost ~ Treatment
                  + CA.300 
                  + CLUMPY.300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management, habitat amount at 300 meters, and
# habitat configuration at 600 meters
LostMod10 <- glmer(HerbLost ~ Treatment
                   + CA.300 
                   + CLUMPY.600
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 300 meters, and
# habitat configuration at 2700 meters
LostMod11 <- glmer(HerbLost ~ Treatment
                   + CA.300 
                   + CLUMPY.2700
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 300 meters, and
# habitat configuration at 3300 meters
LostMod12 <- glmer(HerbLost ~ Treatment
                   + CA.300 
                   + CLUMPY.3300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 300 meters
LostMod13 <- glmer(HerbLost ~ Treatment
                   + CA.900 
                   + CLUMPY.300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 600 meters
LostMod14 <- glmer(HerbLost ~ Treatment
                   + CA.900 
                   + CLUMPY.600
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 2700 meters
LostMod15 <- glmer(HerbLost ~ Treatment
                   + CA.900 
                   + CLUMPY.2700
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 3300 meters
LostMod16 <- glmer(HerbLost ~ Treatment
                   + CA.900 
                   + CLUMPY.3300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 3400 meters, and
# habitat configuration at 300 meters
LostMod17 <- glmer(HerbLost ~ Treatment
                   + CA.3400 
                   + CLUMPY.300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 3400 meters, and
# habitat configuration at 600 meters
LostMod18 <- glmer(HerbLost ~ Treatment
                   + CA.3400 
                   + CLUMPY.600
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 3400 meters, and
# habitat configuration at 2700 meters
LostMod19 <- glmer(HerbLost ~ Treatment
                   + CA.3400 
                   + CLUMPY.2700
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 3400 meters, and
# habitat configuration at 3300 meters
LostMod20 <- glmer(HerbLost ~ Treatment
                   + CA.3400 
                   + CLUMPY.3300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Creating a list of candidate models
Cand.models <- list('LostMod0' = LostMod0,
                    'LostMod1' = LostMod1,
                    'LostMod2' = LostMod2,
                    'LostMod3' = LostMod3,
                    'LostMod4' = LostMod4,
                    'LostMod5' = LostMod5,
                    'LostMod6' = LostMod6,
                    'LostMod7' = LostMod7,
                    'LostMod8' = LostMod8,
                    'LostMod9' = LostMod9,
                    'LostMod10' = LostMod10,
                    'LostMod11' = LostMod11,
                    'LostMod12' = LostMod12,
                    'LostMod13' = LostMod13,
                    'LostMod14' = LostMod14,
                    'LostMod15' = LostMod15,
                    'LostMod16' = LostMod16,
                    'LostMod17' = LostMod17,
                    'LostMod18' = LostMod18,
                    'LostMod19' = LostMod19,
                    'LostMod20' = LostMod20)

# Checking the model residuals visually

# Creating an int for naming figures in the for loop
int <- 0

# Checking residuals of each model visually using
# a for loop that iterates over the candidate models
# and plots their residual versus all predictors

# For models in the candidate model set
for (mods in Cand.models) {
  # Inputing the model:
  model <- mods
  
  # Calculating the Residuals and fitted residuals
  PerasonResiduals <- residuals(model, type = 'pearson')
  FittedValues <- predict(model, type = 'link')
  
  # Putting these together in a data.frame, DF
  DF <- data.frame(FittedValues, PerasonResiduals)
  
  # Extracting the row names, so I can match these to sites in 
  # the Data and create labels with ggplot
  DF$labels <- row.names(DF)
  
  # Getting the data for the qqplot by first constructing a qqplot with the 
  # built in stat_qq() function with ggplot, but to label the outliers, I 
  # need to replot this using the data.
  qq <- ggplot(DF, aes(sample = PerasonResiduals)) +
    stat_qq()
  
  # Extracts the data
  qq.DF <- ggplot_build(qq)$data[[1]]
  
  # Adding the labels to this data, need to order the data by the DevRsiduals
  # as this is the same order that they are in the qq plot made in ggplot.
  qq.DF$label <- DF$labels[order(DF$PerasonResiduals)]
  
  ## Now to constructing the actual plots.
  
  # The QQ plot, with residuals with sample residuals greater than 3 SD from 0 
  # labelled as outliers.
  QQPlot <- ggplot(qq.DF, aes(x = theoretical, y = sample)) +
    geom_point() +
    geom_text(aes(label=ifelse(sample >= (sd(qq.DF$sample)*3) | sample <= (sd(qq.DF$sample)*-3) ,as.character(label),'')),hjust=-1,vjust=0)+
    labs(title = "Q-Q Plot Residuals", x = "Theoretical", y = "Sample", subtitle = as.character(int)) +
    theme_bw()
  
  # The fitted values versus Residuals. 
  FitDevPlot <- ggplot(DF, aes(x = FittedValues, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus Fit', x = "Fitted Values", y = "Residuals") +
    theme_bw()
  
  # Pulling out the covariates from the Data data.frame to add these to the
  # DF data.frame
  Covariates <- select(Data,
                       Treatment,
                       CA.300, 
                       CA.900,
                       CA.3400,
                       CLUMPY.300,
                       CLUMPY.600,
                       CLUMPY.2700,
                       CLUMPY.3300)
  
  # Adding these to the DF data.frame
  DF <- data.frame(DF, Covariates)
  
  # The treatment versus Residuals. 
  TreatDevPlot <- ggplot(DF, aes(x = Treatment, y = PerasonResiduals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus Treatment', x = "Treatment", y = "Residuals") +
    theme_bw()
  
  # The CA.300 versus Residuals. 
  CA300DevPlot <- ggplot(DF, aes(x = CA.300, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CA.300', x = "CA.300", y = "Residuals") +
    theme_bw()
  
  # The CA.900 versus Residuals. 
  CA900DevPlot <- ggplot(DF, aes(x = CA.900, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CA.900', x = "CA.900", y = "Residuals") +
    theme_bw()
  
  # The CA.3400 versus Residuals. 
  CA3400DevPlot <- ggplot(DF, aes(x = CA.3400, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CA.3400', x = "CA.3400", y = "Residuals") +
    theme_bw()
  
  # The CLUMPY.300 versus Residuals. 
  CL300DevPlot <- ggplot(DF, aes(x = CLUMPY.300, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CLUMPY.300', x = "CLUMPY.300", y = "Residuals") +
    theme_bw()
  
  # The CLUMPY.600 versus Residuals. 
  CL600DevPlot <- ggplot(DF, aes(x = CLUMPY.600, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CLUMPY.600', x = "CLUMPY.600", y = "Residuals") +
    theme_bw()
  
  # The CLUMPY.600 versus Residuals. 
  CL600DevPlot <- ggplot(DF, aes(x = CLUMPY.600, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CLUMPY.600', x = "CLUMPY.600", y = "Residuals") +
    theme_bw()
  
  # The CLUMPY.2700 versus Residuals. 
  CL2700DevPlot <- ggplot(DF, aes(x = CLUMPY.2700, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CLUMPY.2700', x = "CLUMPY.2700", y = "Residuals") +
    theme_bw()
  
  # The CLUMPY.3300 versus Residuals. 
  CL3300DevPlot <- ggplot(DF, aes(x = CLUMPY.3300, y = PerasonResiduals)) +
    geom_point() +
    geom_smooth(col = 'black', se = FALSE) +
    geom_hline(yintercept = 0, linetype = 'longdash') +
    geom_text(aes(label=ifelse(PerasonResiduals >= (sd(DF$PerasonResiduals)*3) | PerasonResiduals <= (sd(DF$PerasonResiduals)*-3) ,as.character(labels),'')),hjust=-1,vjust=0) +
    labs(title = 'Versus CLUMPY.3300', x = "CLUMPY.3300", y = "Residuals") +
    theme_bw()
  
  # Putting these plots together in a grid G
  G <- grid.arrange(QQPlot,
                    FitDevPlot,
                    TreatDevPlot,
                    CA300DevPlot,
                    CA900DevPlot,
                    CA3400DevPlot,
                    CL300DevPlot,
                    CL600DevPlot,
                    CL2700DevPlot,
                    CL3300DevPlot,
                    nrow = 4)
  
  # updating the int
  int <- int + 1
  
  # Display the residual plots in the grid G
  G
}

# Creating a selection table based on AICc
selectionTable <- aictab(cand.set = Cand.models)

# Creating a 95% confidence set of models: 
confset(cand.set = Cand.models)

# Creating a list of the top models in the confidence set:
Top.models <- list(LostMod0,
                   LostMod1,
                   LostMod6,
                   LostMod2,
                   LostMod4,
                   LostMod5,
                   LostMod3,
                   LostMod7,
                   LostMod8,
                   LostMod10,
                   LostMod9,
                   LostMod18,
                   LostMod14,
                   LostMod17,
                   LostMod13,
                   LostMod12,
                   LostMod11,
                   LostMod19)

# Storing the results of the multi model average:
LostResults <- summary(model.avg(Top.models))

# Relative Importance of fixed effects:
LostResults$sw

# Pseudo R squared of top model: 
r.squaredGLMM(LostMod0)

# Natural averages results (no shrinkage):
LostResults$coefmat.subset