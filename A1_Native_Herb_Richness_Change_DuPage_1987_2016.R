# Johnson, P.P., S. Kobal, W. Leonard, & E.S. Minor. 2022. 
# Herbaceous plant richness increases with surrounding 
# habitat and management burns over 30-years in suburban 
# forest understories. 

########################################################

# Analysis 1, Has the richness of native forest herbs 
# changed over time, and do these changes depend on whether 
# or not a plot was ever burned?

########################################################

# Loading necessary R packages

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lme4)
library(lmerTest)
library(MuMIn)
library(ggplot2)

################## Analysis code #######################

# Loading in the native herb data
HerbData = read_csv("D1_Anon_Native_Herbs_DuPage_1987_2016.csv")

# Finding the native herb richness at each site in each year: 
RichnessData = HerbData %>% 
  group_by(PlotCode, Year) %>% 
  summarise(Richness = length(Anon_ID),
            Management = unique(Management))

# Adding the Preserve column: 
RichnessData = RichnessData %>%
  mutate(Preserve = str_sub(PlotCode, end = 4))

# Creating the time column: 
RichnessData = RichnessData %>% 
  mutate(Time = Year - 1987)

# Converting PlotCode, Preserve, and Management into factors:
RichnessData = RichnessData %>% 
  mutate(PlotCode = factor(PlotCode),
         Management = factor(Management, levels = c('NotBurned', 'Burned')),
         Preserve = factor(Preserve))

# Visualizing the distribution of the data: 
ggplot(RichnessData, aes(x = Richness)) +
  geom_histogram()

# Constructing the model: 
Richness.Mod <- glmer(Richness ~ Time +
                        Management +
                        Time:Management +
                        (1|Year) +
                        (1|Preserve/PlotCode), 
                      family = poisson(link = "log"),
                      data = RichnessData)

# Inspecting the residuals:
plot(resid(Richness.Mod, type = 'pearson')) # Looks good.
plot(RichnessData$Time, resid(Richness.Mod, type = 'pearson')) # Looks okay. 
plot(RichnessData$Management, resid(Richness.Mod, type = 'pearson')) # Looks okay.

# Calling the model summary: 
summary(Richness.Mod)

# Calculating the pseudo R squared: 
r.squaredGLMM(Richness.Mod)

################## Figure code #########################

# This code is adapted from Zuur and Ieno (2017):

# Zuur, A.F., E.N. Ieno (2017). Data from: A protocol for 
# conducting and presenting regression type analyses. Drayad. 
# Dataset. https://doi.org/10.5061/dryad.v4t42

# Establishing input varialbes for the model:
Time <- rep(seq(0, 29), 2)
Treatment <- c(rep("Treatment", 30), rep("NotBurned", 30))
Treatment <- as.factor(Treatment)
MyData <- data.frame(Time, Treatment)

# Creating a model matrix:
X <- model.matrix(~ Time + 
                    Treatment +
                    Time : Treatment, 
                  data = MyData)

## Modelling the estimates:
MyData$eta <- X %*% fixef(Richness.Mod)
MyData$mu  <- exp(MyData$eta)

## Calcuating the standard error and 95% Confidence interval
MyData$SE   <- sqrt(  diag(X %*%vcov(Richness.Mod) %*% t(X))  )
MyData$seup <- exp(MyData$eta + 1.96 * MyData$SE)
MyData$selo <- exp(MyData$eta - 1.96 * MyData$SE)

# Setting up some data for the visualization:
Years <- seq(1987, 2016, length.out = 30)
Time <- seq(0, 29, length.out = 30)
Years <- seq(1987, 2016, length.out = 30)

TimeYears <- data.frame(Time, Years)

# Joining MyData with the PeriodYears
MyData <- dplyr::left_join(MyData, TimeYears, by = c("Time" = "Time"))

# Changing "Treatment" to "Burned" in MyData
MyData <- dplyr::mutate(MyData, Treatment = ifelse(Treatment == "Treatment", "Burned", "Not Burned"))

# Changing Treatment to a factor: 
MyData <- mutate(MyData, Treatment = as.factor(Treatment),
                 Preserve = as.factor(Preserve))

# Making some new figures:
MyData = MyData %>% dplyr::mutate(Treatment = ifelse(Treatment == 'Burned', 'Burned', 'Not Burned'))

# Making sure that the RichnessData will work for the visualization: 
RichnessData = RichnessData %>% dplyr::rename(Treatment = Management)

RichnessData = dplyr::mutate(RichnessData, Treatment = ifelse(Treatment == "Burned", "Burned", "Not Burned"))

# Manually setting up the breaks
breaks <- c(1989, 1994, 1999, 2004, 2009, 2014)

# Constructing the figure:
Fig <- ggplot()

Fig <- Fig + xlab("Year") + ylab("Native Herb Richness")

Fig <- Fig + geom_ribbon(data = MyData,
                         aes(x = Years, 
                             ymax = seup, 
                             ymin = selo,
                             fill = Treatment),
                         alpha = 0.5)

Fig <- Fig + geom_line(data = MyData, 
                       aes(x = Years, 
                           y = mu, 
                           col = Treatment),
                       size = 1)

Fig <- Fig + geom_point(data = RichnessData, 
                        aes(x = Year, y = Richness, col = Treatment),
                        position = position_jitter(width = .01),
                        alpha = 0.4,
                        size = 1)

Fig <- Fig + theme_bw()

Fig <- Fig + scale_fill_manual(values = c("#ef8a62", "#67a9cf"))

Fig <- Fig + scale_color_manual(values = c("#ef8a62", "#67a9cf"))

Fig <- Fig + scale_x_continuous(name = "Year", breaks)

Fig <- Fig + labs(fill = "Management", color = "Management")

# Inspecting the final figure 
Fig
