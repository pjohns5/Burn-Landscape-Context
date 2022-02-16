# Johnson, P.P., S. Kobal, W. Leonard, & E.S. Minor. 2022. 
# Herbaceous plant richness increases with surrounding 
# habitat and management burns over 30-years in suburban 
# forest understories. 

########################################################

# Code to construct figure 4, 5, & 6 in the paper.

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

########################################################

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


################## Figure 4 #######################

# Models that were in the top model set for Richness: 

# Management only
RichMod1 <- glmer(Richness ~ Treatment
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat amount at 900 meters
RichMod3 <- glmer(Richness ~ Treatment 
                  + CA.900
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 300 meters
RichMod13 <- glmer(Richness ~ Treatment
                   + CA.900 
                   + CLUMPY.300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 600 meters
RichMod14 <- glmer(Richness ~ Treatment
                   + CA.900 
                   + CLUMPY.600
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 2700 meters
RichMod15 <- glmer(Richness ~ Treatment
                   + CA.900 
                   + CLUMPY.2700
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 3300 meters
RichMod16 <- glmer(Richness ~ Treatment
                   + CA.900 
                   + CLUMPY.3300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Creating a list of the top models in the confidence set:
Top.models <- list(RichMod3,
                   RichMod15,
                   RichMod13,
                   RichMod16,
                   RichMod14,
                   RichMod1)

# Storing the results of the multi model average:
RichResults <- summary(model.avg(Top.models))

# Models that were in the top model set for herb Gains:

# Management only
GainMod1 <- glmer(HerbGain ~ Treatment
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat amount at 900 meters
GainMod3 <- glmer(HerbGain ~ Treatment 
                  + CA.900
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat amount at 3400 meters
GainMod4 <- glmer(HerbGain ~ Treatment 
                  +CA.3400
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 300 meters
GainMod5 <- glmer(HerbGain ~ Treatment 
                  +CLUMPY.300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 600 meters
GainMod6 <- glmer(HerbGain ~ Treatment 
                  +CLUMPY.600
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 2700 meters
GainMod7 <- glmer(HerbGain ~ Treatment 
                  +CLUMPY.2700
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management and habitat configuration at 3300 meters
GainMod8 <- glmer(HerbGain ~ Treatment 
                  +CLUMPY.3300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management, habitat amount at 300 meters, and
# habitat configuration at 300 meters
GainMod9 <- glmer(HerbGain ~ Treatment
                  + CA.300 
                  + CLUMPY.300
                  + (1|Preserve), 
                  family = poisson(link = 'log'), 
                  data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 300 meters
GainMod13 <- glmer(HerbGain ~ Treatment
                   + CA.900 
                   + CLUMPY.300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 2700 meters
GainMod15 <- glmer(HerbGain ~ Treatment
                   + CA.900 
                   + CLUMPY.2700
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 900 meters, and
# habitat configuration at 3300 meters
GainMod16 <- glmer(HerbGain ~ Treatment
                   + CA.900 
                   + CLUMPY.3300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Management, habitat amount at 3400 meters, and
# habitat configuration at 300 meters
GainMod17 <- glmer(HerbGain ~ Treatment
                   + CA.3400 
                   + CLUMPY.300
                   + (1|Preserve), 
                   family = poisson(link = 'log'), 
                   data = Data)

# Creating a list of the top models in the confidence set:
Top.models <- list(GainMod13,
                   GainMod3,
                   GainMod5,
                   GainMod16,
                   GainMod15,
                   GainMod1,
                   GainMod17,
                   GainMod9,
                   GainMod7,
                   GainMod6,
                   GainMod8,
                   GainMod4)

# Storing the results of the multi model average:
GainResults <- summary(model.avg(Top.models))

# Models that were in the top model set for herb Losses:

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
# Putting the results of the richness, gains, and losses multimodel
# averages together for the figure: 

Richness = as.data.frame(RichResults$coefmat.subset)
Richness$Model = "Richness"
Richness$Factor = row.names(Richness)

Gains = as.data.frame(GainResults$coefmat.subset)
Gains$Model = "Gains"
Gains$Factor = row.names(Gains)

Loss = as.data.frame(LostResults$coefmat.subset)
Loss$Model = "Loss"
Loss$Factor = row.names(Loss)

# Putting together the effect size data
EffectSizeData = rbind(Richness, Gains, Loss)

row.names(EffectSizeData) = NULL

EffectSizeData = EffectSizeData %>% dplyr::select(Model, Factor, Estimate, `Std. Error`, `Pr(>|z|)`)

EffectSizeData = EffectSizeData %>% dplyr::filter(Factor != "(Intercept)")

EffectSizeData = EffectSizeData %>% dplyr::mutate(HI95 = Estimate + (1.96*`Std. Error`),
                                                  LO95 = Estimate - (1.96*`Std. Error`))

EffectSizeData = EffectSizeData %>% 
  dplyr::mutate(NewFactor = ifelse(Factor == "TreatmentBurned", "Burned", 
                                   ifelse(Factor == "CA.900", "Amount (0.9 km)", 
                                          ifelse(Factor == "CA.300", "Amount (0.3 km)", 
                                                 ifelse(Factor == "CA.3400", "Amount (3.4 km)", 
                                                        ifelse(Factor == "CLUMPY.300", "Config (0.3 km)", 
                                                               ifelse(Factor == "CLUMPY.600", "Config (0.6 km)", 
                                                                      ifelse(Factor == "CLUMPY.2700", "Config (2.7 km)", 
                                                                             ifelse(Factor == "CLUMPY.3300", "Config (3.3 km)", 
                                                                                    Factor)))))))))

# Determining which predictors have a real effect (95% confidence not overlapping 0)
EffectSizeData = EffectSizeData %>% dplyr::mutate(Significant = ifelse((HI95 > 0 & LO95 > 0)|(HI95 < 0 & LO95 < 0), TRUE, FALSE))

# Re-naming 'Loss" to 'Losses' for labelling 
EffectSizeData = EffectSizeData %>% dplyr::mutate(Model = ifelse(Model == "Loss", "Losses", Model))

# Changing the order in which 'Richness', 'Gains', and 'Losses' are displayed 
EffectSizeData$Model2 = factor(EffectSizeData$Model, levels = c("Richness", "Gains", "Losses"))

# Chaning the order that the x-axis labels are displayed
EffectSizeData$NewFactor2 = factor(EffectSizeData$NewFactor, levels = c("Burned",
                                                                        "Amount (0.3 km)",
                                                                        "Amount (0.9 km)",
                                                                        "Amount (3.4 km)",
                                                                        "Config (0.3 km)",
                                                                        "Config (0.6 km)",
                                                                        "Config (2.7 km)",
                                                                        "Config (3.3 km)"))

# Creating the placement on annotiation to show marginal and conidtional R^2 values
ann_text1 <- data.frame(NewFactor2 = "Config (0.6 km)", Estimate = 1.5, Significant = TRUE,
                        Model2 = factor("Richness",levels = c("Richness","Gains","Losses")))

ann_text2 <- data.frame(NewFactor2 = "Config (0.6 km)", Estimate = 1.5, Significant = TRUE,
                        Model2 = factor("Gains",levels = c("Richness","Gains","Losses")))

ann_text3 <- data.frame(NewFactor2 = "Config (0.6 km)", Estimate = 1.5, Significant = TRUE,
                        Model2 = factor("Losses",levels = c("Richness","Gains","Losses")))

# Creating the figure
ggplot(EffectSizeData, aes(x = NewFactor2, y = Estimate, col = Significant)) +
  geom_segment(aes(x = EffectSizeData$NewFactor2, 
                   y = EffectSizeData$LO95,
                   xend = EffectSizeData$NewFactor2,
                   yend = EffectSizeData$HI95, 
                   col = Significant)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 'longdash') +
  scale_color_manual(values = c('grey', 'black')) +
  facet_wrap(~Model2) +
  labs(x = "Variable") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_text(data = ann_text1,label = expression(paste(R^2~m, "(", R^2~c, ")", " = 0.44(0.83)"))) +
  geom_text(data = ann_text2,label = expression(paste(R^2~m, "(", R^2~c, ")", " = 0.52(0.86)"))) +
  geom_text(data = ann_text3,label = expression(paste(R^2~m, "(", R^2~c, ")", " = 0.00(0.65)")))

################## Figure 5 #######################

# Adding a space inbetween 'Not' and 'Burned' in the 
# 'Treatment' column
UnSData = UnSData %>%
  dplyr::mutate(Treatment = as.character(Treatment)) %>%
  dplyr::mutate(Treatment = ifelse(Treatment == 'NotBurned', 'Not Burned', 'Burned')) %>% 
  dplyr::mutate(Treatmnet = as.factor(Treatment))

# Making the boxplots:

# For current native herb richness
RichB = ggplot(data = UnSData, aes(x = Treatment, y = Richness)) +
  geom_point(position=position_jitter(h=0.05, w=0.05), alpha = 0.5) +
  geom_boxplot(alpha = 0.5, fill = c('#67a9cf', '#ef8a62'), size = 0.5) +
  geom_segment(aes(x = 'Not Burned', y = 65, xend = 'Burned', yend = 65)) +
  geom_segment(aes(x = 'Not Burned', y = 65, xend = 'Not Burned', yend = 62)) +
  geom_segment(aes(x = 'Burned', y = 65, xend = 'Burned', yend = 62)) +
  annotate("text", x=1.5, y=70, label= "p < 0.01") + 
  geom_text(x=0.6, y=70, label="(A)") +
  xlab("Management") +
  ylim(-5, 70) +
  theme_bw()

# For native herb gains over the 30-year period
GainB = ggplot(data = UnSData, aes(x = Treatment, y = HerbGain)) +
  geom_point(position=position_jitter(h=0.05, w=0.05), alpha = 0.5) +
  geom_boxplot(alpha = 0.5, fill = c('#67a9cf', '#ef8a62'), size = 0.5) +
  geom_segment(aes(x = 'Not Burned', y = 65, xend = 'Burned', yend = 65)) +
  geom_segment(aes(x = 'Not Burned', y = 65, xend = 'Not Burned', yend = 62)) +
  geom_segment(aes(x = 'Burned', y = 65, xend = 'Burned', yend = 62)) +
  annotate("text", x=1.5, y=70, label= "p < 0.01") + 
  geom_text(x=0.6, y=70, label="(B)") +
  xlab("Management") +
  ylab("Species Gained") +
  ylim(-5, 70) +
  theme_bw()

# For native herb losses over the 30-year period
LostB = ggplot(data = UnSData, aes(x = Treatment, y = HerbLost)) +
  geom_point(position=position_jitter(h=0.05, w=0.05), alpha = 0.5) +
  geom_boxplot(alpha = 0.5, fill = c('#67a9cf', '#ef8a62'), size = 0.5) +
  ylim(-5, 70) +
  xlab("Management") +
  ylab("Species Lost") +
  geom_text(x=0.6, y=70, label="(C)") +
  theme_bw()

# Combining Richness, Gains, and Losses into one figure
grid.arrange(RichB, GainB, LostB, nrow = 1)

################## Figure 6 #######################

# Scatter plot with Loess smoother for Richness 
RichL = ggplot(data = UnSData, aes(x = CA.900, y = Richness)) +
  geom_point(alpha = 0.5) +
  geom_smooth(col = 'black') +
  ylim(-5, 60) +
  xlab("Habitat Area (ha)") +
  geom_text(x=12, y=58, label="(A)") +
  theme_bw()

# Scatter plot with Loess smoother for native herb gains 
GainL = ggplot(data = UnSData, aes(x = CA.900, y = HerbGain)) +
  geom_point(alpha = 0.5) +
  geom_smooth(col = 'black') +
  xlab("Habitat Area (ha)") +
  ylab("Species Gained") +
  geom_text(x=12, y=58, label="(B)") +
  ylim(-5, 60) +
  theme_bw()

# Scatter plot with Loess smoother for native herb losses 
LostL = ggplot(data = UnSData, aes(x = CA.900, y = HerbLost)) +
  geom_point(alpha = 0.5) +
  geom_smooth(col = 'black') +
  xlab("Habitat Area (ha)") +
  ylab("Species Lost") +
  geom_text(x=12, y=58, label="(C)") +
  ylim(-5, 60) +
  theme_bw()

# Combining the richness, native species gains, and losses 
# into one figure
grid.arrange(RichL, GainL, LostL, nrow = 1)




