# Johnson, P.P., S. Kobal, W. Leonard, & E.S. Minor. 2022. 
# Herbaceous plant richness increases with surrounding 
# habitat and management burns over 30-years in suburban 
# forest understories. 

########################################################

# Analysis 2, selecting a set of candidate spatial scales
# to use for evaluating the effect of landscape context
# on native species richness, gains, and losses over
# 30-years in the long-term monitoring plots.

########################################################

# Loading necessary R packages

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

################## Analysis code #######################

# Loading the Fragstats data: Data
Data <- read_csv("D2_Fragstats_DuPage_ASCII.class")

# Creating string patterns to extract site and spatial
# scale form the Fragstats output:
SitePattern <- "..(p|P)(l|L)\\d"

SpatialPattern <- "\\d\\d\\d\\d*"

# Creating two columns for the site and scale in the Data: 
Data <- mutate(Data, 
               Site = str_extract(LID, SitePattern),
               Scale = str_extract(LID, SpatialPattern))

# Converting the 'Scale' column to numeric:
Data <- mutate(Data, 
               Scale = as.numeric(Scale))

# Removing the 'LID' column: 
Data <- select(Data, -LID)

# Selecting only the forest habitat:
# Forest habitat was calssified as type "cls_1" in Fragstats. 
Data <- filter(Data, TYPE == "cls_1")

# Finding the buffer sizes to loop over and test against:
# We sort these to ensure that we are always comparing/considering
# the next largest spatial scale.
scales <- sort(unique(Data$Scale)) # The scale to compare against

test_scales = sort(unique(Data$Scale)) # The scales we are considering

# Creating a data frame to store the results of the the 
# foor loop to find the correlation with class area (CA) data:
RhoCAData <- data.frame()

# Nested for loop to evaluate correlation of CA between scales:

# Since we initial compare potential scales to the 300 meter scale,
# we set up a vector 'finding_indpendent_scale' that already contains
# 300 meters because this is what we will initially compare against.
finding_independent_scale <- c(300)

# We set next_scale to 300 initially, as we will begin comparing scales
# to 300 meters. We modify this in the for loop to compare against the
# next spatial scale that is independent of the 300 meter scale. 
next_scale = 300

# The for loop:

# For every scale in the scales vector (scales to compare against)
for (scale in scales) {
  # Establish an empty vector to store potential independent scales
  increment_scale = c()
  # If the scale we are comparing against is greater than the next_scale
  if (scale >= next_scale) {
    # Loop through the test_scales.
    for (test_scale in test_scales) {
      # If the test_scale (the scale we are considering) is greater than
      # the scale we are comparing it to
      if (test_scale >= scale) {
        # Filter the Data to get rows of the scale we are comparing to
        initial <- filter(Data, Scale == scale)
        # Filter the Data to get rows of the scale we are considering
        test <- filter(Data, Scale == test_scale)
        # Calculte the spearman correlation between these
        caRho <- cor(initial$CA, test$CA, method = 'spearman')
        # Create a new row that we can add to our RhoCAData
        NewRow <- data.frame(scale, test_scale, caRho)
        # Add the new row to the RhoCAData
        RhoCAData <- rbind(RhoCAData, NewRow)
        # If the scale we are considering is no longer strongly correlated
        # (i.e. idenpendent)
        if (caRho < 0.7) {
          # Add that scale to the increment_scale (i.e. the potentially
          # independent) vector
          increment_scale = c(increment_scale, test_scale) }
      }
      # If the incremental_scale vector has anything in it
      if (length(increment_scale) != 0) {
        # Find the smallest independent scale to be considered next
        next_scale <- min(increment_scale)
        # Add this independent scale to the finding_independent_scale vector
        finding_independent_scale = c(finding_independent_scale, next_scale)
        # Keep only the unique values (since it will add the same minimum
        # idepenent scale for each independent scale greater than the next_scale)
        finding_independent_scale = unique(finding_independent_scale)
        } 
    }
  }
}

# Checking the independent scales found with the for loop:
finding_independent_scale

# Filtering the RhoCAData to only keep the correlations with the three 
# independent scales we found with the for loop above. 
RhoCAData = RhoCAData %>% 
  filter(scale %in% finding_independent_scale)

# Nested for loop to evaluate correlation of CLUMPY between scales:

# Creating a data frame to store the results of the the 
# foor loop to find the correlation with clumpiness index (CLUMPY) data:
RhoCLUMPYData <- data.frame()

# Since we initial compare potential scales to the 300 meter scale,
# we set up a vector 'finding_indpendent_scale' that already contains
# 300 meters because this is what we will initially compare against.
finding_independent_scale <- c(300)

# We set next_scale to 300 initially, as we will begin comparing scales
# to 300 meters. We modify this in the for loop to compare against the
# next spatial scale that is independent of the 300 meter scale. 
next_scale = 300

# The for loop:

# For every scale in the scales vector (scales to compare against)
for (scale in scales) {
  # Establish an empty vector to store potential independent scales
  increment_scale = c()
  # If the scale we are comparing against is greater than the next_scale
  if (scale >= next_scale) {
    # Loop through the test_scales.
    for (test_scale in test_scales) {
      # If the test_scale (the scale we are considering) is greater than
      # the scale we are comparing it to
      if (test_scale >= scale) {
        # Filter the Data to get rows of the scale we are comparing to
        initial <- filter(Data, Scale == scale)
        # Filter the Data to get rows of the scale we are considering
        test <- filter(Data, Scale == test_scale)
        # Calculte the spearman correlation between these
        clumpyRho <- cor(initial$CLUMPY, test$CLUMPY, method = 'spearman')
        # Create a new row that we can add to our RhoCAData
        NewRow <- data.frame(scale, test_scale, clumpyRho)
        # Add the new row to the RhoCAData
        RhoCLUMPYData <- rbind(RhoCLUMPYData, NewRow)
        # If the scale we are considering is no longer strongly correlated
        # (i.e. idenpendent)
        if (clumpyRho < 0.7) {
          # Add that scale to the increment_scale (i.e. the potentially
          # independent) vector
          increment_scale = c(increment_scale, test_scale) }
      }
      # If the incremental_scale vector has anything in it
      if (length(increment_scale) != 0) {
        # Find the smallest independent scale to be considered next
        next_scale <- min(increment_scale)
        # Add this independent scale to the finding_independent_scale vector
        finding_independent_scale = c(finding_independent_scale, next_scale)
        # Keep only the unique values (since it will add the same minimum
        # idepenent scale for each independent scale greater than the next_scale)
        finding_independent_scale = unique(finding_independent_scale)
      } 
    }
  }
}

# Checking the independent scales found with the for loop:
finding_independent_scale

# Filtering the RhoCLUMPYData to only keep the correlations with the three 
# independent scales we found with the for loop above. 
RhoCLUMPYData = RhoCLUMPYData %>% 
  filter(scale %in% finding_independent_scale)

################## Figure code #########################

# Converting the scale column in both RhoCAData and 
# RhoCLUMPYData into factors, to make assigning
# shapes easier.
RhoCAData = RhoCAData %>% 
  dplyr::mutate(scale = as.factor(scale))

RhoCLUMPYData = RhoCLUMPYData %>% 
  dplyr::mutate(scale = as.factor(scale))

# Construct figure 4A, for correlation of class area (CA)
CA = ggplot(RhoCAData, aes(x = test_scale, y = caRho, shape = scale)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.7, linetype = 'longdash') +
  labs(x = "Buffer Size (meters)", y = "Spearmans Rho", shape = "Scale") +
  scale_x_continuous(limits = c(0, 4000), breaks = seq(0, 4000, 1000)) +
  geom_text(x=0, y=0.95, label="(A)") +
  theme_bw()

# Construct figure 4B, for correlation of clumpiness index (CLUMPY)
CLUMPY = ggplot(RhoCLUMPYData, aes(x = test_scale, y = clumpyRho, shape = scale)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0.7, linetype = 'longdash') +
  labs(x = "Buffer Size (meters)", y = "Spearmans Rho", shape = "Scale") +
  scale_x_continuous(limits = c(0, 4000), breaks = seq(0, 4000, 1000)) +
  geom_text(x=0, y=0.95, label="(B)") +
  theme_bw()

# Combining figure 4A and figure 4B together
grid.arrange(CA, CLUMPY, nrow = 2)
