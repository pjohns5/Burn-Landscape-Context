# Johnson, P.P., S. Kobal, W. Leonard, & E.S. Minor. 2022. 
# Herbaceous plant richness increases with surrounding 
# habitat and management burns over 30-years in suburban 
# forest understories. 

########################################################

# Analysis 3, determining current native herb richness, 
# species gains, and species losses, over 30-years.

########################################################

# Loading necessary R packages
library(readr)
library(dplyr)

################## Analysis code #######################

# Loading in the native herb data 
data = read_csv("D1_Anon_Native_Herbs_DuPage_1987_2016.csv")

# Determining the current native richness at each site
rich_data = data %>%  
  dplyr::group_by(PlotCode) %>% 
  dplyr::filter(Year == max(Year)) %>%
  dplyr::summarise(Management = unique(Management),
                   Richness = length(Anon_ID))

# Determining the native species gains and losses over
# the 30-year period

# Creating a list of plots to loop over: 
plots = unique(data$PlotCode)

# Creating an empty data frame to store the results
gains_loss_data = data.frame()

# Writing the for loop to find the number of native
# species gained and lost from the sites

# For every plot in the plots list
for (plot in plots) {
  # Select only those rows in the data that correspond
  # to the plot and the initial survey of that plot
  # store these in a variable 'initial_data'
  initial_data = data %>%
    dplyr::group_by(PlotCode) %>% 
    dplyr::filter(Year == min(Year)) %>% 
    dplyr::filter(PlotCode == plot)
  # Select only those rows in the data that correspond
  # to the plot and the most recent survey of that plot
  # store these in a variable 'final_data'
  final_data = data %>% 
    dplyr::group_by(PlotCode) %>% 
    dplyr::filter(Year == max(Year)) %>% 
    dplyr::filter(PlotCode == plot)
  # Determine the species gained by finding which species
  # were identified in the most recent survey that were
  # not identified in the initial survey of the site.
  Gains = dplyr::setdiff(final_data$Anon_ID, initial_data$Anon_ID)
  # Find the number of species gained by looking at the 
  # number of species in the Gains vector. 
  Gains = length(Gains)
  # Determine the species lost by finding which species
  # were identified in the initial survey that were 
  # not identified in the most recent survey
  Losses = dplyr::setdiff(initial_data$Anon_ID, final_data$Anon_ID)
  # Find the number of species gained by looking at the 
  # number of species in the Losses vector. 
  Losses = length(Losses)
  # Create a dataframe with a one row that contains the
  # plot code, the number of gains, and number of losses
  new_row = data.frame(plot, Gains, Losses)
  # Adding the new_row to the gains_loss_data datafrme
  gains_loss_data = rbind(gains_loss_data, new_row)
}

# Combining the rich_data dataframe and the gains_loss_data
# dataframe to create the species turnover data.frame
turnover_data = rich_data %>% dplyr::left_join(gains_loss_data, by = c("PlotCode" = "plot"))

# Inspecting the turnover_data
turnover_data
