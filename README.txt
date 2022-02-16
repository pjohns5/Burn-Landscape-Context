README FILE

############################################################################

Johnson, P.P., S. Kobal, W. Leonard, & E.S. Minor. 2022. Herbaceous plant 
richness increases with surrounding habitat and management burns over
30-years in suburban forest understories.

############################################################################

This file includes an overview of the data files and analyses scripts used 
in Johnson et al (2022). The session information for the R session used to
conduct the analyses is also included. 

############################################################################

File Structure: 

Data files begin with "D".
"D#_Description.csv"

Analysis script files begin with "A". 
"A#_Description.csv"

############################################################################

Included Data Files:

"D1_Anon_Native_Herbs_DuPage_1987_2016.csv" - Comma seperated file that
includes five columns: 'PlotCode', 'Year', 'Management', 'Anon_ID', and 
'Physiog'. This file includes the species code for the native species recorded
in each long-term monitoring plot between 1987 and 2016 along with the management
of the plot ('Burned' or 'NotBurned') and the growth form of the native species.
Species identifications are anonimyzed to protect rare species in the forest
preserves. De-anonimyzed species identifications can be requested for research 
purposes from the authors.

"D2_Fragstats_DuPage_ASCII.class" - Class file produced by the Fragstat 
software. Can be read as comma seperated file in R. Contains four columns: 'LID',
'Type', 'CA', and 'CLUMPY'. This file includes the landscape ID, class type
(habitat '1' or non-habitat '0'), class area, and clumpiness index as measured
with Fragstats over 37 nested spatial scales surrounding the long-term monitoring
plots.

"D3_Herb_Turnover.csv" - Comma seperated file that includes five columns: 
'PlotCode', 'Management', 'Richness', 'Gains', and 'Losses'. This file includes
the number of native herb species in the most recent survey of the long-term 
monitoring plots, the management of the plot ('Burned' or 'NotBurned'), and the 
number of native herb species gained and lost at each long-term monitoring plot
over the 30-year period. This csv can be produced using the 
A3_Current_Herb_Richness_Gains_Losses.R script. 

"D4_Habitat_Amount_Configuration.csv" - Comma seperated file with eight columns: 
'Site', 'CA.300', 'CA.900', 'CLUMPY.2700', 'CLUMPY.300', 'CLUMPY.3300', and 
'CLUMPY.600'. This file contains the outputs from Fragstats for the habitat amount 
and configuration at the candidate spatial scales that were determined with the 
A2_Identifying_Independent_Candidate_Spatial_Scales.R script. 

############################################################################

Included Scripts: 

"A1__Native_Herb_Richness_Change_DuPage_1987_2016.R" - Script to model the change
in native herb richness in burned and unburned long-term management sites from 
1987 to 2016. This script also includes code to construct Figure 3 of the paper.

"A2__Identifying_Independent_Candidate_Spatial_Scales.R" - Script to identify which
of the 37 spatial scales consider are independent measures of habitat area and habitat 
configuration. These scales were included as candidate scales in subsequent models. 
This script also includes code to construct Figure 4 of the paper.

"A3_Current_Herb_Richness_Gains_Losses.R" - Script to determine the number of 
native herb species in the most recent survey of the long-term monitoring plots, 
and the number of native herb species gained and lost over 30-years. 

"A4_Current_Richness_Management_Landscape_Context.R" - Script to determine the 
effect of the burn management and landscape context on the current native herb
richness in the long-term monitoring plots. 

"A5_Herb_Gains_Management_Landscape_Context.R" - Script to determine the 
effect of the burn management and landscape context on the number of native herb
species gained in the long-term monitoring plots over 30-years.

"A6_Herb_Losses_Management_Landscape_Context.R" - Script to determine the 
effect of the burn management and landscape context on the number of native herb
species lost in the long-term monitoring plots over 30-years.

"A7_Figure4_Figure5_Figure6.R" - Script to determine construct figure4, figure 5, 
and figure 6 in the manuscript. 

############################################################################

Citation for components of scripts: 

Zuur, A.F., E.N. Ieno (2017). Data from: A protocol for conducting and presenting
regression type analyses. Drayad. Dataset. https://doi.org/10.5061/dryad.v4t42

############################################################################

R session information when running script:

> sessionInfo()
R version 4.0.4 (2021-02-15)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 22000)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252 
[2] LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] AICcmodavg_2.3-1 gridExtra_2.3    ggplot2_3.3.3    MuMIn_1.43.17   
 [5] lmerTest_3.1-3   lme4_1.1-25      Matrix_1.3-2     tidyr_1.1.2     
 [9] stringr_1.4.0    dplyr_1.0.2      readr_1.4.0     

loaded via a namespace (and not attached):
 [1] statmod_1.4.35      tidyselect_1.1.0    purrr_0.3.4        
 [4] splines_4.0.4       lattice_0.20-41     colorspace_2.0-0   
 [7] vctrs_0.3.5         generics_0.1.0      stats4_4.0.4       
[10] mgcv_1.8-36         survival_3.2-7      utf8_1.1.4         
[13] rlang_0.4.11        nloptr_1.2.2.2      pillar_1.5.1       
[16] glue_1.4.2          withr_2.4.1         sp_1.4-5           
[19] lifecycle_1.0.0     plyr_1.8.6          munsell_0.5.0      
[22] gtable_0.3.0        raster_3.4-5        codetools_0.2-18   
[25] VGAM_1.1-5          labeling_0.4.2      parallel_4.0.4     
[28] fansi_0.4.1         Rcpp_1.0.8          xtable_1.8-4       
[31] scales_1.1.1        unmarked_1.0.1      farver_2.0.3       
[34] digest_0.6.27       hms_1.0.0           stringi_1.5.3      
[37] numDeriv_2016.8-1.1 grid_4.0.4          cli_2.3.1          
[40] tools_4.0.4         magrittr_2.0.1      tibble_3.0.4       
[43] crayon_1.4.1        pkgconfig_2.0.3     MASS_7.3-53        
[46] ellipsis_0.3.1      rstudioapi_0.13     assertthat_0.2.1   
[49] minqa_1.2.4         R6_2.5.0            boot_1.3-26        
[52] nlme_3.1-152        compiler_4.0.4     
> 

############################################################################


