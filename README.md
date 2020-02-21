Contains the experimental software, data and analysis scripts for the results reported in:

Popov, V., & Reder, L. (2019). Greater discrimination difficulty during perceptual learning leads to stronger and more distinct representations. OSF preprints. https://doi.org/10.31219/osf.io/ru7t6

The code runs with paths relative to the parent folder, which is set-up in every script by the here() package and the setwd(here()) command, which finds the .Rproj file and sets that as the working dir.

Files:

- data/search_raw.csv - raw search data
- data/search_preproc.csv - preprocessed search data ready for similarity analyses
- data/nback.csv - nback task data
- expfiles/characterslist.xlsx - vector representations for each character used in the study (ids correspond to ids in search_raw.csv. Data from Yang et al (2009)
- expfiles/characters_full_list.xlsx - vector representation for all ~5000 characters in Yang et al (2009)
- expfiles/char_dist.csv - information about average eucledian distance between each target character and its distractors
- expfiles/stimuli/ - folder containing image files for each character used in study. filename numbers correspond to character ids used in data files
- expfiles/software/ - E-prime code for experimental software
- scripts/preproc_data.R - transform data_raw.csv to data_preproc.csv
- scripts/similarity_analyses.R - main analysis file that does regressions and plots



All code runs under the following R, OS and packages versions:

```
R version 3.5.1 (2018-07-02)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] bindrcpp_0.2.2  lme4_1.1-18-1   Matrix_1.2-14   cowplot_0.9.3   forcats_0.3.0   stringr_1.3.1   dplyr_0.7.6     purrr_0.2.5     readr_1.1.1    
[10] tidyr_0.8.1     tibble_1.4.2    ggplot2_3.1.0   tidyverse_1.2.1

loaded via a namespace (and not attached):
 [1] tidyselect_0.2.4 splines_3.5.1    haven_1.1.2      lattice_0.20-35  colorspace_1.3-2 rlang_0.4.0      pillar_1.3.0     nloptr_1.0.4     glue_1.3.0      
[10] withr_2.1.2      modelr_0.1.2     readxl_1.1.0     bindr_0.1.1      plyr_1.8.4       munsell_0.5.0    gtable_0.2.0     cellranger_1.1.0 rvest_0.3.2     
[19] labeling_0.3     broom_0.5.0      Rcpp_0.12.19     scales_1.0.0     backports_1.1.2  jsonlite_1.5     hms_0.4.2        packrat_0.4.9-3  digest_0.6.18   
[28] stringi_1.2.4    grid_3.5.1       rprojroot_1.3-2  here_0.1         cli_1.0.1        tools_3.5.1      magrittr_1.5     lazyeval_0.2.1   crayon_1.3.4    
[37] pkgconfig_2.0.2  MASS_7.3-50      xml2_1.2.0       lubridate_1.7.4  assertthat_0.2.0 minqa_1.2.4      httr_1.3.1       rstudioapi_0.7   R6_2.3.0        
[46] nlme_3.1-137     compiler_3.5.1 
```