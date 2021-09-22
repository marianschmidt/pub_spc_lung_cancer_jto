# Code for SPC lung cancer paper
This repository includes the analysis code for paper 'Incidence of smoking-related second primary cancers after lung cancer in Germany: an analysis of nationwide cancer registry data' submitted to Journal of Thoracic Oncology.


## Structure of the repository

`1_input`: Folder containing input, i.e. data files. This folder is currently empty due to raw data access restrictions (see below).

`2_scripts`: Folder containing analysis scripts. Due to missing input files, the scripts will currently not run, but you are welcome to check the code for errors or inconsistencies.

`3_output`: Folder containing all output files. This includes all figures and tables.

`4_manuscript`: Folder containing the source file for the manuscript. Availability may depend on the journal's embargo policy.


## Package version control

This repository uses `renv` to ensure that a project-specific R package library is used and that functionality is maintained even if packages have breaking changes in the future.

If you clone this package, make sure you create the same package library by using the command `renv::restore()` to restore the project library locally to your machine.
You find more details on https://rstudio.github.io/renv/articles/collaborating.html. 


## Raw data access

Due to legal restrictions, the individual level raw data used for this analysis is only available via request to the German Center for Cancer Registry Data (ZfKD) that can provide a scientific use file.
More information on the application process is provided on the ZfKD website (https://www.krebsdaten.de/Krebs/EN/Content/ScientificUseFile/scientificusefile_node.html).


