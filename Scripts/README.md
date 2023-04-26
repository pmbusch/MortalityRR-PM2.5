Code
================

**Code in Folder:**
* [`Load_Data`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/main/Scripts/Load_Data): Scripts to load all data sources needed
* [`Aggregate_Data`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/main/Scripts/Aggregate_Data): Scripts to aggregate data to commune level
* [`Supplementary_Analysis`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/main/Scripts/Supplementary_Analysis): Scripts to conduct some exploratory analysis, outside of the main body


**Code for Analysis:**
* [`00-CargaLibrerias.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/00-CargaLibrerias.R): Load all required libraries.
* [`00-Functions.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/00-Functions.R): Common functions for the analysis.
* [`01-LoadAllData.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/01-LoadAllData.R): Consolidate all data into a single dataframe, to use in the analysis.ar todo nuevamente).
* [`02-FeatureData.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/02-FeatureData.R): Generate some features on the data. Clean data.
* [`03-DescriptiveTable.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/03-DescriptiveTable.R): Descriptive tables of the data.
* [`04-CommuneTable.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/04-CommuneTable.R): Table for top and bottom communes in PM2.5.
* [`05-FunctionsCrossSectional.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/05-FunctionsCrossSectional.R): Functions to analyze the results derived from a fitted model.
* [`06-AnalysisCrossSectional.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/06-AnalysisCrossSectional.R): Cross sectional ecological study.
* [`07-CrossSectional_TableEndpoints.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/07-CrossSectional_TableEndpoints.R): SCross sectional ecological study: Summary Table  with different Endpoints.
* [`08-Loop_CrossSectional.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/08-Loop_CrossSectional.R): Cross sectional ecological study: Loop for multiple models with different endpoints and age groups.
* [`09-Loop_Sensitivity.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/09-Loop_Sensitivity.R): Cross sectional ecological study: Loop for multiple models with different endpoints and sensitivity cases.


**Code for Figures:**
* [`Figure2.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/Figure2.R): Figure 2 of main article. Sensitivity of the MRR of PM2.5 under different scenarios and causes of death.
* [`age_pyramid.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/age_pyramid.R): Figure of age pyramid for Chile.


**Note:** To load all required libraries run the following command: *source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")*