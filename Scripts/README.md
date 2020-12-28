Code
================

**Code in Folder:**
* [`Load_Data`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/main/Scripts/Load_Data): Scripts to load all data sources needed
* [`Aggregate_Data`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/main/Scripts/Aggregate_Data): Scripts to aggregate data to commune level


**Code for Analysis:**
* [`00-CargaLibrerias.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/00-CargaLibrerias.R): Load all required libraries.
* [`00-Functions.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/00-Functions.R): Common functions for the analysis.
* [`01-LoadAllData.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/01-LoadAllData.R): Consolidate all data into a single dataframe, to use in the analysis.ar todo nuevamente).
* [`02-FeatureData.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/02-FeatureData.R): Generate some features on the data. Clean data.
* [`03-DescriptiveTable.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/03-DescriptiveTable.R): Descriptive tables of the data.
	* [`commune_table.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/commune_table.R): Table for top and bottom communes in PM2.5.
* [`05-FunctionsCrossSectional.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/05-FunctionsCrossSectional.R): Functions to analyze the results derived from a fitted model.
* [`07-AnalysisCrossSectional.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/07-AnalysisCrossSectional.R): Cross sectional ecological study.
* [`08-CrossSectional_TableEndpoints.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/08-CrossSectional_TableEndpoints.R): SCross sectional ecological study: Summary Table  with different Endpoints.
* [`09-Loop_CrossSectional.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/09-Loop_CrossSectional.R): Cross sectional ecological study: Loop for multiple models.

**Code for Figures:**
* [`map.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/map.R): Map figure.
	* [`functions_map.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/functions_map.R): Functions to generate maps.
* [`age_pyramid.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/age_pyramid.R): Figure of age pyramid for Chile.
* [`deaths_avoided.R`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts/deaths_avoided.R): Calculate deaths avoided by improving air quality to different standards.

**Note:** To load all required libraries run the following command: *source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")*