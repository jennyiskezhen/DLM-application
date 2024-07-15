# DLMs Application - Intercept Parameter Time-series

The dynamic linear models (DLMs) can be used for both predicting water quality values and as a diagnostic tool to quantify the dynamics in the underlying relationship between water quality data and streamflow.  DLMs can be applied for investigating soil erosional processes such as the occurrence of an erosional event and the lasting elevated sediment yield after the event, sudden increase of water quality constituent level and any persistent effects, as well as the effectiveness and longevity of stream restoration projects.      

DLMs have been applied to characterize the multi-scale suspended sediment transport dynamics across datasets with a wide range of conditions. DLMs produced more accurate suspended sediment concentration estimates than many static regression models (Wang et al., 2021a), characterized the dynamicity of sediment and flow relationship across catchments with diverse properties (Wang and Steinschneider, 2022), as well as captured the turning point of sediment regime shift attributed to stream restoration projects (Wang et al., 2021b). Apart from suspended sediment application, DLMs are suitable for any water quality constituents when the variations of the constituent levels are predominantly influenced by streamflow. The DLM parameters fitted on the constituent and flow rating curve relationship (i.e., intercept and slope) are updated at time step of the data and contain information on the changes of water quality source availability and the transport power of the river. 

For more information on the model algorithm and previous applications, please refer to the following papers:

1. Wang, K., & Steinschneider, S. (2022). Characterization of Multi-Scale Fluvial SuspendedSediment Transport Dynamics Across the United States Using Turbidity and Dynamic Regression.*Water Resources Research*, 58(10). <https://doi.org/10.1029/2021WR031863>
2. Wang, K., Davis, D., & Steinschneider, S. (2021b). Evaluating suspended sediment and turbidityreduction projects in a glacially conditioned catchment through dynamic regression and fluvialprocess‐based modelling. *Hydrological Processes*, 35(9). <https://doi.org/10.1002/hyp.14351>
3. Wang, K., Gelda, R. K., Mukundan, R., & Steinschneider, S. (2021a). Inter-model Comparison ofTurbidity-Discharge Rating Curves and the Implications for Reservoir Operations Management.*Journal of the American Water Resources Association*, 57(3). <https://doi.org/10.1111/1752-1688.12906>

<mark>The following content shows you how to apply DLMs to your data and generate informative DLM parameter time-series that capture the changes in the water quality source supply availability (i.e., the intercept parameter).</mark> While DLMs can be applied to dataset with any time steps, daily data are found to produce more stable and interpretable results. When the time steps are small, e.g. 15-min, the generated DLM parameter time-series may reflect the uncertainties of the dataset, rather than the changes of underlying relationship between the constituent and flow conditions. 

> Please note that the code is a simplified version of the ones used for journal publication. Modification of the code may be required to optimize the results.

## Data 
The dataset should have three columns in the following order:

| Date | Flow | Water Quality Data |
| :--------: | :------: | :----: |
|    ...   |  ...   |  ...  |

The dataset used to run the DLMs are in the folder **`data/Data_test.csv`**. Please replace it with your own data.

## Working Directory
Once all the files are downloaded in your local directory, open **`main.R`**. You can run this file right away. The following code will make sure your working directory is set at the current folder. 

```bash
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
```

## Results
After the code finishes running, the DLM results will be plotted and saved in the **`DLM_results.jpeg`** file. You can examine changes in the intercept parameter time-series and compare the timing with known disturbance events or restoration efforts. 
