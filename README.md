# Italian-Olive

### Data Cleaning
- The code loads a dataset named `olive` into a variable `data`.
- The columns `Area` and `Region` in the dataset are transformed: numeric values are replaced with textual labels. For example, `Area == 1` is replaced with "Puglia Nord".
- The transformation is applied using the `mutate` and `case_when` functions from the `dplyr` package, which are part of the Tidyverse.
- After transforming these categorical variables, the code creates a new dataframe, `data_df`, where specific columns (listed in `lista_Acidi`) are divided by 100. This might be for scaling or normalization purposes.
- It then calculates the total percentages for each row (excluding `Region` and `Area`) and stores the results in `totalPcts`.
- The dataset `data_sat` is created, which includes new columns representing the sum of certain fatty acids (saturated, monounsaturated, and polyunsaturated).

### Descriptive Analysis
- The dataset is grouped by `Region`, and the mean percentages of various fatty acids are calculated and reshaped using `gather`, `group_by`, `summarise`, and `spread` functions.
- The results are displayed in a table using `knitr::kable`.

### Analysis of Fatty Acid Composition by Area
- Another analysis is conducted where the data is grouped by `Area`. It calculates the mean values of different types of fatty acids (saturated, monounsaturated, and polyunsaturated) for each area.
- The resulting data is displayed in a table.

### Data Visualization
- The code assumes that `mean_area` is already created and formatted.
- It transforms `mean_area` into a long format using `pivot_longer` and then creates a bar plot using `ggplot2`. The plot shows the average values of different types of fatty acids by area.

### Clustering Analysis
- The `data_cluster` dataset is created by selecting certain columns from `data` and applying a transformation to the `Area` column.
- The data is scaled (excluding the `Area` column) for clustering analysis.
- K-means clustering is performed on the scaled data with 4 clusters, and the cluster assignments are added back to the `data_cluster`.
- A visualization of the clustering results is created using `fviz_cluster`.

### Hierarchical Clustering Analysis
- The code performs hierarchical clustering using Partitioning Around Medoids (PAM) with 4 clusters.
- The clustering results are added to the original data, and a PCA is performed for visualization purposes.
- The `Area` column is again transformed, and a scatter plot of the first two principal components is created, colored by cluster.

### Silhouette Width Analysis
- The code evaluates the optimal number of clusters for PAM clustering using the average silhouette width method.
- It iterates through different numbers of clusters (from 2 to 10) and calculates the average silhouette width for each.
- A plot of the average silhouette widths for different numbers of clusters is created, helping to determine the best number of clusters.

Overall, this script performs a comprehensive analysis of the `olive` dataset, including data cleaning, transformation, descriptive analysis, clustering, and the evaluation of clustering results.
