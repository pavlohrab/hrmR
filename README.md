# Background

![Visitor count](https://shields-io-visitor-counter.herokuapp.com/badge?page=pavlohrab.hrmR&style=for-the-badge)
![GitHub](https://img.shields.io/github/license/pavlohrab/hrmR?style=for-the-badge)
![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/pavlohrab/hrmR?include_prereleases&style=for-the-badge)
![GitHub issues](https://img.shields.io/github/issues/pavlohrab/hrmR?style=for-the-badge)
![GitHub Repo stars](https://img.shields.io/github/stars/pavlohrab/hrmR?style=for-the-badge)
![GitHub contributors](https://img.shields.io/github/contributors/pavlohrab/hrmR?style=for-the-badge)

![DOI](http://img.shields.io/badge/DOI-10.5281%20%2F%20zenodo.4491296-blue.svg?style=for-the-badge)

The hrmR is a shiny app for High Resolution Melting analysis of melting data from Cfx PCR machines (tested with Cfx96). The app is available [here](https://pavloh.shinyapps.io/hrmR/) and require no installation.

# Installation
No installation is nedeed. The app is available via [link](https://pavloh.shinyapps.io/hrmR/). However, the app is hosting on a free shinyapps.io tier, therefore some troubles in connections can be spotted.
So, local run is prefered, given that the GUI in identival to the web app.
To run the app localy the one should have the [Rstudio](https://rstudio.com/products/rstudio/download/) and [R](https://www.r-project.org) installed in a system.
First, clone the repository in your desired location. You can do that via [git](https://git-scm.com) -> `git clone https://github.com/pavlohrab/hrmR` , or just downloading the zip file of a repository. (from a dropout menu in green "Code" button in the upper right corner of a repo)
Then, open app.R via Rstudio, and install following packages (using the Rconsole below the app.R code):
```R
install.packages(c("shiny", "MBmca", "ggplot2", "dplyr", "reshape2", "mclust",  "plotly", "xtable","shinyjs", "tidyverse", "factoextra", "dbscan", "cluster", "devtools" ))
```
The package installation can take a while....
After all the packages from cran are installed,  the ones, that are deprecated from cran are need to be manually installed:
```R
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/MBmca/MBmca_0.0.3-5.tar.gz")
```
```R
devtools::install_url("https://cran.r-project.org/src/contrib/Archive/chipPCR/chipPCR_0.0.8-10.tar.gz")
```
Then just press the "Run app" icon in the right top corner of the app.R script:

![run](images/run.png)

**Hint: You can change where to run the app (browser or viewer) via dropout menu near "Run app" button**

# Inputs
Input is a single dataframe with the RFU values and the first Temperature column. The dataframe is intended to be a csv file, but the delimiter either ";" or "," and 'Temperature' column preceeding with an empty one. However, one of the columns in a provided file should be named 'Temperature', others would be treated as experiment values. The example file (from Yi Liu solution for python [here](https://github.com/liuyigh/PyHRM)) in the example folder.
# Usage
After uploading a csv file, a couple of options are available, which could be divided into 3 categories:
1. Clean the data
2. Clustering options
3. HRM analysis options

It should be mentioned, that the hrmR is entirely depending on MBmca library ([link](https://www.rdocumentation.org/packages/MBmca/versions/0.0.3-5)). 
## Clean the data
After uploading the file you can tweak the data:
1. Interpolate the data (n parameter in [mcaSmoother](https://www.rdocumentation.org/packages/MBmca/versions/0.0.3-5/topics/mcaSmoother) function of MBmca)
2. Smooth the curves (df.fact parameter in [mcaSmoother](https://www.rdocumentation.org/packages/MBmca/versions/0.0.3-5/topics/mcaSmoother) function of MBmca)
3. Drop some data columns or reset the data
4. Trim the range used in the analysis

**Results**:
After uploading a csv file, melting peak plot is available under "Melting curves" tab and melting peak table under "Melting temps" tab. Also, normal curves (not the derivative plot) plot is available under "HMR analysis" tab.
**Note:** The "Used data" tab is always available with the table, which illustrates the data used for clustering and plot building
## Clustering options
The following clustering options are available when the "Enable clustering values" checkbox is checked:
1. Algorithm type: K-means, Model-based, DBscan, Hierarchical clustering. 
2. For every clustering algorithm there are options to pass (except Model-based clustering) and button "Cluster" should be pressed. The options (the ones that need to be filled) are showing individually for every algorithm class.

When HRM mode is not checked, clustering would go with melting temperatures of every curve. If the HRM mode enabled clustering would use the whole range of scaled RFU values (See the difference in "Used data" tab). 

When k-means or Hierarchical clustering is chosen then the "Clustering" tab will hold additional charts to help to choose the optimal number of clusters,
## HRM analysis option
The HRM analysis works only after the reference column (sample) for diff plot is chosen. This sample will be plotted at 0 on the x-axis and other curves are displayed as a difference from the chosen reference sample. So:
1. Choose the reference sample to centre the plot and press the "Plot" button
2. Trim temperatures influence only diff plot and data used for clustering (see "Used data" tab)

**Note:** Clustering will take place on the whole range on the difference dataframe, not only on melting temps when the HRM analysis is on.

## Citation
If you found this work helpful, please cite as following:
Pavlo Hrab. (2021, February 2). pavlohrab/hrmR: Pre-release version (Version v0.1-alpha). Zenodo. http://doi.org/10.5281/zenodo.4491296
