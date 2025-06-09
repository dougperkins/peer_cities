# Peer Cities

This project aims to help cities identify their "housing peer cities" - cities that are similar with regard to a set of housing-related variables. It improves upon existing tools to do so by:

- Improving upon the set of American Community Survey (ACS) variables used to cluster cities

- Deriving temporal versions of these variables, so that clustering captures 



Data is from the 2023, 2018, and 2013 American Community Survey 5-year data, accessed using their API via the tidycensus R library with a Census API key.

Clone this project and run top_level.R line by line in RStudio.

Dependencies are imported at the top of top_level.R

## Directory Structure

- **`notebooks/`**  
  Includes Jupyter Notebooks for exploratory data analysis (EDA), prototyping, and documenting workflows.

- **`references/`**  
  Stores external resources such as research papers, datasets, or documentation that provide context.

- **`reports/`**  
  Contains generated reports, such as PDFs, HTML files, or Markdown documents.

- **`src/`**  
  Holds the main source code for the project.
  
  - top_level.R
  
  - constants.R
  
  - utils.R
  
  - acs.R
  
  - validate.R
  
  - preprocess.R
  
  - dim_red.R
  
  - cluster.R
  
  - eval.R
  
  - peers.R
  
  - app.py

- **`data/`**
  
  Holds public data used in the project

## Current Conceptual Workflow

- Load & combine data

- Feature Engineering
  
  - Derived features
    
    - Aggregates of ACS variables (ie pre-1980 structures)
    
    - Percents
    
    - Temporal
  
  - Log transformations of skewed variables

- Filter cities

- Preprocessing
  
  - Dimension Reduction (generate DR datasets for pre-checks, clustering, and/or final vis.)
    
    - PCA
    
    - kPCA (rbfdot)
  
  - Pre-Clustering (Tendency) checks
    
    - PCA: if 1st 2-3 PCs have high variance explained; low "intrinsic dimensionality"
    
    - Distance distribution
    
    - Hopkins
  
  - Feature Selection
    
    - Variance
    
    - Correlation
    
    - Spectral / Sparse Learning Multivariate Filter method? (per review, these work best)

- Clustering

- Evaluation

- Visualization

- Peer List

- Run App

## Current Workflow

- Load all cities and all variables
  
  - ACS 5y 2013, 2018, 2023: places, counties, townships (NJ only)

- As a list of place/county/township-row dfs with identical variables:
  
  - Filter down to the 960 PCIT cities
  
  - Drop margin of error columns for all variables
  
  - Derive variables
  
  - Tag each df's columns with its year
  
  - Bind each df into one large df (now a city-row df with lag variables)
  
  - Derive temporal variables (2023-2018, 2018-2013)

## Future Development

Bolded things are more immediate goals.

- Features
  
  - Add and remove features in existing themes
    
    - Total population should come from either the decennial Census or between-year Census Bureau population estimates, *not ACS data*. Affects a number of features in PCIT.
    
    - Comparisons should only be done between *non-overlapping* 5y ACS estimates.
    
    - Housing
      
      - Very few remaining possible static ACS variables
      
      - Many remaining derived ACS variables
        
        - 
  
  - Temporal versions of existing features
    
    - **5-year differences (only 3): 2019-2023, 2014-2018, 2009-2013,**
      
      - Once 2024 is released, can get 4: 2005-2009 and move the rest up 1
    
    - **Lag variables**
    
    - 1-year differences (only : 2023-2024, 2022-2023, 2022-2024? ...
      
      - Capture more recent changes
      
      - Many more data points per city
      
      - Pre-post 2008, 2019
  
  - Density versions of existing features
    
    - Area-, housing unit-, household-, and population-densities
  
  - Percents of metro area, county, state, nation

- Data
  
  - MA
    
    - Financial data (DLS Databank)
  
  - US
    
    - Database of Accredited Post-Secondary Institutions and Programs
    
    - Rand State Statistics
    
    - Bureau of Economic Analysis (county level)
    
    - National League of Cities
    
    - ALFIN
    
    - AHS
    
    - 1970 Census
      
      - Nat. Historical GIS of Minnesota Population Center @ U of Minnesota, for calculating change in labor share of manufacturing 1970-2022.
  
  - Global
    
    - Socioeconomic dataset in Hachaichi 2023
    
    - Climate dataset in Hachaichi 2023

- Themes
  
  - Housing
  - Housing Changes
  - Financial
  - General

- Uses
  
  - Leave-feature-out peers
  
  - Previously-similar peers
  
  - Non-peer extreme examples
  
  - Most similar in state
  
  - Most similar per state
  
  - Most similar per county
  
  - Peer tiers (esp. with hierarchical methods)
    
    - Displayed in graph (how to transform into graph data?)

- City Lists
  
  - Expanded US cities
    
    - Townships, other geographies?
  
  - Global cities

- Preprocessing
  
  - **Dimension Reduction**
    
    - PCA
      
      - For multicollinearity
      
      - For visualization
      
      - Working with low-variance PCs
    
    - kPCA
      
      - Tune hyperparameters
      
      - For visualization
      
      - Other kernels: polydot, tanhdot, ...?
      
      - As input to clustering?
    
    - t-SNE (not for clustering input)
      
      - Tune hyperparameters
      
      - For visualization
        
        - Animate tuning
    
    - UMAP
    
    - LLE

- Model
  
  - Centroid-based
    
    - k-means
      
      - As a clustering
      
      - As a method to tune k
      
      - Hierarchical clustering
  
  - Density-based
    
    - DBSCAN
    - OPTICS
  
  - Connectivity-based
    
    - Hierarchical clustering
    - BIRCH
  
  - Spectral clustering
  
  - Deep clustering
  
  - Ensemble clustering

- Reorganizing Code
  
  - 

- Questions
  
  - Better to use summary stat, full data, or something else?
  
  - Several approaches: which is best?
    
    - No selection --> dimension reduction --> cluster
    
    - Selection --> dimension reduction --> cluster
    
    - Selection (many) --> cluster (high dimensional methods)
    
    - Selection (few) --> cluster
  
  - Cluster city-years instead of using temporal information as lag variables
  
  - Inclusion of many variables for a given topic weighs it in the clustering