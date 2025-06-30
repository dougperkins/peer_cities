## Future Development

Bolded things are more immediate goals.

- Features
  
  - Add and remove features in existing themes
    
    - Total population should come from either the decennial Census or between-year Census Bureau population estimates, *not ACS data*. Affects a number of features in PCIT.
    
    - Comparisons should only be done between *non-overlapping* 5y ACS estimates.
    
    - Housing
      
      - Very few remaining possible static ACS variables
      
      - Many remaining derived ACS variables
  
  - Temporal versions of existing features
    
    - **~~5-year differences (only 3): 2019-2023, 2014-2018, 2009-2013,~~**
      
      - Once 2024 is released, can get 4: 2005-2009 and move the rest up 1
    
    - ~~**Lag variables~~**
    
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
  
  - Misc.
    
    - Locations?
  
  - Meta
    
    - Automate grabbing new set of years when year changes (when ACS 2024 is released, will need 2014, 2019, 2024 5y datasets)

- Themes
  
  - ~~Housing~~
  - Housing Changes
  - Financial
  - General
  - Resilience (PCIT)
  - Outlook (PCIT)
  - Equity (PCIT)

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
      
      - ~~For multicollinearity~~
      
      - ~~For visualization~~
      
      - Working with low-variance PCs
    
    - kPCA
      
      - ~~Tune hyperparameters~~
      
      - ~~For visualization~~
      
      - Other kernels: ~~polydot~~, tanhdot, ...?
      
      - ~~As input to clustering?~~
    
    - t-SNE (not for clustering input)
      
      - Tune hyperparameters
      
      - For visualization
        
        - Animate tuning
    
    - UMAP
    
    - LLE

- Model
  
  - Centroid-based
    
    - k-means
      
      - ~~As a clustering~~
      
      - As a method to tune k
  
  - Hierarchical clustering
    
    - ~~Hclust~~
    
    - BIRCH
  
  - Density-based
    
    - DBSCAN
    - ~~HDBScan~~
    - OPTICS
  
  - Model-based
  
  - Spectral clustering
  
  - Deep clustering
  
  - Ensemble clustering

- Reorganizing Code

- Questions
  
  - Better to use summary stat, full data brackets, or something else?
  
  - Several approaches: which is best?
    
    - No selection --> dimension reduction --> cluster
    
    - No selection --> cluster (high dimensional methods)
    
    - Selection (many/few) --> dimension reduction --> cluster
    
    - Selection (many) --> cluster (high dimensional methods)
    
    - Selection (few) --> cluster
  
  - Cluster city-years instead of using temporal information as lag variables?
  
  - Inclusion of many variables for a given topic weighs it in the clustering

- Report generation
  
  - Generate a report for each chosen city - probably the most usable thing for city employees.
