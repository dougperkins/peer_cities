# Temporal Peer Cities

## Description

This project aims to help cities identify their "housing peer cities" - cities that are similar with regard to a set of variables (currently only housing-related variables). It improves upon existing tools to do so by deriving temporal versions of these variables. Cities are clustered and have their peers identified by both the current state of their housing situation, as other analyses and tools have done, and by their housing variables' trajectories over time. This prevents pairs of cities from being considered peers if, for example, they appear similar today, but one has had a continuously worsening housing crisis from 2009-2023 and the other a continuously improving housing crisis in the same period, even if they both have a similar housing situation as of 2023.

## Data

Most of the data is from the 2023, 2018, and 2013 **American Community Survey** 5-year data, accessed using their API via the tidycensus R library with a Census API key. To simplify development, the project originally followed the [process](https://www.chicagofed.org/region/peer-cities-identification-tool/about-the-peer-cities-identification-tool) used by the Federal Reserve Bank of Chicago in their Peer City Identification Tool (PCIT). The project has since been updated and includes 1955 cities, rather than the set of cities in the PCIT. The PCIT, based on an earlier project, includes 960 places across the United States that meet one of the following criteria:

- The place was incorporated, with at least 25,000 population by the 1960 census.

- The place was incorporated, with at least 50,000 population by the 2010 census.

Seven of the places in the PCIT were deincorporated by 2010, and these (all in New Jersey) use data for their presently-existing township boundaries. Ten of them are places which between 1960 and 2010 annexed all or nearly all of their county, and data for these are reported using their county boundaries. The list of these can be found in the link to the PCIT process above. For each year of ACS data, the data for the set of cities included in the PCIT is gathered from a combination of data from their "place", "township", and "county" geographies, and these geographies' data are filtered down to the PCIT city set using a CSV downloaded from the PCIT website. ACS data at the "metropolitan statistical area/micropolitan statistical area" level is also gathered for each year in order to derive the percent of metropolitan area population variable.

Data from **tigris** is additionally used (currently only city area in square miles) for the derivation of variables from ACS variables (currently only housing units per square mile). This data is similarly from 2023, 2018, and 2013, and uses the same geographies.

## How it Works

The project currently, for development purposes, uses a carefully selected subset of six distinct housing features, which become 18 features when the three years' data (lag variables) are loaded, and become 36 features when difference (temporal / "derivative" / "velocity") variables are created between them. 

The project currently scales the data and uses **PCA**, **kPCA (RBF)**, **kPCA (polynomial)**, and **UMAP** dimension reduction as preprocessing methods, and for each resulting dataset uses **k-means**, **hierarchical**, **HDBScan**, and **GMM** clustering to attempt to uncover the structure of the data. Polynomial kPCA's degree, scale, and offset hyperparameters, as well as HDBScan's MinPts hyperparameter, are tuned with grid search. For polynomial kPCA, this tuning maximizes percent of variance explained, and for HDBScan it chooses the clustering that performs the best on the largest number of 10+ internal clustering evaluation metrics. Whichever clustering method results in the best clustering, according to comparison across the same 20+ internal clustering evaluation metrics, is the one that is used for determining the peer list for that particular dimension-reduced dataset.

The dimension-reduced space and clustering are then put to use, as the cities outside of the chosen city's cluster are filtered out, and the city's nearest neighbors within dimension-reduced space are considered potential peers. Currently, each of these lists of peers are arbitrarily intersected to create a final list of peers which are both near the chosen city and are within the same cluster in every dimension-reduced space.

## How To Use

- Clone this project from https://github.com/dougperkins/peer_cities.git

- Run 1_top_level.R line by line in RStudio (this will ultimately be optional, as the streamlit tool will work independently)

Dependencies are imported at the top of top_level.R in the p_load() call

## Directory Structure

- **`references/`**  
  Stores external resources such as research papers, datasets, or documentation that provide context.

- **`reports/`**  
  Contains generated reports, such as PDFs, HTML files, or Markdown documents (currently unused)

- **`scripts/`**
  Holds the main source code for the project.
  
  - **1_top_level.R** - the main script
  
  - **acs.R** - functions for loading ACS data
  
  - **alfin.R** - functions for loading ALFIN data
  
  - **app.py** - the streamlit app
  
  - **cluster.R** - functions for clustering
  
  - **constants.R** - variable sets
  
  - **dim_red.R** - functions for dimension reduction
  
  - **eval.R** - functions for evaluating clustering
 
  - **fips.R** - functions for using the FIPS data to join the Tigris and ACS data
 
  - **pca_plot.py** - Python functions for use in the Streamlit app to dynamically generate the 3D plotly plots of dimension-reduced spaces
 
  - **peers.py** - functions for obtaining peer lists, in Python for dynamic generation of peer table in Streamlit app.
  
  - **peers.R** - functions for obtaining peer lists
 
  - **plot.R** - plotting functions - largely unused now?
  
  - **preprocess.R** - functions for preprocessing data
 
  - **streamlit.R** - for generating Streamlit
 
  - **tigris.R** - for loading Tigris data (to get city areas in sqmi for each year)
  
  - **utils.R** - utility functions and lots of miscellaneous bits that need restructuring
  
  - **validate.R** - data validation (currently unused)
 
- **`variables`**
  Contains some information about how the variables used in the PCIT can be calculated from ACS variables.

- **`data/`**
  
  Holds public data used in the project (currently no private data is used)
  
  - **`acs`** - American Community Survey data caches
  
  - **`alfin`** - Annual Survey of Local Government Finances data (for use in finance theme, implementation not complete)
  
  - **`area`** - Old city land area data, currently unused
 
  - **`city_sets`** - Sets of cities from other sources
  
  - **`fips`** - Federal Information Processing Standards codes for states/counties/places. Used to combine the Tigris and ACS data.
  
  - **`metro_areas`** - Cached tables connecting cities to metropolitan areas (currently only used for % metropolitan area population) 
  
  - **`pcit`** - Data downloaded from the PCIT
  
  - **`tigris`** - Tigris data including land area in square miles cached (currently only used for housing units per square mile) 

- **`gifs`** - Animated plots showing the chosen city among the clusters of other cities in each reduced-dimension space. For use in the Streamlit app. Currently not being used.

- **`html`** - Interactive plots in each dimension-reduced space. For use in the Streamlit app. Currently not being used, dynamic plots generated within Streamlit app.

## 
