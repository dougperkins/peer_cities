## To Do

This is the ground truth of things to do at a granular level, as opposed to leaving TODOs in the code or leaving these things in the larger conceptual Future Development section of the readme.md

- [ ] **Clean & Structure Code**
  
  - [ ] File structure 
  
  - [ ] Function structure
    
    - [ ] All functions should have data, not some data/.../... as parameter? Or is this less clear
    - [ ] inner functions: "runner"
    - [ ] outer functions: name by what they do / interfaces. use map().
  
  - [ ] Improve hacky solutions
    
    - [ ] Geometry intersection to find associated metro area
    
    - [ ] Use of GEO instead of s2 to get percent metro area pops for problem cities
  
  - [ ] Write programmatically, not for these specific years/vars/etc.
    
    - [ ] Years and variables should be inputs to a function that runs the whole thing, should not have data-sixvar and data-allvars the whole time, just input allvars or sixvar into the first function
    
    - [x] dimension reduction methods drlist <- function("dr1", "dr2")
    
    - [ ] distance matrices dists <- function("", "")
  
  - [ ] top_level leave very abstracted
  
  - [ ] flexible years, 
  
  - [ ] flexible dim red methods
    
    - [x] baseline flexibility
    
    - [ ] grid search wrapper? or write each one
  
  - [ ] flexible cluster methods
    
    - [x]  baseline flexibility
    
    - [ ] custom index eval function to replace NbClust()
    
    - [ ] better vote/selection method for best clustering across all indices
  
  - [ ] namespace
    
    - [x] fips
      
      - [x] places
      
      - [x] state
      
      - [x] state_rows
    
    - [ ] feats
    
    - [x] all_acs_ests[[year]]
    
    - [ ] variables
    
    - [x] data
      
      - [ ] structure data - source - year or data - year - source? Either way, should only use one system rather than a weird mix of both
      
      - [ ] different organization within
    
    - [x] cluster
      
      - [x] nclust
      
      - [x] assignments
    
    - [x] peers

- [ ] **Loading Data**
  
  - [ ] Cache
    
    - [x] ACS
      
      - [ ] Why are 4 missing median year renter moved in?
      
      - [ ] Improve ACS data cacheing - don't need to reload all if one geog is missing
    
    - [ ] ACS metro areas 
      
      - [x] cache metro area pop lookups per year
        
        - [x] 2013
        
        - [x] 2018
        
        - [x] 2021
        
        - [x] 2023
    
    - [ ] ALFIN
    
    - [x] tigris
    
    - [ ] The full ACS-sqmi-%MAP per variable set? 
    
    - [ ] Use a library for this instead
      
      - [ ] It should all work for partial information too, as with the geolocation. Doesn't make sense to have to re-download a whole year's data because the variable set is different. The unit of cacheing should be a year-geog-var and should never need to be pulled from the API more than once.
  
  - [ ] Parallelize
    
    - [ ] ACS
    
    - [x] ACS metro areas
    
    - [ ] ALFIN?
    
    - [x] tigris (does by state)
  
  - [ ] Error handling
    
    - [ ] API retries - ACS, tigris
  
  - [ ] Automate
    
    - [x] tigris load / download
  
  - [ ] Get 6-var and 500+-var housing datasets
  
  - [x] is acs_metro_areas[["year"]] the same as data - raw - acs - year metros? Fix if so
  
  - [ ] metro area GEOID colname fix, along with NAME, remove metro_popM, what is the 2nd "pop" var coming from metro areas...
  
  - [ ] Did I fix whatever was causing the nonsense populations in big cities/etc? Double check with some big cities, NYC was having the problem. I think it may have been bind_rows or cols?

- [ ] **Preprocessing**
  
  - [ ] Log-transform variables?
  
  - [x] Use several dimension reduction techniques
  
  - [ ] PCA prior to other DR?

- [ ] **Clustering**
  
  - [ ] Use diceR consensus clustering (km, hc, hdb)
  
  - [ ] Get consensus clusters with raw and dimred

- [ ] **Output Peer Lists**
  
  - [ ] Get consensus peer lists with raw and dimred 

- [ ] **Display Webpage**
  
  - [ ] DR plots
    
    - [x] Interactive (hover to see city, chosen city always shown)
    
    - [ ] 2d
    
    - [x] 3d
    
    - [ ] (Adv) Tabs to see different DR 
    
    - [ ] (Adv) Tabs to see if clustering on low-var PCs, option to try those if needed.
    
    - [x] Color by cluster
    
    - [ ] Static clustering animated changing over time points
  
  - [ ] Peer lists / tables / images

- [ ] **Automating Manual Steps (automate, log, notify as needed)**
  
  - [ ] finding best set of components to cluster on 
    
    - [ ] May require actually clustering on them?
    
    - [ ] ggpairs plots save
  
  - [ ] pre-clustering checks
    
    - [ ] hopkins (print, stop if particularly bad?)
    
    - [ ] pairwise distance plot save, print modality, highlight it if good
    
    - [ ] pulling the necessary # of PCA components
  
  - [ ] pre-processing
    
    - [ ] transformations of variables based on summary stats
  
  - [ ] other plots:
    
    - [ ] corrplots
    
    - [ ] scree
    
    - [ ] other fviz
