import glob
import os
import pandas as pd
import numpy as np
from functools import reduce
from scipy.stats import zscore

def add_distance_to_chosen(df, city_col="City", reference_city="Somerville city, Massachusetts"):
    # Identify numeric columns
    numeric_cols = df.select_dtypes(include=np.number).columns.tolist()
    
    # Extract reference row's numeric vector
    ref_row = df[df[city_col] == reference_city]
    if ref_row.empty:
        raise ValueError(f"City '{reference_city}' not found in data.")
    
    chosen_city_vec = ref_row[numeric_cols].iloc[0].values

    # Compute Euclidean distance for each row
    df['Dissimilarity'] = df[numeric_cols].apply(
        lambda row: np.sqrt(np.sum((row.values - chosen_city_vec) ** 2)), axis=1
    )
    
    return df
  
def get_peers(data, chosen_city, city_col="City"):
    if chosen_city is None:
        raise ValueError("Please specify a city")
    
    peers = {}
    
    for method, method_data in data['reduced'].items():
        df = method_data['six_vars']
        
        # Row of chosen city
        city_row = df[df[city_col] == chosen_city]
        if city_row.empty:
            continue  # Skip this method if city is not found
        
        # Cluster assignment
        city_cluster = city_row['cluster'].iloc[0]
        
        # Members in the same cluster
        cluster_members = df[df['cluster'] == city_cluster].copy()
        
        # Add dissimilarity
        cluster_with_dist = add_distance_to_chosen(cluster_members, city_col, chosen_city)
        
        peers[method] = cluster_with_dist[[city_col, 'Dissimilarity']].sort_values(by='Dissimilarity')
    
    return peers

def load_reduced_data(csv_folder="./data/out", suffix="_six_vars.csv", city_col="City"):
    reduced = {}
    csv_files = glob.glob(os.path.join(csv_folder, "*.csv"))

    for csv_file in csv_files:
        method = os.path.basename(csv_file).replace("data_", "").replace(".csv", "")
        df = pd.read_csv(csv_file)
        if city_col not in df.columns or 'cluster' not in df.columns:
            continue  # Skip if critical columns are missing
        reduced[method] = {'six_vars': df}

    return {'reduced': reduced}

def combine_similarity_rankings(peers_dict, method_names=None, agg_method="mean", city_col="City"):
    if method_names is None:
        method_names = list(peers_dict.keys())

    norm_dfs = []

    for method in method_names:
        df = peers_dict.get(method)
        if df is None or city_col not in df.columns or "Dissimilarity" not in df.columns:
            raise ValueError(f"Each peers[{method}] must have '{city_col}' and 'Dissimilarity' columns")

        df = df.copy()
        df = df[[city_col, "Dissimilarity"]].dropna()

        # Normalize dissimilarity
        df[method] = zscore(df["Dissimilarity"].astype(float))
        df = df[[city_col, method]]
        norm_dfs.append(df)

    # Merge on city
    combined = reduce(lambda left, right: pd.merge(left, right, on=city_col, how='outer'), norm_dfs)

    # Aggregate
    if agg_method == "mean":
        combined["Aggregated"] = combined[method_names].mean(axis=1, skipna=True)
    elif agg_method == "median":
        combined["Aggregated"] = combined[method_names].median(axis=1, skipna=True)
    else:
        raise ValueError("Unsupported aggregation method. Use 'mean' or 'median'.")

    return combined.sort_values("Aggregated").reset_index(drop=True)
