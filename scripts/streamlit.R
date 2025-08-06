#### Landing Page ####
writeLines("
import streamlit as st
import pandas as pd
import numpy as np
from pca_plot import *
from peers import *
import glob
import os


data_paths = {
    'PCA': './data/out/data_pca.csv',
    'kPCA (RBF)': './data/out/data_kpca_rbf.csv',
    'kPCA (Poly)': './data/out/data_kpca_poly.csv',
    'UMAP': './data/out/data_umap.csv'
}

data = {
    'PCA': pd.read_csv(data_paths['PCA']),
    'kPCA (RBF)': pd.read_csv(data_paths['kPCA (RBF)']),
    'kPCA (Poly)': pd.read_csv(data_paths['kPCA (Poly)']),
    'UMAP': pd.read_csv(data_paths['UMAP'])
}

data_for_tbls = load_reduced_data()

#st.set_page_config(layout='wide')
st.set_page_config(page_title='Peer City Discovery Tool')

col1, col2, col3 = st.columns([1.5, 2, 1])  # Middle column wider

with col2:
    st.image('figures/pcdt_logo_2.png', width=500)
#st.image('./figures/pcdt_logo_1.png', width = 200)
# st.markdown(
#     '''
#            <div style='text-align: center;'>
#              <img src='./figures/pcdt_logo_1.png' alt='Logo' width='200'>
#              </div>
#              ''',
#     unsafe_allow_html=True
# )
#st.title('Temporal Housing Peer Cities')

st.markdown(
    '''
           <div style='text-align: center;'>
             <p>Peer cities are similar to each other in certain ways. This tool identifies housing peer cities not only with their present states, but with their changes over time. This means that a city with a worsening and improving housing crises are less likely to be considered peers.</p>
             </div>
             ''',
    unsafe_allow_html=True
)
#st.write('Peer cities are similar to each other in certain ways. This tool identifies housing peer cities not only with their present states, but with their changes over time. This means that a city with a worsening and improving housing crises are less likely to be considered peers')

city_df = pd.read_csv('./data/out/dissim.csv')  # must have a column 'City'
cities = sorted(city_df['City'].dropna().unique().tolist())

#default_city = 'Somerville city, Massachusetts'
#default_index = cities.index(default_city) #if default_city in cities else 0  # fallback to 0

city_choice = st.selectbox(
    'Choose a city',
    sorted(cities),
    index=0,
    #index=default_index, # for 960
    placeholder='Somerville city, Massachusetts',
)

# st.image('./gifs/pca/3d_spin_z.gif')

# OLD TABLE:
# tables = {
#     'Overall': pd.read_csv('./data/out/dissim.csv'),
#     'PCA': pd.read_csv('./data/out/peers_pca.csv'),
#     'kPCA (RBF)': pd.read_csv('./data/out/peers_kpca_rbf.csv'),
#     'kPCA (Poly)': pd.read_csv('./data/out/peers_kpca_poly.csv'),
#     'UMAP': pd.read_csv('./data/out/peers_umap.csv')
# }
# 
# tab_list = st.tabs(list(tables.keys()))
# 
# for i, key in enumerate(tables.keys()):
#     with tab_list[i]:
#         st.subheader(key)
#         st.dataframe(tables[key])
        
if city_choice:
    peers = get_peers(data_for_tbls, city_choice)

    if not peers:
        st.warning('No peers found for city.')
    else:
        # Combine similarity rankings
        aggregated_df = combine_similarity_rankings(peers, agg_method='mean')

        # Create tabs
        all_tabs = ['aggregated'] + list(peers.keys())
        tabs = st.tabs(all_tabs)
        
        # Aggregated tab first
        with tabs[0]:
            st.write(f'**Aggregated dissimilarity (z-scored & averaged)** for `{city_choice}`:')
            st.dataframe(aggregated_df[[ 'City', 'Aggregated' ]])
        
        # Then each method
        for i, method in enumerate(peers, start=1):
            with tabs[i]:
                st.write(f'Peers for `{city_choice}` using **{method.upper()}**:')
                st.dataframe(peers[method])
        
# col1, col2 = st.columns(2)

# with col1:
#     st.subheader('k-Means PCA')
#     with open('./html/pca_best_3d.html', 'r', encoding='utf-8') as f:
#         html1 = f.read()
#     st.components.v1.html(html1, height=600, scrolling=True)

# with col2:
#     st.subheader('Kernel PCA')
#     with open('./html/kpca_best_3d.html', 'r', encoding='utf-8') as f:
#         html2 = f.read()
#     st.components.v1.html(html2, height=600, scrolling=True)

# plots = {
#   'PCA': './html/pca_best_3d.html',
#   'kPCA (RBF)': './html/kpca_best_3d.html',
#   'kPCA (Poly)': './html/kpca_poly_best_3d.html',
#   'UMAP': './html/umap_best_3d.html'
# }
# 
# # Create tabs
# tab_list = st.tabs(list(plots.keys()))
# 
# # Load and display each plot in its corresponding tab
# for i, key in enumerate(plots):
#     with tab_list[i]:
#         st.subheader(key)
#         with open(plots[key], 'r', encoding='utf-8') as f:
#             html_content = f.read()
#         st.components.v1.html(html_content, height=600, scrolling=True)

# Tab names (can be changed as needed)
tab_names = ['PCA', 'kPCA (RBF)', 'kPCA (Poly)', 'UMAP']
tabs = st.tabs(tab_names)

# Define what happens in each tab
with tabs[0]:  # PCA
    st.subheader('PCA')
    render_pca_3d_plot(data_paths['PCA'], city_choice)

with tabs[1]:  # kPCA (RBF)
    st.subheader('kPCA (RBF)')
    render_kpca_3d_plot(data_paths['kPCA (RBF)'], city_choice, title='3D kPCA (RBF) Plot')

with tabs[2]:  # kPCA (Poly)
    st.subheader('kPCA (Poly)')
    render_kpca_3d_plot(data_paths['kPCA (Poly)'], city_choice, title='3D kPCA (Poly) Plot')

with tabs[3]:  # UMAP
    st.subheader('UMAP')
    render_kpca_3d_plot(data_paths['UMAP'], city_choice, title='3D UMAP Plot')

st.write('by Douglas Perkins')

#render_pca_3d_plot('./data/out/data_pca.csv', city_choice)
#render_kpca_3d_plot(csv_path = './data/out/data_umap.csv', selected_city = city_choice)

", "./scripts/app.py")

#### Page 1: Basic ####
# writeLines("
# import streamlit as st
# import pandas as pd
# 
# st.title('Temporal Housing Peer Cities')
# 
# 
# 
# ", "./scripts/pages/01_Peers.py")

#### Page 1: Advanced ####

writeLines("
import streamlit as st
import pandas as pd

st.title('Advanced')

# col1, col2 = st.columns(2)
# 
# with col1:
#     st.subheader('KMeans PCA')
#     with open('./html/pca_best_3d.html', 'r', encoding='utf-8') as f:
#         html1 = f.read()
#     st.components.v1.html(html1, height=600, scrolling=True)
# 
# with col2:
#     st.subheader('Kernel PCA')
#     with open('./html/kpca_best_3d.html', 'r', encoding='utf-8') as f:
#         html2 = f.read()
#     st.components.v1.html(html2, height=600, scrolling=True)
# 
# st.image('./gifs/pca/3d_spin_z.gif')
# 
# tables = {
#     'PCA': pd.read_csv('./data/out/peers_pca.csv'),
#     'kPCA (RBF)': pd.read_csv('./data/out/peers_kpca_rbf.csv'),
#     'kPCA (Poly)': pd.read_csv('./data/out/peers_kpca_poly.csv')
# }
# 
# tab_list = st.tabs(list(tables.keys()))
# 
# for i, key in enumerate(tables.keys()):
#     with tab_list[i]:
#         st.subheader(key)
#         st.dataframe(tables[key])

", "./scripts/pages/01_Advanced.py")

#### Page 2: About ####

writeLines("
import streamlit as st
import pandas as pd

st.title('About')

# col1, col2 = st.columns(2)
# 
# with col1:
#     st.subheader('KMeans PCA')
#     with open('./html/pca_best_3d.html', 'r', encoding='utf-8') as f:
#         html1 = f.read()
#     st.components.v1.html(html1, height=600, scrolling=True)
# 
# with col2:
#     st.subheader('Kernel PCA')
#     with open('./html/kpca_best_3d.html', 'r', encoding='utf-8') as f:
#         html2 = f.read()
#     st.components.v1.html(html2, height=600, scrolling=True)
# 
# st.image('./gifs/pca/3d_spin_z.gif')
# 
# tables = {
#     'PCA': pd.read_csv('./data/out/peers_pca.csv'),
#     'kPCA (RBF)': pd.read_csv('./data/out/peers_kpca_rbf.csv'),
#     'kPCA (Poly)': pd.read_csv('./data/out/peers_kpca_poly.csv')
# }
# 
# tab_list = st.tabs(list(tables.keys()))
# 
# for i, key in enumerate(tables.keys()):
#     with tab_list[i]:
#         st.subheader(key)
#         st.dataframe(tables[key])

", "./scripts/pages/02_About.py")

# df = pd.read_csv('./data/out/peers_pca.csv')
# st.title('PCA Peers Data Table')
# st.dataframe(df)