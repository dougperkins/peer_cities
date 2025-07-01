
import streamlit as st
import pandas as pd

st.title('Temporal Housing Peer Cities')

col1, col2 = st.columns(2)

with col1:
    st.subheader('KMeans PCA')
    with open('./html/pca_best_3d.html', 'r', encoding='utf-8') as f:
        html1 = f.read()
    st.components.v1.html(html1, height=600, scrolling=True)

with col2:
    st.subheader('Kernel PCA')
    with open('./html/kpca_best_3d.html', 'r', encoding='utf-8') as f:
        html2 = f.read()
    st.components.v1.html(html2, height=600, scrolling=True)

st.image('./gifs/pca/3d_spin_z.gif')

tables = {
    'PCA': pd.read_csv('./data/out/peers_pca.csv'),
    'kPCA (RBF)': pd.read_csv('./data/out/peers_kpca_rbf.csv'),
    'kPCA (Poly)': pd.read_csv('./data/out/peers_kpca_poly.csv')
}

tab_list = st.tabs(list(tables.keys()))

for i, key in enumerate(tables.keys()):
    with tab_list[i]:
        st.subheader(key)
        st.dataframe(tables[key])


