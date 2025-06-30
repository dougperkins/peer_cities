
import streamlit as st

st.title('Plot from R in Streamlit')

with open('./html/kmeans_pca_3d.html', 'r', encoding='utf-8') as f:
    html = f.read()

st.components.v1.html(html, height=600, scrolling=True)

