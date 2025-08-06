
import streamlit as st

st.title('Plot from R in Streamlit')

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

df = pd.read_csv('my_dataframe.csv')
st.title('Data Table from R')
st.dataframe(df)

st.components.v1.html(html, height=600, scrolling=True)

