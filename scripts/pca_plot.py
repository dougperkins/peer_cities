# pca_plot.py
import pandas as pd
import plotly.graph_objects as go
import streamlit as st
import plotly.io as pio
import os

def render_pca_3d_plot(csv_path: str, selected_city: str = "Somerville city, Massachusetts"):
    # Load the PCA data
    df = pd.read_csv(csv_path)
    df['cluster'] = df['cluster'].astype(str)

    # Tag the selected city
    df['highlight'] = df['City'].apply(lambda x: "highlight" if x == selected_city else "normal")
    normal_points = df[df['highlight'] == "normal"]
    highlight_point = df[df['highlight'] == "highlight"]

    # Build the figure
    fig = go.Figure()

    fig.add_trace(go.Scatter3d(
        x=normal_points['C1'],
        y=normal_points['C2'],
        z=normal_points['C3'],
        mode='markers',
        marker=dict(
            size=4,
            color=normal_points['cluster'].astype('category').cat.codes,
            colorscale='Viridis'
        ),
        text=normal_points['City'],
        name='Cities'
    ))

    fig.add_trace(go.Scatter3d(
        x=highlight_point['C1'],
        y=highlight_point['C2'],
        z=highlight_point['C3'],
        mode='markers+text',
        marker=dict(size=10, color='yellow'),
        text=highlight_point['City'],
        name='Highlighted City'
    ))

    fig.update_layout(
        title='3D PCA Plot',
        scene=dict(
            xaxis_title='PC1',
            yaxis_title='PC2',
            zaxis_title='PC3'
        ),
        margin=dict(l=0, r=0, b=0, t=40)
    )

    st.plotly_chart(fig, key = csv_path)


def render_kpca_3d_plot(csv_path: str,
                 selected_city: str = "Somerville city, Massachusetts",
                 pc_cols: list = ["C1", "C2", "C3"],
                 cluster_col: str = "cluster",
                 text_col: str = "City",
                 title: str = "3D kPCA Plot with Highlight",
                 save_path: str = None):
    """
    Load PCA/kPCA data from CSV, highlight selected city, and show interactive 3D plot in Streamlit.
    Optionally save to an HTML file.
    """

    # --- Load and validate data ---
    try:
        df = pd.read_csv(csv_path)
    except Exception as e:
        st.error(f"Failed to load CSV: {e}")
        return

    expected_cols = pc_cols + [cluster_col, text_col]
    missing_cols = [col for col in expected_cols if col not in df.columns]
    if missing_cols:
        st.error(f"Missing column(s) in data: {missing_cols}")
        return

    # --- Mark highlight column ---
    #df[text_col] = df[text_col].astype(str).str.strip()
    #selected_city = selected_city.strip()

    df["highlight"] = df[text_col].apply(lambda x: "highlight" if x == selected_city else "normal")

    # --- Split data ---
    highlight_df = df[df["highlight"] == "highlight"]
    normal_df = df[df["highlight"] != "highlight"]

    # --- Plot ---
    fig = go.Figure()

    # Normal points
    fig.add_trace(go.Scatter3d(
        x=normal_df[pc_cols[0]],
        y=normal_df[pc_cols[1]],
        z=normal_df[pc_cols[2]],
        mode='markers',
        marker=dict(
            size=4,
            color=normal_df[cluster_col].astype('category').cat.codes,
            colorscale='Viridis',
            opacity=0.6
        ),
        text=normal_df[text_col],
        name='Cities'
    ))

    # Highlighted point
    if not highlight_df.empty:
        fig.add_trace(go.Scatter3d(
            x=highlight_df[pc_cols[0]],
            y=highlight_df[pc_cols[1]],
            z=highlight_df[pc_cols[2]],
            mode='markers+text',
            marker=dict(size=10, color='yellow'),
            text=highlight_df[text_col],
            name='Highlighted City'
        ))
    else:
        st.warning(f"City '{selected_city}' not found in dataset.")

    # Layout
    fig.update_layout(
        title=title,
        scene=dict(
            xaxis_title=pc_cols[0],
            yaxis_title=pc_cols[1],
            zaxis_title=pc_cols[2]
        ),
        margin=dict(l=0, r=0, b=0, t=40)
    )

    st.plotly_chart(fig, key = csv_path)

    # Optional save
    if save_path:
        os.makedirs(os.path.dirname(save_path), exist_ok=True)
        pio.write_html(fig, file=save_path, full_html=True)
        st.success(f"Plot saved to: {save_path}")
