import streamlit as st

# Set the page layout
st.set_page_config(layout="centered")

# Create tabs
tabs = st.tabs(["Peer Cities", "Leaderboard", "Methodology", "About"])

# Peer Cities tab content
with tabs[0]:
    st.header("Peer Cities")
    city = st.text_input(
        "Enter a city:",
        key="peer_city",
        help="Type the name of a city to find its peers.",
    )
    if city:
        st.write(f"You entered: {city}")

# Leaderboard tab content
with tabs[1]:
    st.header("Leaderboard")
    st.write("This is the leaderboard tab.")

# Methodology tab content
with tabs[2]:
    st.header("Methodology")
    st.write(
        """
        The methodology used to identify peer cities involves analyzing various 
        socioeconomic, demographic, and geographic factors. Data is sourced from 
        reliable public datasets, and clustering algorithms are applied to group 
        cities with similar characteristics.
        """
    )

# About tab content
with tabs[3]:
    st.header("About")
    st.write(
        """
        This application is designed to help users explore peer cities based on 
        shared characteristics. It provides insights into urban similarities and 
        differences, enabling better decision-making and collaboration.

        This was built by Douglas Perkins and the SomerStat group at the City of Somerville, MA, USA,
        as part of the Northeastern University M.S. in Data Science program's capstone project.
        """
    )
