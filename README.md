# OpenAlexRFSCI2025
This repository includes all the code and instructions needed to analyze OpenAlex data as explained in the FSCI 2025 workshop "Assessing Institutional Research Data Using OpenAlex."

This project includes the following code files:
1. RCode_Practice - This file is just intended for the FSCI workshop and is not needed to analyze any OpenAlex data.
2. RCode_OpenAlexR - This file includes code for pulling data from the OpenAlex API and then to clean the data to enable it to be saved as a flat spreadsheet. This code should be run before any of the following code files.
3. RCode_OpenAlexR - This file is the exact same code as RCode_OpenAlexR but is for participants to practice on and change as needed for their institutions.
4. RCode_AllArticles_OAAnalysis - This file will create two tables - one showing total counts and one showing percentages - of journal articles by publishers for all articles where the corresponding author came from a particular institution. It will then create a bar chart to visualize this.
5. RCode_GrantArticles_OAAnalysis - This file contains almost the exact same code as RCode_AllArticles_OAAnalysis except that it focuses on articles that OpenAlex has identified has being grant funded.
6. RCode_NoGrantArticles_OAAnalysis - This file contains almost the exact same code as RCode_AllArticles_OAAnalysis except that it focuses on articles that OpenAlex has identified has NOT being grant funded.
