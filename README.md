# OpenAlexRFSCI
This repository includes all the code and instructions needed to analyze OpenAlex data as explained in the FSCI workshop "Assessing Institutional Research Data Using OpenAlex." This workshop was first offered as part of FSCI 2025 and is being offered agains as part of FSCI's Charleston Conference Asia 2026.

This project includes the following code files:
1. RCode_Practice - This file is just intended for the FSCI workshop and is not needed to analyze any OpenAlex data.
2. RCode_OpenAlexR_Primary - This file includes code for pulling data from the OpenAlex API and then to clean the data to enable it to be saved as a flat spreadsheet. This code should be run before any of the following code files.
3. RCode_OpenAlexR_Working - This file is the exact same code as RCode_OpenAlexR but is for participants to practice on and change as needed for their institutions.
4. RCode_OAAnalysi_AllArticless - This file will create two tables - one showing total counts and one showing percentages - of journal articles by publishers for all articles where the corresponding author came from a particular institution. It will then create a bar chart to visualize this.
5. RCode_OAAnalysis_GrantArticles - This file contains almost the exact same code as RCode_AllArticles_OAAnalysis except that it focuses on articles that OpenAlex has identified has being grant funded.
6. RCode_OAAnalysis_NoGrantArticles - This file contains almost the exact same code as RCode_AllArticles_OAAnalysis except that it focuses on articles that OpenAlex has identified has NOT being grant funded.
7. RCode_FurtherAnalysis - This file contains code to analyze some of the nested variables that were extracted from the main dataset in the RCode_OpenAlexR_Working script.

Everything in this repository is licensed with a CC-BY-NC-4.0 license.
