# City Managers
This is the GitHub repository for my research project: "Municipal Governance and Voter Turnout: Analyzing the Impacts of City Managers"  

This repo houses all tables, figures, and code used to produce the final paper with explanations for reproduction  

The full paper is published and available online here: <https://edspace.american.edu/clocksandclouds/wp-content/uploads/sites/115/2025/04/Spring-2025-Final-Publication-Digital.pdf>  

## Motivation and Methods

The goal of this study was to contribute to the broader research surrounding municipal governance structures,
and add to the existing literature on what impacts political participation on the local level. The focus
of this particular paper is to examine the role the type of government could have in encouraging or discouraging political engagement,
measured as voter turnout. Specifically, I hone in on whether or not each municipal election takes place in a city that is managed or un-managed.  

City managers are appointed officials, which dictate municipal policy, and therefore taking power out of the hands of voters, so it would be reasonable to assume that turnout would decrease as a result.
This paper tests this assumption by combining a propensity score matching approach, to handle endogeneity, and a regression analysis to determine the precise impact of city managers on turnout.
Ultimately, the nature of the data does not allow for the endogeniety problem to be fully solved through matching, but the process clarified the OLS estimation, creating a final model where I cannot reject the $H_0: \beta_{\text{Manager}}=0$.


## Data
This folder contains all the data for the project. I use a combination of Census Data from 2010 to 2022, and a local elections database compiled by Benedictis et. al.  

## Final_Project
This folder contains all the code for the project

* `Sample.R` is the data cleaning file and produces `final_contests.csv/rds`, which is used for the analysis.  
* `final_paper_Ochital.R` is all the code for the final paper, it produces and saves all figures.  
* `matching.qmd` is a testing quarto document where I build the code for the matching procedure.  
* `ref_govt310.bib`, is a .bib file containing citation information.  

## Plots

This folder contains all plots as png and tables as text files for viewing.  

## NCUR Poster
Presentation poster created for National Conference for Undergraduate Research 2025 (NCUR 2025) using `posterdown` (<https://github.com/brentthorne/posterdown>)

