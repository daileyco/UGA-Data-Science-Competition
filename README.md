# UGA-Data-Science-Competition

This repository contains the files for the University of Georgia Data Science Competition in partnership with Wells Fargo. See ["./Information"](https://github.com/daileyco/UGA-Data-Science-Competition/tree/main/00-Information) for more detailed documents. 

This `README` document outlines the team members, the organization of the repository, overall project workflow, and minutes from team meetings. 






## Team Members & Affiliations  
  
[Cody Dailey](https://daileyco.github.io/), Department of Epidemiology and Biostatistics  
[Ishaan Dave](https://github.com/ishaandave), Department of Epidemiology and Biostatistics  
[Yang Ge](https://yangepi.github.io/), Department of Epidemiology and Biostatistics  
[Vipul Shinde](https://github.com/Vipul-15), Department of Computer Science  
  
  
  
  
  
  
## Directory Structure  

00-Information  
  
00-References  
- 01-Finance-and-Credit  
- 02-Machine-Learning  
- 03-Miscellaneous  
  
01-Data  
- 00-Raw  
- 01-Clean  
- 02-Final
  
02-Data-Management  
  
03-Exploratory-Analyses  
  
04-Data-Visualization  
- 01-Tables
- 02-Figures
- 03-Non-Static
  
05-Model-Fitting  
- 01-Logistic-Regression
- 02-Machine-Learning
- 03-Comparisons
  
06-Report  
- 01-Written
- 02-Presentation
  
  
README.md
  
  
  
  
  
  
## Task List  
  
>**1. Collaboration Orientation - March 25 @ 5PM**  
2. Independent Exploration with Item Analysis
3. Data Management, Pre-processing, & Cleaning (Create Data Dictionary with Comments / Same Script)   
4. Independent Literature Review  
>**5. Planning Meeting - March 30 @ 9-11AM**  
6. Data Management, Pre-processing, & Cleaning II  
7. Cooperative Interrogation  
8. Data Management, Pre-processing, & Cleaning III  
>**9. Planning Meeting - April 2 @ 4PM**  
10. Model Fitting  
11. Diagnostics  
12. Estimation & Inference  
>**13. Planning Meeting - April 9 @ 3-4PM**  
14. Interpretation & Conclusions  
15. Reflection & Review  
16. Communication  
>**17. Final Meeting - April 13-14 TBD**  






## Running Questions  
- Outside data sources?
- Will this repo suffice as a deliverable for "the codes for analysis, with brief but adequate annotations"? 






## Meeting Notes

### March 25

- Discussed competition deliverables, team member competencies, general direction / project workflow, and organization structure
- Suggested resources to augment Machine Learning knowledge
- Agreed on using R/RStudio for analysis and write-up, perhaps integrate Python if needed (maybe reticulated?)
- Set meeting dates and times to keep on track
- Improved workflow task list
- [Agenda needed for other meetings?]

#### To-do before next meeting
Each team member will independently explore the data / project to get a sense of contents and context. Next meeting we will discuss our findings, concerns and directions to pursue. 



### March 30

- Discussed three datasets given: training, validation, testing
  - determined should focus on training for exploratory data analysis in write-up, but could include tables for others in supplement
  - remarked on how objective is for determining credit approvals
    - data is for those who were **aproved**, missing non-approvals / declined
    - modeling would essentially be identifying those who shouldn't have been approved (i.e., a false positive in a sense)
- Noted distributions in training data
  - clean because artifact of simulation?
  - low relative frequency of defaults
  - groupings of related variables, how best to address?
- Exploratory data analysis
  - constructing descriptive tables stratified by default (generic Table 1)
  - correlation matrix among predictors
    - using different metrics depending on data types
  - other figures for variables with respect to groupings (e.g., credit debt)
- Thinking forward about model fitting
  - defining objective function options to help with class imbalance
  - using a "cost" function
    - defining a "cost" associated with predictions
    - fixed v dynamic "cost" definition
  - comparing model fits with "cost"
    - how to estimate distribution of "cost", confidence intervals?
  - how would we incorporate the validation data into logistic regression model fitting?
    - Bayesian framework?

#### To-do before next meeting

Each team member has been tasked with specific components of project. **Yang** will create some figures to describe variables with respect to the broader groupings. **Vipul** will initiate the background of the write-up with particular attention to descriptions and importance of the variables included in the data. **Ishaan** and **Cody** will create descriptive tables, calculate "correlations" for an association matrix, and further explore the "cost" function. 
