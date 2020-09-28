Tools for managing, formatting, coalescing and exporting data from our products, including creating subsets!  

A RStudio tutorial is beyond the scope of this readme, but if you need to get started with installing R and RStudio: https://www.earthdatascience.org/courses/earth-analytics/document-your-science/setup-r-rstudio/

# How to use GitHub

1. Create an account: https://github.com
2. Work through chapters 6-12 here if you need to install git, and connect it all with RStudio: https://happygitwithr.com/install-git.html
3. Follow the instructions (at least through 5) here under "How to do this using RStudio and GitHub?" https://r-bio.github.io/intro-git-rstudio/  
  a. you don't need to enter the backticks in the shell  
  b. this example is a bit misleading because it doesn't include the .git, copy the link to the clipboard like before  
  c. RESTART RSTUDIO BEFORE MOVING ONTO STEP 6 IN THIS TUTORIAL   
4. If you want to pull updates from here to your copy, see chapter 31: https://happygitwithr.com/upstream-changes.html#pull-changes-from-upstream

# How to Use These Tools

There is a subfolder named "functions" which is full of, well, scripts that contain functions! You'll notice they're often called (via source()) at the top of the example scripts. This loads in the custom functions that I have written to handle CTT data. Ultimately, these will be rolled into an R package.

"example.R" shows you example implementations of the data management and node health functions (also read comments, functions that produce files are commented out)   
"locate_example.R" is a template script for running the location functions

Ideas for R package functions:  
load_data()  
export_data()  
summarize_health_data()  
node_plots()  
export_node_plots()  
node_channel_plots()  
export_node_channel_plots()
