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

# How to use These Tools

There is a subfolder within this repo named "functions" which is full of, well, scripts that contain functions! You'll notice they're often called (via source()) at the top of the example scripts. This loads in the custom functions that I have written to handle CTT data. Ultimately, these will be rolled into an R package.

## Functions

### Data Manager

#### load_data(infile)

The input folder can contain any melange of raw downloaded files from the sensor station (beep data, node health, GPS) all in the same folder or subfolders. Zipped folders need to be unzipped, but compressed files do not (i.e. csv.gz files are just fine as they are). The function will return a list of 3 dataframes from the files in the folder you give it:  

1. beep data  
2. node health  
3. GPS  

### Node Health

#### node_channel_plots(health, freq)
This function is the "engine" behind the export function. You can run it standalone with the following parameters, but you don't have to.  

health: the 2nd dataframe output by the load_data() function  
freq: the time interval for which you want variables to be summarized  

The output is a nested list for each combination of channel and node, with the following plots for each:  

1. battery  
2. RSSI  
3. number of check-ins  
4. scaled number of check-ins as line plot over scaled RSSI  
5. box plot of node RSSI

#### v2_plots(health, freq)
health: the 2nd dataframe output by the load_data() function    
freq: the time interval for which you want variables to be summarized  

The output is a nested list for each combination of channel and node, with the following plots for each:  

1. latitude  
2. longitude  
3. RSSI  
4. dispersion  

#### node_plots(health, nodes, freq)
NOTE: THIS ONLY WORKS FOR V2  

health: the 2nd dataframe output by the load_data() function  
nodes: list of nodes  
freq: the time interval for which you want variables to be summarized  

The output is a nested list for each node, with the following plots for each:  

1. RSSI  
2. number of check-ins  
3. battery  
4. time mismatches  
5. small time mismatches  

#### gps_plots(gps, freq)
gps: the 3rd data frame from the load_data() function  
freq: the time interval of summary  

1. altitude
2. number of fixes

#### export_node_channel_plots(health_data, freq, out_path, x, y, z)
health_data: the 2nd dataframe output by the load_data() function  
freq: the time interval for which you want variables to be summarized  
out_path: where you want your plots to go  
x: the plot for the 1st panel  
y: the plot for the 2nd panel  
z: the plot for the 3rd panel  

To assign x, y and z, look at the description for node_channel_plots() and select those plot indices in the order you want them on the page.  

#### export_node_plots(health_data, freq, out_path, x, y, z)
NOTE: THIS ONLY WORKS FOR V2  
same as above; indices for the plots can be chosen from the list under the node_plots() description  

## Example Scripts

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
