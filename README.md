## Airline-delay-data

# About Datasets
The dataset includes 22-year airline flights from 1987 to 2008 saved in csv files by year on the American Statistical Association website. http://stat-computing.org/dataexpo/2009/the-data.html.
The data include commercial flight information for those carriers with at least 1% of domestic U.S. flights in a given year.
In total, there is information for over 120 million flights. Each flight has 29 variables including year, month, arrival delay, departure delay, origin, distance and others. Detailed information can be found on the website above.

# The UNIX Shell
Combining each of the CSV files into a single file is accomplished by UNIX Shell. 
Corresponding commands can be found in CombineFiles.sh.

# SQL Database with R
SQLite is used here to store 12GB airline data. 
Data is extracted by SQL commands invoked within R.
All interactions with the database are done in R with the RSQLite package.
All the detailed data analysis can be found in AirlineDelayAnalysis.R.
All the related graphs can be found in the folder named Graphs.


