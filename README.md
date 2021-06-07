# rmis-data
Chinook and coho data from RMIS, joined by releases / recoveries and statistical areas, etc
File 00 merge_releases_recoveries.R created by E. Ward to merge RMIS release and recoveries. 
File 01 process spring chinook data.R takes RMIS data filters it to focal species using a file Ole provided (in script), joins in our ocean region codes updated to NMFS Stat Area and joins in the Snoutbase recovery information. Saves updated data at rmis_release_recovery.csv
