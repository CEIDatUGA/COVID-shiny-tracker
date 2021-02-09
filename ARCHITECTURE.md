
# File Layout

All data cleaning code is saved in `data_cleaning_lib.R`, which only
contains functions. Data can be cleaned on-the-fly with these functions,
but most often the lib is used by `data_clean.R`, which saves a clean_data
file to the `data/` folder with today's date. This clean_data file is
updated every few hours with a cron job. `app.R` then prefers using this 
clean_data file. 

# Diagram

```
       --   data_clean.R --> clean_data
      /                        /
lib --                        /     
      \
       ---------         app.R  ----> Rshiny ----> rendered page

``` 



