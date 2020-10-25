## Notes for function for merge linelists

## to make reviewing easier:
## copy layout of merge_m_and_e.R
## try and stick to google code style guidelines (can use the {styler} package)
## comment everything (even if it seems ridiculous)

## steps:
## read in all file names in folder
## extract the first 3 letters from filename, save as vector obj
## drop those that are AFRO (maybe chuck a warning so that people deal with them?)
## Drop things like pdfs and word docs
## read in dictionary (will need all three sheets)
## start a for-loop for each country
## if statement for whether ISOcode is template or not based on dictionary
## simple recode for template conform datasets
## else for non templates, individual recodes for each country (i.e. individual if statements)
## if doesnt have all variables in dictionary var_standards, add in cols as NA
## only keep the cols from var_standards and re-order correctly
## bind_rows
## recode based on franck's dictionary (will need to be cleaned and added to)
