## A Quarto Manuscript Template

This is my template repo for generating a manuscript from Quarto. I am using examples from the 
[Quarto Manuscripts Tutorial](https://quarto.org/docs/manuscripts/authoring/rstudio.html), but I have
added my own changes to align with how I would like to start my projects. 

### Some useful folders

You can potentially do everything in the index.qmd file, but I have some other folders that I use to help me stay organized. 

- **1_data:** This is to put any data used in your project. 

**1_images:** I added this folder to store any images you might need to put in your paper

**1_tables:** This can be used to store excel/csv files that are used for creating tables. 
(Remember, many tables will be created in the qmd files as you analyze the data, but sometimes you might have a csv that exists solely to be the table that you don't want to get confused with your data files)

**notebooks:** This is where you store qmd notebooks. For details, see [Quarto Manuscripts: RStudio](https://quarto.org/docs/manuscripts/authoring/rstudio.html)

**R:** Any R scripts that aren't part of your notebooks folder. You might not need extra r scripts, but here is a spot to put them just in case. 
