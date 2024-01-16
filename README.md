# QCPA-r-script
Tailored R script guiding novice R users in the extraction and compilation of batch QCPA output data into .csv tables that can be used for downstream analysis in R.
This R script is a companion script for:
> van den Berg, C.P. et al. Automated workflows using Quantitative Colour Pattern Analysis (QPCA) A guide to batch processing and downstream data analysis 

Check out the _bioRXiv_ [paper](https://doi.org/10.1101/2023.02.02.526788) associated with the first version of this script (_November 16, 2023_).
## Data
Data used in this example is accessible [here](https://doi.org/10.48610/3cdcc1f) from UQ eSpace. To account for varying data download capacities, particularly for users with limited data caps, bandwidth, or unstable connections, we offer two options for downloading data. 
### Option 1
For users that do not have limited downloading abilities and/or have skipped the QCPA batch scripting step:
1.	Download the Worked_example_data_Processed.zip (_301.66 MB_)
2.	Extract the data into your working folder
3.	Rename the unzipped folder to “Worked_example_data”
### Option 2
For users with limited data, we recommend downloading the unprocessed data and implementing the batch QCPA workflow. Take these steps:
1.	Download the Worked_example_data_Unprocessed.zip (_150.61 MB_)
2.	Implement the [QCPA-batch-script]( https://github.com/cedricvandenberg/QCPA-batch-script) following the [QCPA-batch-script pdf manual]( https://github.com/cedricvandenberg/QCPA-batch-script/blob/main/Manual%20v1.0.pdf)
3.	Rename the folder to “Worked_example_data”
## Using QCPA-r-script
We would recommend downloading and using the [`QCPA_r-script.R`](https://github.com/CaraConradsen/QCPA-r-script/blob/main/QCPA_r-script.R) script in R. You can then follow either the [QCPA_R pdf manual](https://github.com/CaraConradsen/QCPA-r-script/blob/gh-pages/QCPA_r-script.pdf) or the [QCPA_R html manual](https://caraconradsen.github.io/QCPA-r-script/QCPA_r-script.html). 
### Markdown version
For users using the R Markdown version of **QCPA-r-script**, the dependency scripts are:
* `QCPA_r-script.Rmd` bibliography uses the `QCPA_r_bib.bib`
* `pdf_document` uses `legend_fix.tex` to fix figure and table legends 
* `html_document` uses `styles.css` to modify the table of contents and manual headers

## Authors
[Cara Conradsen]( https://github.com/CaraConradsen), [Cedric van den Berg](https://github.com/cedricvandenberg), [Tom White]( https://github.com/thomased)
