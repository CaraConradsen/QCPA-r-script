# QCPA-r-script
Tailored `R` scripts enabling the extraction and compilation of batch QCPA output data into .csv tables that can be used for downstream analysis in `R`.
This R script is a companion script for:
> van den Berg, C.P. et al. 2023 Automated workflows using Quantitative Colour Pattern Analysis (QPCA) A guide to batch processing and downstream data analysis
## Data
Data used in this example is accessible [here](https://doi.org/10.48610/3cdcc1f). To account for varying data download capacities, particularly for users with limited data caps, bandwidth, or unstable connections, we offer two options for downloading data. 
### Option 1
For users that do not have limited downloading abilities and/or have skipped the QCPA batch scripting step:
1.	Download the Worked_example_data_Processed.zip (301.66 MB)
2.	Extract the data into your working folder
3.	Rename the unzipped folder to “Worked_example_data”
4.	Use the `QCPA_r-script.R` following either the [pdf manual](https://github.com/CaraConradsen/QCPA-r-script/blob/gh-pages/QCPA_r-script.pdf) or [html manual](https://caraconradsen.github.io/QCPA-r-script/QCPA_r-script.html)
### Option 2
For users with limited data, we recommend downloading the unprocessed data and implementing the batch QCPA workflow. Take these steps:
1.	Download the Worked_example_data_Unprocessed.zip (150.61 MB)
2.	Implement the [QCPA-batch-script]( https://github.com/cedricvandenberg/QCPA-batch-script) following the [pdf manual]( https://github.com/cedricvandenberg/QCPA-batch-script/blob/main/Manual%20v1.0.pdf)
3.	Rename the folder to “Worked_example_data”
4.	On the subsequent data, use the `QCPA_r-script.R` following either the [pdf manual](https://github.com/CaraConradsen/QCPA-r-script/blob/gh-pages/QCPA_r-script.pdf) or [html manual](https://caraconradsen.github.io/QCPA-r-script/QCPA_r-script.html)
## Authors
[Cara Conradsen]( https://github.com/CaraConradsen), [Cedric van den Berg](https://github.com/cedricvandenberg), [Tom White]( https://github.com/thomased)
