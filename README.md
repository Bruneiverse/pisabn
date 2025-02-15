
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pisabn 🇧🇳

<!-- badges: start -->
<!-- badges: end -->

This project provides R scripts that downloads the latest PISA data (SAS
files from the OECD website), reads it into R using the `{haven}`
package, and filters the data specifically for Brunei. The processed CSV
file is also provided for researchers interested in conducting analyses
such as IRT and latent trait regression.

## Usage

If you would like to replicate the data processing steps, you can follow
the instructions below. Prerequisites:

- R (version 4.0 or higher recommended)
- R packages: `haven`, `tidyverse` (install them using
  `install.packages("haven")` and `install.packages("tidyverse")`)
- A stable internet connection!

Then, follow these steps:

1.  Clone this repository:

    ``` bash
    git clone https://github.com/yourusername/pisabn.git
    ```

2.  Open the project in RStudio or your preferred R environment.

3.  Run the scripts in the `R/` directory in order, to initiate the
    download and processing of the PISA data.

## Data description

The PISA data is divided into several files, each containing different
types of data. The data files are as follows:

1.  `CY08MSP_CRT_COG` (Creative Thinking data)
2.  `CY08MSP_FLT_COG` (Financial Literacy data)
3.  `CY08MSP_FLT_QQQ` (Financial Literacy questionnaire data)
4.  `CY08MSP_FLT_TIM` (Financial Literacy timing data)
5.  `CY08MSP_SCH_QQQ` (School questionnaire data)
6.  `CY08MSP_STU_COG` (Student cognitive item data)
7.  `CY08MSP_STU_QQQ` (Student questionnaire data)
8.  `CY08MSP_STU_TIM` (Student questionnaire timing data)
9.  `CY08MSP_TCH_QQQ` (Teacher questionnaire data)

To learn more about the data, consult the downloaded codebook
`CY08MSP_CODEBOOK_27thJune24.xlsx`.
