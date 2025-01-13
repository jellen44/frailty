# Predicting Development of Frailty Among Middle-Aged and Older Adults Using Machine Learning

## Overview
This repository contains the code and analysis for the study **"Predicting Development of Frailty Among Middle-Aged and Older Adults Using Machine Learning."** We developed a machine learning model to predict the onset of frailty using data routinely collected in primary care. The study used data from Waves 1 and 2 of the Survey of Health, Aging, and Retirement in Europe (SHARE).  

### Key Features
- **Objective:** Predict frailty onset using EMR-compatible data.  
- **Dataset:** SHARE-FI data, Waves 1 (2004) and 2 (2006/2007).  
- **Model:** Artificial Neural Network (ANN).  
- **Outcome:** Binary frailty status (frail vs. pre-frail/non-frail).

## Getting Started
### Prerequisites
The codebase relies on R and Python for data preprocessing, analysis, and model implementation. Ensure you have the following libraries installed:

**R Libraries:**
- `tidyverse`
- `haven`
- `ggplot2`
- `dplyr`

**Python Libraries:**
- `pandas`
- `numpy`
- `keras`
- `tensorflow`
- `scikit-learn`

### Files
- `Final_SHARE_Preprocessing.R`: Preprocesses the SHARE dataset for analysis.
- `Final_Frailty_Modeling.ipynb`: Implements the ANN for predicting frailty.

### Running the Code
1. **Preprocess Data:**  
   Run `Final_SHARE_Preprocessing.R` in R to clean and prepare the dataset.
   
2. **Train and Test Model:**  
   Use `Final_Frailty_Modeling.ipynb` in Python to train and test the ANN. 

## Contact
For inquiries or collaboration opportunities, please contact the corresponding author:  
**Jacob Ellen, MSc**  
[jellen@hms.harvard.edu](mailto:jellen@hms.harvard.edu)  
