# Guatemala Household Poverty Analysis

This repository contains the full workflow of a micro-level poverty analysis project using the **ENCOVI 2000** dataset from Guatemala. The goal is to build reliable poverty prediction models by integrating survey-weighted regression and machine learning, with a strong emphasis on data cleaning, index construction, and variable engineering.

## Repository Structure

```
guatemala-poverty-analysis/
├── cleaned_data/              # Final cleaned datasets used in modeling
├── intermediate_data/         # Intermediate merged/transformed files
├── metadata/                  # Variable dictionary and category mapping
├── raw_data/                  # Original LSMS (ENCOVI) survey files (.dta)
├── docs/                      # Records of data cleaning decisions and logs
├── scripts/                   # Code scripts for analysis
│   ├── aggregate.ipynb        # Python notebook for data aggregation and cleaning
│   └── guatemala_household_analysis.R  # Full R analysis workflow
```

> *Note: This project uses both R and Python for data processing and analysis.*

## Key Components

### Data Cleaning & Integration
- Harmonizes over 10+ module files from the LSMS survey.
- Handles household, community, and infrastructure layers.
- Applies survey weights for representativeness.

### Index Construction
Constructed weighted indices using PCA, MCA, K-means, and FAMD:
- `Equipment Index`: Material asset ownership (PCA)
- `Housing Quality Index`: Housing structure (MCA)
- `Energy Use Index`: Household energy sources (MCA)
- `Agricultural Cluster`: Engagement in livestock/land use (K-means)
- `Community Infrastructure Index`: Access to facilities (PCA)
- `Education Index`: Composite of literacy, schooling levels, and enrollment (FAMD)
- `Demographic Ratios`: Dependency, gender power, and social margin
- `livelihood activities`: work situation, ownership of businesses
- `geographic indicators`: urban/rural

### Modeling
#### Linear Regression (Survey-weighted)
- Predicts daily per capita household consumption (USD PPP 2017)
- `R² ≈ 56.1%`
- Key drivers: Equipment index, education, energy access, dependency ratio

#### Logistic Regression (Survey-weighted)
- Binary poverty prediction using $2.15/day threshold
- ROC AUC: `0.89`

#### Random Forest (Weighted)
- Predicts poverty classification with high performance
- ROC AUC: `0.988`
- Top predictors: equipment, dependency ratio, housing, energy

## Requirements

Install R packages before running analysis:

```r
install.packages(c("tidyverse", "survey", "FactoMineR", "factoextra",
                   "randomForest", "ranger", "pROC", "ggplot2",
                   "caret", "sjPlot", "fastDummies", "olsrr"))
```

## Citation

- **ENCOVI 2000 dataset** from Instituto Nacional de Estadística (INE), Guatemala.  
  [Encuesta Nacional sobre Condiciones de Vida (ENCOVI) 2000](https://microdata.worldbank.org/index.php/catalog/586/study-description)
