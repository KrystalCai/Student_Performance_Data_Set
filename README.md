# Student Performance Data Set

## Overview

This repository contains the dataset and associated analysis for predicting student performance. The dataset includes various attributes related to students' study habits, social life, academic performance, and other factors that may influence their grades.

## Dataset

The dataset is taken from the UCI Machine Learning Repository and includes data on students' academic achievements in secondary school, along with factors like:

- **G1, G2, G3**: First, second, and final grades.
- **studytime**: Time spent on studying.
- **failures**: Number of past class failures.
- **absences**: Number of school absences.
- **other variables**: Includes students' demographic details, family background, and more.

The goal of this project is to build a predictive model that can estimate students' final grades (G3) based on these attributes.

## Files

- **student-mat.csv**: The main dataset file in CSV format.
- **scripts/**: Contains R scripts used for data analysis and model building.
- **reports/**: Includes generated reports, such as plots and PDF summaries.
- **.gitignore**: A file used to exclude unnecessary files and directories from version control.
- **README.md**: This file, containing the project description and instructions.

## Usage

To load the dataset and perform initial analysis, use the following R code:

```r
library(dplyr)
stu <- read.csv("student-mat.csv", sep = ";")
head(stu)
```
