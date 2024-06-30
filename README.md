# Classification Project | R Programming Language

## Table of Contents
- [Overview](#Overview)
- [Approch 1](#Approch_1)
- [Approch 2](#Approch_2)
- [Decision Tree Split](#Decision_Tree_split)


## Overview

The goal of this project is to compare different machine learning techniques and evaluate their effectiveness on a given dataset. The approaches include logistic regression, support vector machines (SVM), decision trees, random forests, and XGBoost, among others. The project involves detailed steps such as data cleaning, feature engineering, model training, and performance evaluation.

## Approch 1 : File 1

This script focuses on data preprocessing and categorical feature transformation using the MDLPC algorithm. Key steps include:

Data Cleaning: Handling missing values and outliers.
Feature Transformation: Discretizing continuous variables into categorical ones using the MDLPC algorithm.
Exploratory Data Analysis (EDA): Generating contingency tables and visualizations to understand the distribution of categorical variables.
Train-Test Split: Splitting the dataset into training and testing sets for model validation.
Model Training: Implementing logistic regression and SVM models.
Model Evaluation: Calculating performance metrics for the trained models.

## Approch 2 : File 2

This approach is focused on working with only numerical variables. To achieve this, we need to transform the categorical variables without losing the information and the correlation between them and the target variable.

The method used is to apply a decision tree model to the target variable on the categorical variables. However, the objective is not to make predictions, but to split the data into smaller datasets, where each dataset represents a unique combination of categories from the categorical variables that is not correlated with the target variable.

![Decision Tree Split](https://github.com/HamzaEssh22/ML-Classification/blob/main/Decision%20Tree%20Split.png)

Then, we will apply various machine learning models to each dataset to determine the best model for each one. The process of predicting a specific row will start by evaluating the categories and associating the row with one of the datasets, and then using the ML model associated with that dataset to make the prediction.
