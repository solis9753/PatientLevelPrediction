PatientLevelPrediction
======================

Introduction
============
An R package for building patient level predictive models using data in Common Data Model format.

Features
========
- Takes a cohort and outcome of interest as input.
- Extracts the necessary data from a database in OMOP Common Data Model format.
- Uses a large set of covariates including for example all drugs, diagnoses, procedures, as well as age, comorbidity indexes, etc.
- Various machine learning algorithms can be used to develop predictive models.
- Includes function for evaluating predictive models
- Includes functions to plot and explore model performance (ROC + Calibration)
- Supported outcome models are l1 logistic regression, Random forest, Gradient boosting machines, Naive Bayes, KNN and MLP.

Screenshots
===========
<table border = "">
<tr valign="top">
<td width = 50%>
  <img src="https://github.com/OHDSI/PatientLevelPrediction/blob/master/vignettes/sparseCalibration.png" alt="Prediction calibration plot" title="Prediction calibration plot" />
</td>
<td width = 50%>
 <img src="https://github.com/OHDSI/PatientLevelPrediction/blob/master/vignettes/sparseROC.png" alt="ROC plot" title="ROC plot" />
</td>
</tr><tr>
<td>Calibration plot</td><td>ROC plot</td>
</tr>
</table>

Technology
==========
PatientLevelPrediction is an R package, with some functions implemented in C++ and python.

System Requirements
===================
Requires R (version 3.2.2 or higher). Installation on Windows requires [RTools](http://cran.r-project.org/bin/windows/Rtools/). Libraries used in PatientLevelPrediction require Java and Python.

The python installation is required for some of the machine learning algorithms. We advise to
install Python 2.7 using Anaconda (https://www.continuum.io/downloads)

Dependencies
============
 * Cyclops
 * DatabaseConnector
 * SqlRender
 * FeatureExtraction

Getting Started
===============
1. On Windows, make sure [RTools](http://cran.r-project.org/bin/windows/Rtools/) is installed.
2. The DatabaseConnector and SqlRender packages require Java. Java can be downloaded from
<a href="http://www.java.com" target="_blank">http://www.java.com</a>.
3. Random forest, Naive Bayes and MLP require python 2.7.  Python 2.7 can be downloaded from: <a href="https://www.continuum.io/downloads" target="_blank">https://www.continuum.io/downloads</a>.
4. In R, use the following commands to download and install PatientLevelPrediction:

  ```r
  install.packages("devtools")
  library(devtools)
  install_github("ohdsi/SqlRender") 
  install_github("ohdsi/DatabaseConnector") 
  install_github("ohdsi/Cyclops") 
  install_github("ohdsi/FeatureExtraction")
  install_github("ohdsi/PatientLevelPrediction") 
  ```

Getting Involved
================
* Vignette: [Building patient-level predictive models](https://github.com/OHDSI/PatientLevelPrediction/blob/develop/vignettes/BuildingPredictiveModels.pdf)
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="../../issues">GitHub issue tracker</a> for all bugs/issues/enhancements
 
License
=======
PatientLevelPrediction is licensed under Apache License 2.0

Development
===========
PatientLevelPrediction is being developed in R Studio.

###Development status
[![Build Status](https://travis-ci.org/OHDSI/PatientLevelPrediction.svg?branch=master)](https://travis-ci.org/OHDSI/PatientLevelPrediction)
[![codecov.io](https://codecov.io/github/OHDSI/PatientLevelPrediction/coverage.svg?branch=master)](https://codecov.io/github/OHDSI/PatientLevelPrediction?branch=master)

Beta

# Acknowledgements
- This project is supported in part through the National Science Foundation grant IIS 1251151.

