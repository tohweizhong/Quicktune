# Quicktune

An R package for quick tuning of xgboost models (WIP)

I envision this package to be a hyperparameter package in R, comprising of various tuning algorithms - potentially including one which I would like to study more thoroughly first. Primary ML algorithm to be focused is the extreme gradient boosting algorithm, may move on to others. Key functionalities are based on the caret and xgboost packages.

To install from github, run the following in R:
```
library(devtools)
install_github("tohweizhong/Quicktune")
```
## 1st iteration (MVP)

Functions:
- FirstLevelTune()
- SecondLevelTune()
- TwoLevelTune()
- PlotTune()