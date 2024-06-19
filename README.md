# Mannual for DLM-application

Currently, the DLM application only supports daily water quality data. 

## Data
The dataset should have three columns:

* Date
* Flow
* Constituents

## Working Directory
```bash
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
```
