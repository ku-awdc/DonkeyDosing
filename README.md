---

## DonkeyDosing (R package)

---

### Overview

---

This R package was created as part of a research project ending in 2018 that was part of a long-term strategy working towards “best practice” for control of small strongyles and other endoparasites of welfare concern to donkeys and other equids at the Donkey Sanctuary. The ultimate goal of this project was to manage the parasite population in co-grazed animals with minimal use of anthelmintic drugs, in order to reduce the development rate of anthelmintic resistance and therefore maintain the efficacy of drugs for use in animals with clinical disease caused by endoparasites. The goal was to develop and implement a predictive model that could be used to predict the future faecal worm egg count (FWEC) of a group of individual animals, and provide recommendations for optimal dosing of the animals with anthelmintics in order to prevent contamination of pasture with the minimal possible use of drugs. At the end of the project, an Excel-based dosing support tool implementing this model was delivered to the Donkey Sanctuary. The code contained in this R package is intended as an accompaniment to this tool, and contains functions to re-estimate the prediction model on an annual basis once more data becomes available.  Code is also provided to replicate and validate the Excel-based tool, along with a number of additional utility functions.

---

### Installation

To install this package, the free software R must first be installed from http://cran.r-project.org (installation of RStudio from https://www.rstudio.com is also recommended).  It is strongly recommended that R be installed in a location without spaces in the (absolute) file path:  this is particularly relevant on Windows where the default installation of 'Program Files' will likely lead to problems with installing packages from source.  The following standard R packages are also required to be installed from within R: devtools, tidyverse, hellno, lmerTest, darksky and R.rsp.  Please also ensure that the most recent versions of these packages are installed by running e.g.:

```r
update.packages(ask=FALSE, type='binary')
```

The DonkeyDosing package can then be installed from the online source code by running the following code from within R:

```r
devtools::install_github('https://github.com/ku-awdc/DonkeyDosing')
```

If this operation fails, please ensure that R is installed in a location that does not contain any spaces in the path (i.e. NOT within 'Program Files').

---

### Usage

Following download, the package installation can be tested using:

```r
library('DonkeyDosing')
vignette('DonkeyDosing')
```

The vignette that this command opens gives a more complete description of how to use the package.  The vignette can also be accessed using a web browser directly from:  https://github.com/ku-awdc/DonkeyDosing/inst/vignette_source/DonkeyDosing.html

---

### Contact

If you are interested in contributing to this R package, or in collaborating on a similar project, please feel free to get in touch.  I am also actively developing related tools for designing and analysing faecal egg count reduction test (FECRT) studies (http://www.fecrt.com/).  For contact details, please see: http://www.fecrt.com/about/ and/or https://ivh.ku.dk/english/employees/?pure=en/persons/487288

This package was developed as part of a research project funded by the Donkey Sanctuary (https://www.thedonkeysanctuary.org.uk).
