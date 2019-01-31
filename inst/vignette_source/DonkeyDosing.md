

---

<center> <h1>A guide to using the DonkeyDosing package</h1> </center> 

---

## Introduction

---

TODO

Planned content:

- Set up the model
- Import 2014-2017 locations, FEC and weather data from an archive
- Import 2018 locations and FEC data from the dosing tool
- Import 2018 weather data
- Run the model and obtain coefficients
- Graphical/check outputs
- Verification of Excel model outputs
- Clinical outputs (conditional modes of location/year)
- Scraping weather data
- Outputting animal/location and weather data ready for the 2019 dosing tool

---

## Vignette

This static vignette was built with:


```r
sessionInfo()
```

```
## R version 3.5.2 (2018-12-20)
## Platform: x86_64-apple-darwin15.6.0 (64-bit)
## Running under: macOS High Sierra 10.13.6
## 
## Matrix products: default
## BLAS: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRblas.0.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
## 
## locale:
## [1] en_GB.UTF-8/en_US.UTF-8/en_GB.UTF-8/C/en_GB.UTF-8/en_GB.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## loaded via a namespace (and not attached):
## [1] compiler_3.5.2 magrittr_1.5   tools_3.5.2    stringi_1.2.4 
## [5] knitr_1.20     stringr_1.3.1  evaluate_0.12
```

To rebuild the vignette, find the 'DonkeyDosing.Rmd' file in the 'vignette_source' directory (inside the inst directory of the package tarball), and (after editing as necessary) run:


```r
archivepath <- '..path_to_archive_data../archive.Rdata'
newpath <- '..path_to_2018_data../DosingTool2018.xlsx'

knitr::knit2html('DonkeyDosing.Rmd')
```

This will create an updated html file that can be used to replace the file provided in the vignettes folder of the package.

Note that the two necessary data files are not included in this R package, but were provided to the Donkey Sanctuary as part of the project ending in December 2018.
