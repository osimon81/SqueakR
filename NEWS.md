# SqueakR 1.3.0

## version 1.3.0 [GitHub release] [CRAN release]

---

* Added `animal` field for each added experiment data, which updates experiment
  metadata for `animals` field. Updated compatibility to match semi-automatic and
  automatic SqueakR pipelines, as well as `describe_experiment()` function. Updated
  documentation to reflect the change
* Added Plotly sunburst plot (to functions list and dashboard) to graph
  proportions of collected data done per experimenter, as well as data collected
  per animal per group
* Added Plotly 3D surface plot (`plotSurface()`) using 2D kernel density estimation to represent 
  call density against principal frequency and call length
* Added functions for unblinding experimenters to data
* Added function — `plotContours()` — which renders `plotSurface()` as a 2D rendering, flanked by
  histograms displaying distributions of principal frequency and call length
* Standardized some function naming (plotSurface instead of plotCallDataSurface, etc)

## version 1.2.8

---

* Used `report` package for analyses: added functions for reporting descriptive
  statistics for grouped data in SqueakR Dashboard tab
* Deprecated SqueakR Dashboard repository on GitHub and merged functions into
  SqueakR
* Added 3D cluster plot visualization function `plotClusters()` against call
  length (z), principal frequency (x) and mean power (y) using plotly, available
  in base SqueakR and on dashboard

## version 1.2.5

---

* Merged SqueakR dashboard into SqueakR package! Dashboard can now be run after
  loading the package by running `squeakRDashboard()`
* Added function for calculating ANOVA between groups
* Added ANOVA tab on SqueakR Dashboard for determining statistical significance
  between call features

## version 1.2.0

---

* Added fully-automatic pipeline function (`autosqueakRpipeline()`) which references
  Google Sheet to add data without experimenter input for every file
* Renamed `experiment_pipeline()` to `semisqueakRpipeline()`
* Added function (`remove_experiment_data()`) to remove data from experiment object
* Updated `vignette` to reflect package changes

## version 1.0.0

---

* Recreated `NEWS.md` file to track changes to the package.
* Added functions for generating experiment and analyzing data between experimental groups.
* Added visualizations including ethnograms, histograms, box-plots, and correlation matrices.

## version 0.5.1

---

- Basic set of functions without all plotting functions
- Bug fixes
- Prepared development release for Mac
- Prepared development release for Windows


## version 0.0.0.9000

---

### NEWS.md setup

- added development `NEWS.md` file
