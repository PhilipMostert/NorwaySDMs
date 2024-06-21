## *intSDM* version 2.0.1

------------------------------------------------------------------------

### Package updates

-   Fixes to the plot function such that it works even if no species present in a given dataset.

-   Removed the argument *assign2Global* in `.$addGBIF()`

-   Fixed issue regarding obtaining covariates from *WorldClim* when a country name was not specified in `startWorkflow` or `.$addArea`.

-   Fixed the naming of variable problem that occurred when only one dataset is considered in an analysis.

-   Added a new vignette: *PennsylvaniaWarbler*. This vignette is similar to the *Setophaga* vignette from the *PointedSDMs* R package. It is designed to show how to add species occurrence data and environmental covariates to the workflow from sources other than *GBIF* and *WorldClim*.

-   Added the argument *removeDuplicates* to `.$addGBIF`, which will now remove duplicate records for a given species. This was automatic in the previous version, but became quite timely to run when multiple datasets were included in the workflow.
