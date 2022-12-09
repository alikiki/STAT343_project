# Decision / Change List

* (APPROVED) For types, delete the entries that are ambiguous. For example, the ones that are like "Blade (Flake?)" or "Core fragment? Flake?". Reason: if labeled ambiguously, we will never know the "correct" label, and if we decide to coerce into one of the levels by looking at the data, then we are introducing selective inference problems. We only lose 9 data points (original: 652, modified: 643)

* (APPROVED) For site, delete the two sites that are not Ali Kosh and Chagha Sefid. Reason: the label is ambiguous for the Ali Kosh / Chagha Sefid point, and for the Tepe point, there's only one point so it's not like we could derive any useful insights regarding that site anyway. We only lose two data points (original: 643, modified: 641)

* Calculated R-squared of regressing a covariate onto another covariate (e.g. regressing `element_Y` onto `element_Rb`, `element_Sr`, `element_Zr`)

* Plan is to first do some outlier analysis, then do some forward stepwise selection, then deal with the variance considerations via transformations and/or interaction terms

* Completed outlier analysis - no statistically significant outliers

* Completed variable selection - tested all models of the form type + site + [continuous covariate]. The model type + site + element_Rb was found to reduce the R-squared the least. 

* Checked again for outliers and high leverage points - none found




# To do

* clean up data cleaning portion and state reasons (mostly selective inference concerns)

* check again for outliers, high leverage points, etc.