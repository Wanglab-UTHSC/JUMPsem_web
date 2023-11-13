# eSEM_web

This is an web page application for the package **[eSEM](https://github.com/Wanglab-UTHSC/eSEM)** developed by our lab.
Latest local version is available on the github.
The online version is available on shinyapps.io. [link](https://esem.shinyapps.io/eSEM_rshiny/)

---

# About eSEM package
**[eSEM]** is a R package infering enzyme activity from phosphoralation, ubiquitination, acetylation.

## eSEM Analysis: User Guide

---
In this **online version of eSEM package**, users could easily set up parameters just as in the packages. Whaat's more, it also provides plotting options for users to directly visualize the results.  

## Steps(Example)

- On the top left, click `Example data`. 
- Under `Select Sample` section, select the data type.
  - `Phosphorylation` data search among mouse data set and uses a whole proteom dataset to normalize.
  - `Ubiquination` and `Acetylation` data search among human kinases and does not use whole proteomics data to normalize.
  - The result are in Log2 transformation form.
- Click `Run Example Data` to view the result on right.
- Interactive `Heatmap` will be shown for each result.

---

## Steps(General)

1.  Click `Sample` tab.
2.  Select data type and upload your substrate data. (**necessary**)
3.  If you have group information file, please upload under `Group Inforamtion(Optional)`.
4.  If you need whole proteomics data to normalize, please upload under `normalization data(optional)`.
5.  Species of substrate is **required** to select under `Parameters`.
6.  Additional parameters in the package could be adjusted under `Advanced Parameters`.
7.  Click `Run eSEM Analysis` and wait for result!

---
