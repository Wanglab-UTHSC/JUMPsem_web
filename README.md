# JUMPsem_web

This is a web page application for the package **[JUMPsem](https://github.com/Wanglab-UTHSC/JUMPsem)** developed by Xusheng's lab.
Latest local version is available on github.

The [online version](http://jumpsem.genenetwork.org/) is available on shinyapps.io.

---

# About JUMPsem package
**[JUMPsem]** is a R package infering enzyme activity from phosphoralation, ubiquitination, acetylation.

## JUMPsem Analysis: User Guide

---
In this **online version of JUMPsem package**, users could easily set up parameters just as in the packages. What's more, it also provides plotting options for users to directly visualize the results.

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
2.  Select data type and upload your substrate data. (**required**)
3.  If you have group information file, please upload under `Group Inforamtion(Optional)`.
4.  If you need whole proteomics data to normalize phosphorylation data, please upload under `normalization data(optional)`.
5.  Species of substrate is **required** to select under `Parameters`.
6.  Additional parameters in the package could be adjusted under `Advanced Parameters`.
7.  Click `Run JUMPsem Analysis` and wait for result!

---
