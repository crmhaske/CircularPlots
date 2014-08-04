CircularPlots
=============

R functions for polar plotting. These functions are best used with directional data.

Master
------

**Stacked polar bar chart**

stackedPolarBarChart.R - plots un-normalized, stacked bars on a polar plot

stackedPolarBarChartExs.R - exmaple of how to use the function in stackedPolarBarChart.R

HistogramFits
-------------

**Plotting a single histogram with one fit line**

polarHistFit.R <- contains two functions: histxy and polarHistFit
- histxy: computes rectangular histograms
- polarHistFit: plots the histograms and the fit line

polarHistFitExs.R <- contains two examples using polarHistFit
- bimodal distribution around the semi-circle
- normal distribution around the whole circle

**Plotting multiple histograms with multiple fit lines onthe same plot**

polarHistFitMulti.R <- contains two functions: histxy and polarHistFit
- histxy: computes rectangular histograms
- polarHistFit: plots the histograms and the fit lines

polarHistFitMultiExs.R <- contains one example using polarHistFitMulti
- three normal distributions with varying standard deviations
