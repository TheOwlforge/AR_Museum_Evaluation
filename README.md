# Projective AR in a Museum
## Evaluation of an Interactive Painting Application

This repository contains R code used to evaluate an [interactive museum application](https://github.com/timkaiser/Paintique), as well as the applied surveys and the data gathered during the evaluation. In total two studies were conducted to evaluate the application. The first user study assessed the prototype, whereas the second focused on the impact of the application on knowledge transfer in comparison to traditional exhibition methods.
The analysis features Quantile Regression and Generalized Additive Models, as well as Bootstrapping next to traditional statistical testing. Most plots were produced using the library ggplot2.

## Featured results 
(more can be found in the plots folder)

### Comparative Study on Knowledge Transfer
<p align="center">
  <img src="plots/Survey2/gam_reg_CI.png" width="400" title="GAM">
  <img src="plots/Survey2/gam_boot.png" width="400" title="Bootstrapped GAM">
</p>
<p align="center">
  <img src="plots/Survey2/boxplot_4_11.png" width="400" title="Boxplot">
  <img src="plots/Survey2/quant_reg_0.75.png" width="400" title="Quantile Regression">
</p>

### User Study on the Prototype
<p align="center">
  <img src="plots/Survey1/dataplot_1_Age Group.png" width="300" title="Age Distribution">
  <img src="plots/Survey1/dataplot_usability_questions_cut5.png" width="500" title="Usability Questions">
</p>
<p align="center">
  <img src="plots/Survey1/piechart_5_Drawing Frequency.png" width="400" title="Drawing Frequency">
  <img src="plots/Survey1/testplot_5_17_fill.png" width="400" title="Drawing Frequency vs. Would use again">
</p>
<p align="center">
  <img src="plots/Survey1/corrplot.png" width="400" title="Correlation Matrix">
</p>
