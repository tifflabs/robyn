# robyn
<b>Meta's Robyn Implementation</b>
The following code is my implementation of Meta's Robyn R package. The original source can be found here https://github.com/facebookexperimental/Robyn.
I have only made some minor changes such as:

1. Specifing the number of CPUs to use as parallels does not seem to leverage Mac M1, M2 chips sets yet. 
2. Selecting top models based on selecting model metric ranges
3. Adding a function to change divide hyperparms when using daily data

<b>What is Robyn?</b>

Robyn is an experimental, semi-automated and open-sourced Marketing Mix Modeling (MMM) package from Meta Marketing Science. It uses various machine learning techniques (Ridge regression, multi-objective evolutionary algorithm for hyperparameter optimization, time-series decomposition for trend & season, gradient-based optimization for budget allocation, clustering, etc.) to define media channel efficiency and effectivity, explore adstock rates and saturation curves. It's built for granular datasets with many independent variables and therefore especially suitable for digital and direct response advertisers with rich data sources.
