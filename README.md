# robyn
Meta's Robyn Implementation
The following code is my implementation of Meta's Robyn R package. The original source can be found here https://github.com/facebookexperimental/Robyn.
I have only made some minor changes such as:

1. Specifing the number of CPUs to use as parallels does not seem to leverage Mac M1, M2 chips sets yet. 
2. Selecting top models based on selecting model metric ranges
3. Adding a function to change divide hyperparms when using daily data
