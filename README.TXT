README

This R repository is for demonstration of algorithms involved in the book
Mathematical Modeling (4th Edition) written by Prof. Mark. M. Meerschaert

Some codes are edited from the original repository on the website of the book before 2018
https://www.stt.msu.edu/users/mcubed/modeling.html
Others are coded, edited and tested by Hao Li during Dec. 2018 - Jan. 2019.

This repository includes:
a README.txt
.R : R scripts
.Rmd: R markdown files, from which you can edit documents
.docx: Knitted documents from .Rmd with text and graph output



For the version of R on which it is tested:

platform       x86_64-w64-mingw32          
arch           x86_64                      
os             mingw32                     
system         x86_64, mingw32             
status                                     
major          3                           
minor          5.1                         
year           2018                        
month          07                          
day            02                          
svn rev        74947                       
language       R                           
version.string R version 3.5.1 (2018-07-02)
nickname       Feather Spray

Most script would work on a 3+ version of R.

I used CRAN version of R with Rstudio.
Specifically, some knitr may not work if you use Microsoft MRAN version of R and OpenGL need to be installed (ignore this if you are using windows) to run interactive 3D plotting.


About functional programming:
In R it is encouraged that the programmer get a piece of working chunk of code before writing them as functions.
Functions can be easy to reuse. However, it may be tedious to load functions from scripts (as shown in this repository). You may install packages by other users or even make your own packages.

Packages:

For CAS:

deriv, Ryacas are used as CAS (Computational Arithmetic System) within R. Some integrations of CAS in R may be limited. The symbolic objects are less flexible and maybe unintuitive. The use of CAS is demonstrated in a small scale.

For visualizations:

rgl, plot3D, and plot3Drgl are used for 3d plotting as replacement for base and scatterplot3D package. GL interface is interactive which enables the users to zoom and rotate the plots in an GL device.
The folder 'rgl_screenshots' contains images of rgl Device captured. Grid and axis can make it more intuitive to check scales on the 3D graphs. Grid and axis can be modified with the grid3d() and axis() functions.

For documents:

The .rmd file can be  opened and edited with R if you have knitr installed. It is a powerful package to write paper with codes. The .rmd support tables and math expressions (LaTeX) and can be exported to HTML, .DOC or PDF.

For parallel computing:

The cases involved do not need lots of time for computing, however, parallel mapping in R can be a tidy way to format the data.

The most popular package in R for parallel computing may be snow, however, to make mapping and setting up easy I am using parallel (installation not required) and doParallel.