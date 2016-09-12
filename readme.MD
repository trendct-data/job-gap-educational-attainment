
# Analyzing employment figures by educational attainment

This repository contains source data and analysis code for the Trend CT story:

* [Economic recovery leaving behind those who did not go to college](http://trendct.org/2016/07/13/countrys-economic-recovery-leaving-behind-those-who-did-not-go-to-college/)

# Walkthroughs

* Data analysis and visualizations of national and Connecticut data- [[RMarkdown](https://github.com/trendct-data/job-gap-educational-attainment/blob/master/index.Rmd)] [[HTML](http://trendct-data.github.io/job-gap-educational-attainment/)]

### The Data

* The monthly data used in this analysis was copy and pasted and cleaned by hand from the [Census website](http://www.census.gov/cps/data/cpstablecreator.html) and the annual data was brought in using the Census API.

### What's in this repo

* `data` [folder]((https://github.com/trendct-data/tree/master/job-gap-educational-attainment/data)) with raw data and modified data.
* `jobs_ct_census.R` is an R script charting annual Connecticut's job changes over time by educational attainment using the Census API.
* `state_specific.R` is an R script charting some Connecticut-specific data.
* `us_visualizations.R` is an R script that will chart the changes in employment by educational attainment. 
* `png` folder of .png files of various chart outputs

----

### Philosophy

We believe in open data. Our scripts may be too technical for the average reader, but we make it accessible because we believe it is important to be as transparent as possible about our methodology so our work can be checked or expanded upon. 

Check out the reproducible scripts and data behind many of our other stories in our [central repo](https://github.com/trendct/data)

Follow us out on Twitter [@TrendCT](http://www.trendct.org) and on [Facebook/TrendCT](https://www.facebook.com/trendct/).

### Usage

If you use our data or methodology, please give us a shout out in your story. It'd also be nice if you gave me a heads up: [@abtran](http://www.twitter.com/abtran) or abtran@trendct.org.