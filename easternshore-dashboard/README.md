# equity-dashboard
This repo hosts the R code to collect and prepare data and to create the app for the prototype dashboard for regional indicators.

[Regional Equity Dashboard Prototype](http://commdash.batten.virginia.edu:3838/cville-region/)

## datacode

Files to pull and prepare data

* county_codes.csv:	define localities to include
* acs_county_data.R:	get county-level variables from American Community Survey, 5-year estimates
* acs_tract_data.R:	get tract-level variables from American Community Survey, 5-year estimates
* acs_blockgroup_data.R:	get blockgroup-level variables from American Community Survey, 5-year estimates
* acs_county_data_time.R:	get select county-level variables from decennial/acs past years
* acs_tract_data_time.R:	get select tract-level variables from decennial/acs past years
* addl_county_data.R:	get non-census county-level variables
* addl_tract_data.R:	get non-census tract-level variables
* schools_data.R:	get school location and attendance zone data
* parks_data.R:	get local park polygons data
* combine_data.R:	pull data together, derive additional measures, create app_data.R used by app.R

## cville-region

Files to create app

* app.R: the file to create the app
* BAed_003.html: file containing example story (created in stories)
* alb_homesales_school.html: file containing example story (created in stories)
* equity_links.Rmd: markdown file generating equity_links.html
* equity_links.html: file containing links to research and resources

## stories

Files to create example stories for dashboard

* alb_sales/get_alb_data.R:	get real estate data from county website
* alb_sales/albemarle.R: generate sales by jurisdiction
* alb_sales/alb_story.R: working on summaries/figures to use in alb_homesales_school.Rmd
* alb_sales/alb_homesales_school.Rmd:	generate alb_homesales_school.html doc to include in app
* alb_sales/alb_homesales_school.html: file containing example story (copied to cville-region)
* alb_edequity/BAed_003.Rmd: generate BAed_003.html doc to include in app (built from work on [equity-indicators](https://github.com/commpaslab/equity-indicators))
* alb_edequity/BAed_003.html: file containing example story (copied to cville-region)

