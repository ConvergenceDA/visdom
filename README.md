# VISDOM (Visualization and Insight System for Demand Operations and Management) 
This module supports bulk time series analysis and related modeling tools for utility interval meter data.

To get started, ensure that you can access the repository here: https://github.com/convergenceda/visdom

See the code in the vignettes directory, especially the R markdown file [example_feature_extraction.rmd](./vignettes/example_feature_extraction.rmd). Note: rmd's mix commentary and code and this one will give you a sense of what is required to run features on a given sample of meter data. Cleaning them up is still on our todo list, but the raw source will provide a sense of usage for other tasks.

You steps will be:

1. To install VISDOM as a package directly from the github repository, with all of its module dependencies automatically installed, follow the instructions in [install_visdom.rmd](./vignettes/install_visdom.rmd) in the vignettes folder. In a nutsheel, you want to run these commands:
   ```r
   # install devtools if you don't have it.
   install.packages(c("devtools"))

   devtools::install_github("convergenceda/visdom", build_vignettes=T )
   
   # check if it works!
   library(visdom)
   ?visdom
   
   # find a vignette you are interested in
   vignette(package='visdom')
   vignette('authoring_data_source', package='visdom')
   ```
2. If you will be contributing code or documentation, follow Hadley Wickham's excellent "Getting Started" introduction to get devtools and documentation generation support: http://r-pkgs.had.co.nz/intro.html#intro-get, or you can follow the same steps and see related notes and caveats in [bootstrap_devel_environment.rmd](./vignettes/bootstrap_devel_environment.rmd) in vignettes.
  *Familiarize yourself with the rest of the R package background reading available here: http://r-pkgs.had.co.nz/
  * Get added as a collaborator and clone the project from GitHub `git clone git@github.com:convergenceda/visdom.git` so you can work on it and contribute changes locally.
  * You can generate documentation and/or install from your local source as a module with the commands `devtools::document()` and `devtools::install()`
3. To use VISDOM with your own data, you will need to author a DataSource object that maps between the formatting of your data and the data structures used by VISDOM by implementing all the relevant functions stubbed out by DataSource in [util-dataSource.R](./R/util-dataSource.R). This is the key step to using VISDOM and the implementation and usage of a typical data source is detailed in [authoring_data_source.rmd](./vignettes/authoring_data_source.rmd). You will typically need to set up data access (i.e. to a SQL database if applicable - see [util-dbUtil.R](./R/util-dbUtil.R) - or figure out how you will be loading your data from disk or elsewhere), and write the code to perform the queries or other data access steps as appropriate to load, format, and return your data in the VISDOM standard format expected to come out of a data source. You can see the DataSource implemented for testing purposes in the file [testDataSource.R](./R/testDataSource.R) in the R directory of the package. 
5. Call `DATA_SOURCE = YourDataSource()` to setup your data source for use by VISDOM (i.e. assign it to the global variable DATA_SOURCE)
6. Beyond this point, we assume that you have a working knowledge of the capabilities of the VISDOM package and a decent idea of how you would want to use it for inspiration, look at the vignettes relatd to exploring meter data ([customer_data_objects.rmd](./vignettes/customer_data_objects.rmd) and [weather_data_objects.rmd](./vignettes/weather_data_objects.rmd)), doing feature extraction ([example_feature_extraction.rmd](./vignettes/example_feature_extraction.rmd) and [example_iterator_usage.rmd](./vignettes/example_iterator_usage.rmd)), load shape analysis ([example_load_shape_analysis.rmd](./vignettes/example_load_shape_analysis.rmd)), and customizing analysis for advanced users ([advanced_usage.rmd](./vignettes/advanced_usage.rmd)).
7. Follow the outline of the extract features script using your data source and correcting any errors and issues that come along.
8. Use the functions iterator.todf() from [R/iterator.R](./R/iterator.R) to extract a data.frame of scalar meter data features from the list of lists returned by `iterator.iterateCustomers()` and the various merge and export capabilities of [util-export.R](./R/util-export.R) to format your feature data for external consumption.

