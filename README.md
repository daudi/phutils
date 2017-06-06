# phutils
Public Health utilities

This is package of various utilities first developed by the Public Health Intelligence team in Medway. 

At the moment it is not on CRAN, so to use it you can build it from the source or use devtools as shown below.

# Installation

## From zip
Download this repository from GitHub and either build from source or do:

	source <- devtools:::source_pkg("C:/path/to/phutils-package")
	install(source)

## With devtools

	devtools::install_github("daudi/phutils")


## If you are blocked by firewalls, try this

Download the zip file and unzip it somehere, e.g. c:\temp\phutils-master
Then in Rstudio do this to install it. You won't get the history and won't be able to git pull to get updates but at least you'll have the package. In time I'll try to get it ready for CRAN to make it easier to install.

	setwd("C:/temp/phutils-master")
	devtools::document()
	devtools::build()
	install.packages("C:/temp/phutils_1.3.0.tar.gz", repos = NULL, type = "source")
	library(phutils)
	## Test it
	proportion_graphic(0.4, denoms = 16)
	
