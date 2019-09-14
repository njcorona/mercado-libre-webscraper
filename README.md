# MercadoLibre web scraper 

This repository contains a Mercado Libre web scraper written in R.  It scrapes Argentina, Brazil, Chile, Colombia, Mexico, Peru, and Uruguay for data on the product listings and seller listings of iPhone8 cases, iPhone8+ cases, motorcycle helmets, Bluetooth speakers, and Bluetooth headphones; and it cleans that data to be research-ready.

For those interested in countries and products other than those listed above, the R code in this project should serve as a useful guide for building your own web scraper for Mercado Libre in R.

## Getting started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

1.  Install R on your computer:  http://lib.stat.cmu.edu/R/CRAN/
2.  Install RStudio Desktop (or your preferred interactive development environment (IDE) for R):  https://www.rstudio.com/products/rstudio/download/
3.  Download the code onto your system (I recommend [forking](https://help.github.com/en/articles/fork-a-repo) this repository).
4.  Run setup.R to install the relevant packages onto R.
    a.  This hasn't been written yet - bear with me.

### Running the code

To run multiple scraper files from one master file, you can [run a script that calls source()](https://www.dummies.com/programming/r/how-to-source-a-script-in-r/).  If source() is new to you, as it was to me, you should take a look at runningScrapersNoMerging.R, which is built solely to do this.

### Building your own MercadoLibre webscraper

I recommend creating a copy of one of the country-specific files and editing that one to your needs.  

Please feel welcome add an issue if you would need or want help scraping your own country or product!

### Automating the scraper

This was my ongoing struggle for a while.  I recommend AWS's Free Tier.  I am no longer regularly running the scraper - please contact me if you would like me to change that.

## Author

**Nicolas Corona** - [njcorona](https://github.com/njcorona)  


## License

MercadoLibre webscraper

Copyright (C) 2019 Nicolas Corona

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    See a copy of the GNU General Public License at https://www.gnu.org/licenses/.

## Acknowledgments

* My employer, Felipe Flores-Golfin, Applied Economics Doctoral Student at the Wharton School at the University of Pennsylvania.
* My former professor, Professor Prasanna Tambe, Associate Professor of Operations, Information and Decisions at the Wharton School at the University of Pennsylvania.
