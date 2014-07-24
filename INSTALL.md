## Step 1 - Installing R

Explora is large written in the [R Statistical Language](http://www.r-project.org/) and is projected to be available as a package within the Comprehensive R Archive Network (CRAN) as well as this github project site.

To get started, users of Explora should first install (generally latest version of) the core R statistical package.  Going to the main [R Statistical Language home page](http://www.r-project.org/) , click on  Download, Packages..CRAN link, which takes you to the CRAN mirrors page. Pick a mirror site geographically closest to you and follow the directions for your particular operating system (i.e. Windows, Linux, MacOSX).

For convenience, you should put the R program bin directory on your operating system PATH variable.

### Setting the CRAN Repository

Every time you install additional R packages, you are asked which repository R should use. It us usually the case that you should point to one of the suitable mirror sites - mentioned above - of the Comprehensive R Archive Network (CRAN) from where you downloaded the main R package.

To configure your system to point to a chosen CRAN repository and avoid having to specify this at every package install, you can simply create a file .Rprofile in your home area and add the following piece of code to it:

    cat(".Rprofile: Setting UK repository")
    r = getOption("repos") # hard code the UK repo for CRAN
    r["CRAN"] = "http://cran.uk.r-project.org"
    options(repos = r)
    rm(r)
		
Alternatively, you check your Rprofile.site (in the directory /your-R-installation/etc/ , eg R-3,3,1/etc) where you will see following lines commented out:

    # set a CRAN mirror
    # local({r <- getOption("repos")
    #       r["CRAN"] <- "http://my.local.cran"
    #       options(repos=r)})

So remove the comment marks, and change "http://my.local.cran" to point to your CRAN mirror site.

## Step 2 - Downloading Explora

The Explora package may be downloaded from [here](https://github.com/bioversity/germplasm-selection-tool/archive/master.zip). This is the current copy of the R package extracted directly from the current Github project folder as a zip archive called "germplasm-selection-tool-master.zip" [1].

## Step 3 - Installing Dependencies

Explora has dependencies on several other R statistical packages - i.e. gWidgets, gWidgetsRGtk2, plyr, vegan, cluster, ade4, grid, gridExtra - which should generally be installed first on your system. 

A small R configuration script is available under the "R" directory of the (unzipped) downloaded Explora package, or may be downloaded directly from Github [here](https://raw.githubusercontent.com/bioversity/germplasm-selection-tool/master/R/explora_install.R). Once available, the script can be either run from from operating system command line, as follows:

    R CMD BATCH explora_install.R  output.log

(where you can see the results documented in output.log) or alternatively, from inside the R command line or R graphical application [2]  window, as follows:

    source("explora_install.R")

assuming, of course, that the script is located in your working directory. 

## GIMP Tool Kit (GTK+)

The graphical components of the above dependencies also depend on the GIMP Tool Kit (GTK+) which is available from the [GTK+ Project website download page](http://www.gtk.org/download/index.php).  Some Unix-like systems may already have GTK+ installed (you should check).
 
However, for Windows, it is highly likely that you need to download and install Version 2.22 or better (but _NOT_ the 3.x version) of the BINARY BUNDLE (not just the regular binary) of GTK+ from the above link. Note that you should generally get the version that matches the version of R software you installed (i.e. 32 bit Win32 or 64 bit Win64).

After downloading and unzipping the archive somewhere on your local disk, you need point a [Windows environment variable](http://msdn.microsoft.com/en-ca/library/windows/desktop/ms682653%28v=vs.85%29.aspx) called GTK_PATH to this directory, and also, add _%GTK_PATH%\bin_ to your PATH environment variable [3].  

## Step 4 - Installing the Explora Package

After installing the above dependencies, the Explora package itself may be successfully installed using the following command in the working directory containing the downloaded package (zip file or unzipped directory):

    R CMD INSTALL germplasm-selection-tool-master  

where 'germplasm-selection-tool-master' is the default name of the downloaded package from Github. as the installation procedure runs, you'll likely see the application appear briefly on your desktop - that is fine. It will shut itself down again.

## Step 5 - Running the Software

After the installation of Explore, you can run the software from the R command line or from the R graphical application (preferably, from the package _germplasm-selection-tool-master\R_ directory) by using the R _source_ command to load the explora.R script as follows:

    source("explora.R")

From within the R graphical application, this command may also be conveniently run from the menu item **_File..Source R code_** command to select the explora.R script for running.

**Notes:**

[1]  At the present time, the package is only available directly off the Github repository but will be published in CRAN towards the end of 2014. 

[2]  The R graphical application, installed by the Windows binary, is a very convenient environment from which to run the above Explora installation process and to run the application itself

[3] Note that the R GTK+ using packages seem to be very particular about using the Win32 2.22 or greater (not 3.x) version of the library, in order to properly work.