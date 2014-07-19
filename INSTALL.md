The Explora package relies on several third party R packages
which need to be installed in a local R library for access
by the main Explora script 'library()' loading procedures.

(Note: The following set of instructions was adapted from
 http://www.r-bloggers.com/installing-r-packages/).

Let’s suppose you want to install the ggplot2 package. 
You just fire up an R shell and type:

> install.packages("ggplot2")

For Explora, the required set of such package installation 
instructions may be found in the install.R' script in the R
subdirectory of the project.

In theory, by running an *install.packages* instruction,
the package should just install; However if you 
are using Linux and don’t have root access,  this command won’t work. 
You will be asked to select your local mirror, i.e. 
which server should you use to download the package.

# Installing packages without root access

First, you need to designate a directory where you will store the downloaded packages. 
For example, you can use the directory /data/Rpackages/ 

After creating a package directory, to install a package you use the command:

> install.packages("ggplot2", lib="/data/Rpackages/")
> library(ggplot2, lib.loc="/data/Rpackages/")

It’s a bit of a pain having to type /data/Rpackages/ all the time. To avoid this burden,  we create a file .Renviron in our home area, and add the line R_LIBS=/data/Rpackages/ to it. This means that whenever you start R, the directory /data/Rpackages/ is added to the list of places to look for R packages and so:

> install.packages("ggplot2")
> library(ggplot2)

just works!

# Setting the repository

Every time you install a R package, you are asked which repository R should use. 

To set the repository and avoid having to specify this at every package install, simply:

	    <create a file .Rprofile in your home area.
	    <Add the following piece of code to it:
		<
		<cat(".Rprofile: Setting UK repository")
		<r = getOption("repos") # hard code the UK repo for CRAN
		<r["CRAN"] = "http://cran.uk.r-project.org"
		<options(repos = r)
		<rm(r)
		
Alternatively, you check your Rprofile.site (in the directory /your-R-installation/etc/ , eg R-2.14.0/etc). 
There you see following lines commented out :

# set a CRAN mirror
# local({r <- getOption("repos")
#       r["CRAN"] <- "http://my.local.cran"
#       options(repos=r)})

So remove the comment marks and change "http://my.local.cran" to the correct website.
