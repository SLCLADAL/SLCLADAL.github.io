# LADAL branding of SLCLADAL.github.io
2nd version, released 14 Dec 2020

## Changes
### Files added

- ladal_icon_cas_tran_white_trimed.png

### Files modified

- _navbar.html
- styles.css
- favicon.ico

## Installation Procedure

ladal_icon_cas_tran_white_trimed.png, styles.css and favicon.ico can all just be pasted into the root of the site over the top of the existing files with the same names. This is the same directory that contains the _site.yml and Rmd files.

Like last time _navbar.html is a little trickier. 

We are looking for a directory in RStudio called '/rmarkdown/rmd/h/'.
To locate the directory we need from the RStudio console run the command:

```r

bookdown:::bookdown_file('../rmarkdown/rmd/h/')

```

This should result in something like:
"something/something/site-library/bookdown/../rmarkdown/rmd/h/"
This directory 'h' is where we need to make changes.


#### _navbar.html

In the directory ..../rmarkdown/rmd/h/ should be a file: '_navbar.html'.

Back it up as _navbar.html.bak.14Dec2020 and save it.

Then copy the new _navbar.html over it.


# knit_all.R Script 
I've included a script to knit all Rmd files in a directory
If you only want some files processed then you can modify the array of file names that is passed in.

i.e. 
```R

files = <- list('file1.Rmd', 'file2.Rmd', 'file3.Rmd') 

```