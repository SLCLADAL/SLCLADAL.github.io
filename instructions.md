# LADAL branding of SLCLADAL.github.io


## Changes
### Files added

- styles.css
- favicon.ico
- ladal_icon_cas_tran_white.png

### Files modified

- _navbar.html
- jquery.tocify.css
- default.html

## Installation Procedure 

The three files to be added: styles.css, favicon.ico and ladal_icon_cas_tran_white.png can all just be pasted into the root of the site, the same directory that contains the _site.yml and Rmd files.

It can't cause any damage if these files are added to the wrong place.

#### Modify RStudio.

We are going to change three files used as templates by pandoc - one of the tools used by RStudio to turn Rmd into HTML.
Care must be taken modifying the three existing files. I suggest you create a backup in place by adding .bak as a file extension and keep a copy of the original files in another location - presumably in git.

I have tested this procedure on mac and linux.

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

Back it up as _navbar.html.bak make a copy and save it in git.

Then copy the new _navbar.html over it.

#### default.html

In the same directory as above ..../rmarkdown/rmd/h/ should be a file: 'default.html'

Back it up as default.html.bak make a copy and save it in git.

Then copy the new default.html over it.

#### jquery.tocify.css

This file is in a subdirectory of 'h'

In the directory ..../rmarkdown/rmd/h/tocify should be a file: 'jquery.tocify.css'.

Back it up as jquery.tocify.css.bak make a copy and save it in git.

Then copy the new jquery.tocify.css over it.
