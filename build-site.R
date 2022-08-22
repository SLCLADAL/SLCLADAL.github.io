# This script builds the whole site - optionally you can pass specific files
# to build by name and it build just those files.
library(rmarkdown)

# Manual list of files to exclude from automatic rebuilding
# Based on analysis, these are either work in progress, event specific, or
# just unfinished - I think these are all candidates for deletion from the
#  repository to avoid confusion.

exclude <- list(
	'etpro.Rmd',
	'eyews.Rmd',
	'git.Rmd',
	'mcol.Rmd',
	'mixreg.Rmd',
	'mmws.Rmd',
	'webcrawling.Rmd',
	'todo.Rmd',
	'txt2rda.Rmd',
	'speech2text.Rmd',
	'sldata.Rmd',
	'rmd2html.Rmd',
	'rmd2jupyter.Rmd',
	'simulatedata.Rmd'
)

# Check if we have any command line arguments, otherwise just run on everything.
args = commandArgs(trailingOnly=TRUE)

if (length(args) > 0) {
	to_process <- args
} else {
	to_process <- setdiff(list.files(path="content", pattern = "Rmd"), exclude)
}

failed_files <- list()
failure_reasons <- list()

# Note that we can't just use the render_site functionality, as that doesn't work with bookdowns html_document2 format.
for (source_file in to_process) {
	tryCatch(
		xfun::Rscript_call(
		  rmarkdown::render,
		  args = list(file.path("content", source_file), output_dir = "docs"),
		),
		error = function(cond){
			failed_files <<- append(failed_files, source_file)
			failure_reasons <<- append(failure_reasons, cond)
		},
		warning = function(cond){
		}
	)
}

for(i in seq_along(failed_files)){
    message(failed_files[i])
    message(failure_reasons[i])
    message("")
}


# It is an error to return with any failed files
quit(save="no", status=length(failed_files))
