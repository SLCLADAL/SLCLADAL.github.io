library(rmarkdown)

# Manual list of files to exclude from automatic rebuilding
# Based on analysis, these are either work in progress, event specific, or
# just unfinished - I think these are all candidates for deletion from the
#  repository to avoid confusion.

exclude <- list('etpro.Rmd', 'eyews.Rmd', 'git.Rmd', 'mcol.Rmd', 'mixreg.Rmd', 'mmws.Rmd')

# Note we're randomly ordering the list - if the build fails because of this, we have a problem
# in the file that fails.
to_process <- setdiff(list.files(path="content", pattern = "Rmd"), exclude)

# Note that we can't just use the render_site functionality, as that doesn't work with bookdowns html_document2 format.
for (source_file in to_process) {
	rmarkdown::render(
		file.path(
			"content", source_file),
			output_dir = "built_site",
			envir = new.env(),
			clean = TRUE
		)
}
