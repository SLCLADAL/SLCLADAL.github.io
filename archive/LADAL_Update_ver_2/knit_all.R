# This simple script knits all Rmd files in the current directory
# Stephen Kennedy-Clark Dec 2020

# to restrict the script to only processing a subset of files
# alter the first line to something like:
# files = <- list('file1.Rmd', 'file2.Rmd', 'file3.Rmd') 

files <- list.files(pattern = "[.]Rmd$")

bad_files <- list()
warning_files <- list()

for (file in files) {
    tryCatch(
        {
            rmarkdown::render(file)
        },
        error=function(e){
            list.append(bad_files, file)
        },
        warning=function(w){
            list.append(warning_files, file)
        }

    )
}

print("unable to process: ")
print(bad_files)

print("Warnings for files: ")
print(warning_files)