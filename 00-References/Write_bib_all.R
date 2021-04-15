# Create aggregate .bib file from individuals

# Directory assumed to be "./book_project/References"
## If working in project directory, no change
## If working in References directory, setwd to parent

if(!dir.exists("./00-References")){setwd("..")}

# List .bib files within "References" directory 
bib.files <- list.files(path="./00-References", pattern="\\.bib$", recursive = TRUE, full.names = TRUE)

print(paste("There are", length(bib.files), ".bib files in the References directory."))

# Read .bib files into a single object
bibs <- lapply(bib.files,readLines)

# Write new .bib file for book project bibliography
write.table(unlist(bibs), file = "./00-References/references.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)
