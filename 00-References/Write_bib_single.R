# Program to write .bib file for single entry
# Review Entry types below, then include appropriate fields


# Follows structure
# # @ENTRY.TYPE{BIBTEX.KEY,
# #   FIELDS,
# # }

# 1. Change Entry Type
# 2. Change Bibtex Key
# 3. Enter Fields
# 4. Change Filename to reflect Bibtex Key
# 5. Be cognizant of directory


# From wikipedia <https://en.wikipedia.org/wiki/BibTeX> [Accessed 13 March 2020]

# #  article
# # # An article from a journal or magazine.
# # # Required fields: author, title, journal, year, volume
# # # Optional fields: number, pages, month, doi, note, key

write.table(
'@article{key,
  author = {},
  title = {},
  journal = {},
  year = {},
  volume = {},
  
  number = {},
  pages = {},
  month = {},
  doi = {},
  note = {},
  key = {},
}'
, file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)





# #  book
# # # A book with an explicit publisher.
# # # Required fields: author/editor, title, publisher, year
# # # Optional fields: volume/number, series, address, edition, month, note, key, url

write.table(
'@book{key,
  author = {},
  editor = {},
  title = {},
  publisher = {},
  year = {},
  
  volume = {},
  number = {},
  series = {},
  address = {},
  edition = {},
  month = {},
  note = {},
  key = {},
  url = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)





# #  booklet
# # # A work that is printed and bound, but without a named publisher or sponsoring institution.
# # # Required fields: title
# # # Optional fields: author, howpublished, address, month, year, note, key

write.table(
'@booklet{key,
  title = {},
  
  author = {},
  howpublished = {},
  address = {},
  month = {},
  year = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)





# #  conference
# # # The same as inproceedings, included for Scribe compatibility.

write.table(
'@conference{key,
  author = {},
  title = {},
  booktitle = {},
  year = {},
  
  editor = {},
  volume = {},
  number = {},
  series = {},
  pages = {},
  address = {},
  month = {},
  organization = {},
  publisher = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)





# #  inbook
# # # A part of a book, usually untitled. May be a chapter (or section, etc.) and/or a range of pages.
# # # Required fields: author/editor, title, chapter/pages, publisher, year
# # # Optional fields: volume/number, series, type, address, edition, month, note, key

write.table(
'@inbook{key,
  author = {},  
  editor = {},
  title = {},
  chapter = {},
  pages = {},
  publisher = {},
  year = {},
  
  volume = {},
  number = {},
  series = {},
  type = {},
  address = {},
  edition = {},
  month = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  incollection
# # # A part of a book having its own title.
# # # Required fields: author, title, booktitle, publisher, year
# # # Optional fields: editor, volume/number, series, type, chapter, pages, address, edition, month, note, key

write.table(
'@incollection{key,
  author = {},
  title = {},
  booktitle = {},
  publisher = {},
  year = {},
  
  editor = {},
  volume = {},
  number = {},
  series = {},
  type = {},
  chapter = {},
  pages = {},
  address = {},
  edition = {},
  month = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  inproceedings
# # # An article in a conference proceedings.
# # # Required fields: author, title, booktitle, year
# # # Optional fields: editor, volume/number, series, pages, address, month, organization, publisher, note, key

write.table(
'@inproceedings{key,
  author = {},
  title = {},
  booktitle = {},
  year = {},
  
  editor = {},
  volume = {},
  number = {},
  series = {},
  pages = {},
  address = {},
  month = {},
  organization = {},
  publisher = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)





# #  manual
# # # Technical documentation.
# # # Required fields: title
# # # Optional fields: author, organization, address, edition, month, year, note, key

write.table(
'@manual{key,
  title = {},
  
  author = {},
  organization = {},
  address = {},
  edition = {},
  month = {},
  year = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  mastersthesis
# # # A Master's thesis.
# # # Required fields: author, title, school, year
# # # Optional fields: type, address, month, note, key

write.table(
'@mastersthesis{key,
  author = {},
  title = {},
  school = {},
  year = {},
  
  type = {},
  address = {},
  month = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  misc
# # # For use when nothing else fits.
# # # Required fields: none
# # # Optional fields: author, title, howpublished, month, year, note, key

write.table(
'@misc{key,
  
  author = {},
  title = {},
  howpublished = {},
  month = {},
  year = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  phdthesis
# # # A Ph.D. thesis.
# # # Required fields: author, title, school, year
# # # Optional fields: type, address, month, note, key

write.table(
'@phdthesis{key,
  author = {},
  title = {},
  school = {},
  year = {},
  
  type = {},
  address = {},
  month = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  proceedings
# # # The proceedings of a conference.
# # # Required fields: title, year
# # # Optional fields: editor, volume/number, series, address, month, publisher, organization, note, key

write.table(
'@proceedings{key,
  title = {},
  year = {},
  
  editor = {},
  volume = {},
  number = {},
  series = {},
  address = {},
  month = {},
  publisher = {},
  organization = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  techreport
# # # A report published by a school or other institution, usually numbered within a series.
# # # Required fields: author, title, institution, year
# # # Optional fields: type, number, address, month, note, key

write.table(
'@techreport{key,
  author = {},
  title = {},
  institution = {},
  year = {},
  
  type = {},
  number = {},
  address = {},
  month = {},
  note = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






# #  unpublished
# # # A document having an author and title, but not formally published.
# # # Required fields: author, title, note
# # # Optional fields: month, year, key

write.table(
'@unpublished{key,
  author = {},
  title = {},
  note = {},
  
  month = {},
  year = {},
  key = {},
}'
  , file="test.bib", quote = FALSE, row.names = FALSE, col.names = FALSE)






