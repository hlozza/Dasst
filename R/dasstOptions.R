### Global options for the Dasst package
###
### These options are set especially for the DSSAT data structure.
### The Dasst package may be used in other fix format environments
### by means of the edition of these options.

.dasstOptions <- new.env(FALSE, globalenv())

assign("sectionSign", "^\\*", envir = .dasstOptions)
assign("headerSep", " ", envir = .dasstOptions)
assign("headerSign", "^@", envir = .dasstOptions)

### Ignore empty lines (^$), blank lines (^ +$), 
### and beginning with exclamation (^!)
assign("ignoreSign", "^$|^ +$|^!", envir = .dasstOptions)

assign("pathSep", "/", envir = .dasstOptions)

assign("decimalPoint", ".", envir = .dasstOptions)

