#' Capitalize first letter of a phrase
#' 

capitalize_first_letter <- function(phrase) {
  
  retstr <- NULL
  for (irow in 1:length(phrase)) {
    
    if (is.na(phrase[irow])) {
      str <- "NA"
    } else {
      
      s <- strsplit(tolower(phrase[irow]), "\\s+")[[1]]
      if (nchar(substring(s[1],2)) > 0 ) { # multiple letters
        if (length(s) == 1) { #single word
          str <- paste0(toupper(substring(s[1],1,1)),substring(s[1],2))
        } else {
          #  str <- paste(paste0(toupper(substring(s[1],1,1)),substring(s[1],2)),s[-1],sep=" ",collapse=" ")
          str <- capture.output(cat(paste0(toupper(substring(s[1],1,1)),substring(s[1],2)),s[-1]))
        }
      } else { # single letter
        
        str <- paste0(toupper(substring(s[1],1,1)),s[-1])
      }
    }
    
    retstr <- c(retstr,str)
  }
  
  return(retstr)
}