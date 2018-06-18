
#### parameters ####
p.import = "import/"
p.export = "export/"

#### functions ####

#' imports a csv file and converts it to dataframe
#' filters irrelevant columns
import <- function(){
 df <- read.csv(paste(p.import, "umsatz.csv", sep =""), 
                sep = ";", 
                header = T, 
                skip = 6, 
                fileEncoding = "UTF-16",
                stringsAsFactors = F)
 
 names(df) <- c("Buchungstag", 
                "Wertstellung", 
                "Buchungstext", 
                "Auftraggeber", 
                "Verwendungszweck", 
                "Kontonummer", 
                "BLZ", 
                "Betrag",
                "GlaeubigerID", 
                "Mandatsreferenz", 
                "Kundenreferenz", 
                "Notiz")

 return(df)
}

#' prints provided line to the terminal in structured manner
#'
print.line <- function(line, all = F){
  out = ""
  if(all){
    relevant = names(line)
    
  } else {
    relevant = c("Buchungstag",
                 "Buchungstext",
                 "Betrag",
                 "Verwendungszweck") 
  }
  
  for(e in relevant){
    field = paste(e, line[e], sep ="\t\t\t")
    out = paste(out, field, sep = "\n")
  }
  
  cat(out)
}


