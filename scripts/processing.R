
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

  df$Betrag <- sub(".", "", df$Betrag, fixed = TRUE)
  df$Betrag <- sub(",", ".", df$Betrag, fixed = TRUE)
  df$Betrag <- as.numeric(df$Betrag)
  
  df$Kategorie <- NA
  
  return(df)
}

preprocess <- function(){
  data <- import()
  data <- filter(data)
  data <- categorize(data)
  
  return(data)
}

filters.add <- function(column, criterion, value){
  rule = data.frame(column, criterion, value)
  write.table(rule, 
            "filters/filters.csv", 
            append = T, 
            col.names = F, 
            row.names = F)
}

filters.load <- function(n){
  return(read.csv("filters/filters.csv",
                  stringsAsFactors = F))
}

filters.remove <- function(column, criterion, value){
  
}

#' filtering
#' 
filter <-function(data){
  result <- data
  
  # import filter rules
  filters <- filters.load()  
  
  for(row in 1:nrow(filters)){
    column = filters[row, "column"]
    criterion = filters[row, "criterion"]
    value = filters[row, "value"]

    if(criterion %in% c("equal", "gleich", "==")){
      result = result[ !(result[column] == value) ,]
      
    } else if (rule %in% c("not equal", "nicht gleich", "!=")) {
      result = result[ !(result[column] != value) ,]
      
    } else if (rule %in% c("greater", "größer", ">")) {
      result = result[ !(result[column] > value) ,]
      
    } else if (rule %in% c("smaller", "kleiner", "<")) {
      result = result[ !(result[column] < value) ,]
      
    } else {
      
    }
  }
  
  return(result)
}

cat_rules.add <- function(column, value, category){
  df = data.frame(column, value, category)
  write.table(df,
            "cat_rules/cat_rules.csv",
            append = T,
            row.names = F,
            col.names = F)
}

cat_rules.load <- function(){
  cat_rules <- read.csv("cat_rules/cat_rules.csv",
                        stringsAsFactors = F)
  return(cat_rules)
}

categorize <- function(data){
  result = data
  cat_rules <- cat_rules.load()
  
  for(row in 1:nrow(cat_rules)){
    result$Kategorie[result[cat_rules[row, "column"]] == cat_rules[row, "value"]] <- cat_rules[row, "category"]
  }
  
  #result$Kategorie[result$Verwendungszweck == "Haushaltsgeld"] <- "Haushaltsgeld"
  return(result)
}


#' prints provided line to the terminal in structured manner
#'
line.print <- function(line, all = F){
  out = ""
  if(all){
    relevant = names(line)
    
  } else {
    relevant = c("Buchungstag",
                 "Buchungstext",
                 "Betrag",
                 "Verwendungszweck",
                 "Auftraggeber") 
  }
  
  for(e in relevant){
    field = paste(e, line[e], sep ="\t\t\t")
    out = paste(out, field, sep = "\n")
  }
  
  cat(out)
}

#' asks the user for a classification of the provided line line
#'
line.classify <- function(line){
  if(!is.na(line["Kategorie"])) return() 
  
  cat("Please classify the following line:\n")
  line.print(line)
  
  valid = c("Essen", 
            "Freizeit", 
            "Miete",
            "Sonstiges",
            "Foto",
            "Mobilitaet"
            )
  
  while(T){
    category <- readline("Enter category: ")
    
    if(category %in% valid){
      return(category)
    } else if (category == "?"){
      cat("Those are valid categories:\n")
      cat(valid, sep = "\n")
    } else{
      cat("You entered an invalid category. Try again or type '?' for help!")
    }
  }
}

#' classify all references
#' 



main <- function(){
  data <- preprocess()
  data <- apply(data, 1, line.classify)
  
}

main()
