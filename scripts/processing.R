
#### parameters ####
p.import = "import/"
p.export = "export/"
p.categories = "categories/categories.csv"
p.filters = "filters/filters.csv"
p.cat_rules = "cat_rules/cat_rules.csv"

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

filters.add <- function(column = NULL, criterion = NULL, value = NULL){
  
  while(T){
    if(is.null(column)) column = readline("Enter column >>>")
    if(is.null(criterion)) criterion = readline("Enter criterion >>>")
    if(is.null(value)) value = readline("Enter value >>>")
    
    cat("confirm new filter rule:\n")
    cat(paste(column, criterion, value, sep = " "))
    cat("\nlines which evaluate to true will be removed")
    if(readline("y/n >>>") == "y"){
      break
    } 
  }
  
  rule = data.frame(column, criterion, value)
  write.table(rule, 
            p.filters, 
            append = T, 
            row.names = F, 
            col.names = F,
            sep = ",")
}

filters.load <- function(n){
  return(read.csv(p.filters,
                  stringsAsFactors = F))
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

cat_rules.add <- function(column = NULL, value = NULL, category = NULL){
  cat("Enter category rule")
  
  while(T){
    if(is.null(column)) column = readline("Enter column >>> ")
    if(is.null(value)) criterion = readline("Enter value >>> ")
    if(is.null(category)) value = readline("Enter target category >>> ")
    
    cat("confirm new categorization rule:\n")
    cat(paste(column, value, category, sep = " "))
    if(readline("y/n >>>") == "y"){
      break
    } 
  }
  
  df = data.frame(column, value, category)
  write.table(df,
            p.cat_rules,
            append = T,
            row.names = F,
            col.names = F,
            sep = ",")
}

cat_rules.load <- function(){
  cat_rules <- read.csv(p.cat_rules,
                        stringsAsFactors = F)
  return(cat_rules)
}

categorize <- function(data){
  result = data
  cat_rules <- cat_rules.load()
  
  for(row in 1:nrow(cat_rules)){
    result$Kategorie[result[cat_rules[row, "column"]] == cat_rules[row, "value"]] <- cat_rules[row, "category"]
  }
  
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

manual.show <- function(){
  cat("Show valid categories:  1\n")
  cat("Add category:           2\n")
  cat("Add filter rule:        3\n")
  cat("Add category rule:      4\n")
  
  while(T){
  selection <- readline("Enter option: >>>")
    if(selection %in% c("1")){
      categories.print()
      break
      
    } else if(selection %in% c("2")){
      categories.add()
      break
      
    } else if (selection %in% c("3")){
      filters.add()
      break
      
    } else if (selection %in% c("4")){
      cat_rules.add()
      break
      
    } else {
      cat("Selected invalid option")
    }
  }
}

categories.add <- function(category = NULL){
  
  if(is.null(category)){
    while(T){
      category = readline("Enter new category >>>")
      
      res = readline(paste("Confirm: '", category, "' (y/n) >>>", sep = ""))
      if(res == "y"){
        break
      } else if (res == "n"){
        
        res = readline("retype? (y/n) >>>")
        if(res != "y") return()
        
      } else cat("Wrong input. Retry!")  
    
    }    
  }
  
  df = data.frame(cat = category)
  write.table(df,
              p.categories,
              append = T,
              row.names = F,
              col.names = F,
              sep = ",")
}

categories.print <- function(){
  cat("Valid categories:")
  print(categories.load())
}

categories.load <- function(){
  return(read.csv(p.categories))
}

#' asks the user for a classification of the provided line
#'
line.classify <- function(line){
  if(!is.na(line["Kategorie"])) return() 
  
  cat("Please classify the following line:\n")
  line.print(line)
  
  valid = categories.load()
  
  while(T){
    category <- readline("Enter category: ")
    
    if(category %in% valid){
      return(category)
    } else if (category == "?"){
      manual.show()
      valid = categories.load()
      
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
