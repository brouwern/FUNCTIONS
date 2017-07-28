save.my.file <- function(object., 
                         abs.root. = abs.root.,
                         rel.dir. = rel.dir., 
                         file.name.,
                         extension){
  full.name <- paste(abs.root.,
                     rel.dir.,
                     file.name.,
                     sep = "/")
  full.name <- paste(full.name, 
                     extension, 
                     sep = ".")
  
  full.name <- gsub("\\.\\.",".",full.name)
  
  print(full.name)
  if(extension == ".csv" | extension == "csv"){
    write.csv(object., file = full.name, row.names = F)
  }
  
  if(extension == ".Rdata" | extension == "Rdata"){
    save(object., file = full.name)
  }
  
}