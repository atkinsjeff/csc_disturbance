data_dir <- "./output/ise"


file.names <- dir(data_dir, pattern =".csv")


#for loop that moves through files in directory
for(i in 1:length(file.names)){
  filename <- file.names[i]
  
  f <- file.path(data_dir, filename)
  
  df <- read.csv(f)
  
  df.z <- stats::aggregate(vai ~ zbin, data = df, FUN = mean)
  df.z$plot <- filename
  
  outputname = substr(filename, 1, nchar(filename) - 18)
  outputname <- paste(outputname, "output.csv", sep = "_")
  output_directory <- ("./summary/ise")
  write.csv(df.z, file.path(output_directory, outputname))
}