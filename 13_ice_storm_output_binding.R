require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()

# binding ise files
data_path <- "./summary/ise"
files <- list.files("./summary/ise")

files %>%
  map(read_csv) %>%    # read in all the files individually, using
  # the function read_csv() from the readr package
  reduce(rbind) %>%
  select(-X1) -> data# reduce with rbind into one dataframe


data <-data.frame(data)


# data <- data_frame(filename = files) %>% # create a data frame
#   # holding the file names
#   mutate(file_contents = map(filename,          # read files into
#                              ~ read_csv(file.path(data_path, .))) # a new data column
#   )  
# data
# 
# data %>% 
#   unnest(data) -> df
# 
# df %>%
#   select(-c(X1, plot)) -> df

df <- data.frame(df)


# binding ise files
data_path <- "./summary/ise/output_vai_dfz"
files <- dir(data_path, pattern = "*.csv")
files

data <- files %>%
  # read in all the files, appending the path before the filename
  map(~ read_csv(file.path(data_path, .))) %>% 
  reduce(rbind)
data
data <-data.frame(data) 
write.csv(data, "./summary/ise/ice_storm_vaip_profiles_data.csv")