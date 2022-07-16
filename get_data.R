# Load the libraries
library(internetarchive)
library(tidyverse)
library(jsonlite)

# Get the Treetrunk data from IA
s_query <- c("collection" = "treetrunk")
treetrunk_names <- ia_search(s_query, num_results = 751)
treetrunk_list <- treetrunk_names %>% ia_get_items()

# Create an empty data.frame for Treetrunk
treetrunk_df <- data.frame(matrix(ncol = 5, nrow = length(treetrunk_names)))
colnames(treetrunk_df) <- c("Identifier", "Creator", "Title", "Downloads", "Date")

# Fill the data.frame with data. The metadata format changed a couple of times on IA
# between 2006 and 2013, or probably wasn't enforced strictly, producing
# 4 missing values for Creator. Impute with the help of Title.
for (i in seq_along(treetrunk_names)) {
  treetrunk_df$Identifier[i] <- treetrunk_list[[treetrunk_names[i]]][["metadata"]][["identifier"]][[1]]
  treetrunk_df$Downloads[i] <- treetrunk_list[[treetrunk_names[i]]][["item"]][["downloads"]]
  treetrunk_df$Title[i] <- treetrunk_list[[treetrunk_names[i]]][["metadata"]][["title"]][[1]]
  treetrunk_df$Date[i] <- treetrunk_list[[treetrunk_names[i]]][["metadata"]][["publicdate"]][[1]]
  if (!is.null(treetrunk_list[[treetrunk_names[i]]][["metadata"]][["creator"]][[1]])) {
    treetrunk_df$Creator[i] <- treetrunk_list[[treetrunk_names[i]]][["metadata"]][["creator"]][[1]]
  } else {
    treetrunk_str <- treetrunk_list[[treetrunk_names[i]]][["metadata"]][["title"]][[1]] %>%
      str_split("-")
    treetrunk_df$Creator[i] <- treetrunk_str[[1]][[1]]
  }
}

# Save the Treetrunk data
write.csv(treetrunk_df, file = "treetrunk_data.csv")
