combine_HSIS_files <- function(hsis_folder, save_folder = NULL){
  
  if (is.null(save_folder)){
    save_folder = paste0(hsis_folder,"/","combined_data")
  }
  
  if (dir.exists(save_folder) == FALSE){
    dir.create(save_folder)
  }
  
  # get lists of file names and full file locations
  files <- list.files(hsis_folder, full.names = T)
  file_names <- list.files(hsis_folder, full.names = F)
  file_names <- grep(".csv",file_names,value = TRUE)
  
  # Find all sequences of digits (one or more)
  number_positions <- gregexpr("[[:digit:]]+", file_names)
  # Extract the matches
  numbers_list <- regmatches(file_names, number_positions)
  # Unlist to get a vector and convert to numeric
  numbers <- unique((as.numeric(unlist(numbers_list))))
  
  # split up by the numbers and get the unique endings of the files
  split_names <- strsplit(file_names,"[[:digit:]]+") |>
    sapply(function(x) x[[length(x)]]) |>
    unique()
  
  for (unique_name in split_names){
    selected_files <- grep(unique_name,files,value = TRUE)
    
    dfs <- list()
    for (file in selected_files){
      orgnl_file_name <- basename(file)
      
      df <- read.csv(file) |>
        dplyr::mutate(orgnl_file = orgnl_file_name)
      
      dfs[[orgnl_file_name]] <- df
    }
    
    combined_dfs <- dplyr::bind_rows(dfs)
    
    save_file <- paste0(save_folder, "/", "combined_",min(numbers),"_to_",max(numbers), "_",unique_name)
    write.csv(combined_dfs, save_file)
  }
  
}


