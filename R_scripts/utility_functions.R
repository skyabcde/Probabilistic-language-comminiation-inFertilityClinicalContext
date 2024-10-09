
parseItemStrings = function(x, task_a = 22, task_b= 31){
  #' Grab the terms from the participant's itemString
  terms = x$itemString %>% 
    strsplit(x = ., split = ";") %>%
    dplyr::first() %>%
    make.names()
  
  #' "Task A"
  #' Find the "old" column names, then construct the new ones
  #' using the terms
  old_namesA = x %>% 
    select(matches(glue::glue("Q{task_a}_"))) %>% 
    names()
  new_namesA = paste("task_a_", terms, sep="")
  
  # Ensure new names are unique and valid
  new_namesA = make.names(new_namesA, unique = TRUE)
  
  #' Rename the columns
  x %<>% 
    rename_with(~ new_namesA[which(old_namesA == .x)], .cols = all_of(old_namesA))
  
  #' "Task B"
  x %<>%
    mutate(across(matches(glue::glue("Q{task_b}_\\d*$")), as.character))
  
  #' Find the "old" column names, then construct the new ones
  #' using the terms.
  old_namesB = x %>% 
    select(matches(glue::glue("Q{task_b}_\\d*$"))) %>% 
    names()
  new_namesB = paste("task_b_", terms, sep="")
  
  
  #' The _DO_ columns give the display order, because the order was randomly determined
  old_namesB_DO = x %>% 
    select(matches(glue::glue("Q{task_b}_DO_\\d*$"))) %>% 
    names()
  new_namesB_DO = paste("Task_b_display_", terms, sep="")
  
  new_namesB = c(new_namesB, new_namesB_DO)
  old_namesB = c(old_namesB, old_namesB_DO)
  
  # Ensure new names are unique and valid
  new_namesB = make.names(new_namesB, unique = TRUE)
  
  #' Rename the columns
  x %<>% 
    rename_with(~ new_namesB[which(old_namesB == .x)], .cols = all_of(old_namesB))
  
  return(x)
}

