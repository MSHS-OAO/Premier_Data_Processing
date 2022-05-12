new_dept <- data_Epic_merge %>%
  filter(is.na(`Cost Center`)) %>%
  select(Department) %>%
  unique()

tru_new <- new_dept %>%
  filter(!(Department %in% dictionary_EPIC$Department))

remov_dept <- new_dept %>%
  filter(!(Department %in% remove_departments_Epic$Department))
