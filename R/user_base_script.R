# script to write down user credentials

# Author: Peter Boshe
# Version: 2023-06-19

# Packages
# library(sodium)

# Parameters

# ============================================================================

# dataframe that holds usernames, passwords and other user data
user_base <- tibble::tibble(
  user = c("admin", "user2", "user3", "user4"),
  # password = purrr::map_chr(c("admin12345", "user123", "user234", "user345"), sodium::password_store),
  password = c("admin12345", "user123", "user234", "user345"),
  permissions = c("admin", "standard", "standard", "standard"),
  name = c("User One", "User Two", "User Three", "User Four")
)

saveRDS(user_base, "user_base.rds")
