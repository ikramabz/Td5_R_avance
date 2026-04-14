## code to prepare `df_velo` dataset goes here



df_velo <- readr::read_delim("C:\\Users\\ikram\\Downloads\\base_velo_td5.csv",
delim = ";")

usethis::use_data(df_velo, overwrite = TRUE)
