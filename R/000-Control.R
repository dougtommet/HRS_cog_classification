

quarto::quarto_render(here::here("R", "000-master.qmd"), output_format = "html")


fs::file_move(here::here("R", "000-master.html"),
              here::here("Reports", stringr::str_c("HRS_cognition_", Sys.Date(),".html")))



