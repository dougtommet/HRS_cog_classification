

quarto::quarto_render(here::here("R", "000-master.qmd"), output_format = "html")


fs::file_move(here::here("R", "000-master.html"),
              here::here("Reports", stringr::str_c("HRS_cognition_", Sys.Date(),".html")))


# quarto::quarto_render(here::here("R", "HRS-HCAP Algorithm in HRS Core 2024-12-19.qmd"), output_format = "html")
# fs::file_move(here::here("R", "HRS-HCAP Algorithm in HRS Core 2024-12-19.html"),
#               here::here("Reports", stringr::str_c("HRS_presentation_2024-12-19_", Sys.Date(),".html")))

fs::file_move(here::here("R", "PMM_000_Analysis_Report_Control.html"),
              here::here("Reports", stringr::str_c("PMM_Analysis_Report_", Sys.Date(),".html")))

