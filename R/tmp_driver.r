quarto::quarto_render(here::here("R", "tmp_control.qmd"), output_format = "html")

output_path <- here::here(
  "Reports",
  stringr::str_c("tmp_consensus_sample_comparison_", Sys.Date(), ".html")
)

if (fs::file_exists(output_path)) {
  fs::file_delete(output_path)
}

fs::file_move(
  here::here("R", "tmp_control.html"),
  output_path
)