#!/usr/bin/env Rscript

# Analysis A3_2: score HRS Core 2016-2022 with the fixed HCAP16 PMM (Analysis A3).
# Requires Analysis A0 (R_objects/A0_030_hrs16_22_merged.rds) and Analysis A3
# (R_objects/PMM_100.RDS, mplus_output/pmm_103*/pmm_hcap_103b*.inp/.out).

setwd(here::here())

render_target <- here::here("R", "A3_2", "A3_2_000-Main_control.qmd")
rendered_output <- here::here("R", "A3_2", "A3_2_000-Main_control.html")
final_output <- here::here("Reports", stringr::str_c("A3_2_PMM_scoring_", Sys.Date(), ".html"))

fs::dir_create(here::here("Reports"))

quarto::quarto_render(render_target, output_format = "html")

if (fs::file_exists(final_output)) {
  fs::file_delete(final_output)
}

fs::file_move(rendered_output, final_output)

message("Rendered analysis A3_2 to: ", final_output)
