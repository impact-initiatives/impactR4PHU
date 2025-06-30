library(testthat)
library(withr)
library(fs)
library(purrr)
library(callr)

rstudio_templates <- list(
  fsl_quality_report = create_fsl_quality_report_template
)

purrr::iwalk(
  rstudio_templates,
  ~ {
    template_name <- .y
    create_fn <- .x

    test_that(
      sprintf("[%s] template scaffolds .Rprofile and renv", template_name),
      {
        withr::with_tempdir({
          src <- system.file(template_name, package = "impactR4PHU")
          expect_true(
            nzchar(src) && dir_exists(src),
            info = "Template dir must be installed under system.file()"
          )

          create_fn(folder_path = ".")

          new_dir <- path_file(src)
          expect_true(
            dir_exists(new_dir),
            info = paste0("Template must scaffold a ", new_dir)
          )

          expect_true(
            file_exists(path(new_dir, ".Rprofile")),
            info = ".Rprofile must be present after scaffold"
          )

          # 5) smoke–test renv skeleton
          expect_true(dir_exists(path(new_dir, "renv")))
          expect_true(file_exists(path(new_dir, "renv.lock")))

          # TODO ignore on cran with testthat::skip_on_cran()
          if (interactive() && nzchar(Sys.getenv("NOT_CRAN"))) {
            status <- callr::r(
              func = function(proj) {
                renv::restore(project = proj, prompt = FALSE)
                renv::status(project = proj)
              },
              args = list(proj = new_dir),
              timeout = 60 * 1000
            )

            expect_true(
              status$synchronized
            )
          } else {
            skip("Skipping slow renv restore/status under R CMD check")
          }
        })
      }
    )
  }
)
