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
        skip_if_offline()
        skip_on_ci()

        withr::with_tempdir({
          new_dir <- sprintf("test-%s", template_name)
          create_fn(folder_path = new_dir)

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

          status <- callr::r(
            func = function(proj) {
              renv::restore(project = proj, prompt = FALSE)
              renv::status(project = proj)
            },
            args = list(proj = new_dir)
          )
          expect_true(status$synchronized)
        })
      }
    )
  }
)
