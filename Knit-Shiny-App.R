rmarkdown::render("Shiny-App-Background.Rmd", 
                  output_format = "github_document",
                  output_file = "README.md",
                  output_options = list(
                    df_print = "default",
                    toc = TRUE,
                    number_sections = FALSE,
                    keep_html = FALSE)
)