render_report <- function(input, output, params) {
  rmarkdown::render(
    input,
    output_file = output,
    params = params,
    envir = new.env(parent = globalenv()
    )
  )
}
