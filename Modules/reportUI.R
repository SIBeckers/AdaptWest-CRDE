reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    prettyRadioButtons(
      inputId = ns("reportFormat"),
      label = "Report Format",
      choices = c("PDF","Word","Powerpoint","HTML","RMD"),
      inline = T,
      selected = "Word",
      status="success",
      shape="round",
      icon=icon("check"),
      bigger=T,
      animation="pulse"
    ),
    div(style="text-align: center",
      downloadBttn(
        outputId=ns("reportBttn"),
        style="bordered",
        color="primary",
        size="md",
        no_outline=F
      )
    )
  )
}