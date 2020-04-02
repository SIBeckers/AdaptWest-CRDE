reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("selPolys")),
    div(style="display:inline-block",
      prettyRadioButtons(
        inputId = ns("reportFormat"),
        label = "Report Format",
        choices = c("PDF","Word","HTML"),
        inline = T,
        selected = "HTML",
        status="success",
        shape="round",
        icon=icon("check"),
        bigger=T,
        animation="pulse",
        width="100%"
      )
    ),
    div(style="display:inline-block",
      conditionalPanel(
        condition="input.reportFormat=='HTML'",
        ns=ns,
        switchInput(
          inputId=ns("reportInteractive"),
          label="Interactive HTML?",
          value=T,
          onLabel="YES",
          offLabel="NO",
          onStatus="primary",
          offStatus="default",
          labelWidth="50%",
          inline=T,
          width="100%"
        )
      )
    ),
    div(style="text-align: center",
      downloadBttn(
        outputId=ns("reportBttn"),
        style="unite",
        color="primary",
        size="md",
        no_outline=F,
        block=T
      )
    )
  )
}