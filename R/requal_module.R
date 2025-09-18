# --- UI Function (Unchanged) ---
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        column(
            width = 3,
            fileInput(
                ns("file_input"),
                "Choose Text Files",
                multiple = TRUE,
                accept = c(
                    ".txt",
                    ".json",
                    ".csv",
                    ".tsv",
                    ".html",
                    ".xml",
                    ".pdf",
                    ".odt",
                    ".doc",
                    ".docx",
                    ".rtf"
                )
            ),
            checkboxInput(
                ns("pdf_stream"),
                "PDF raw stream order (recommended for multiple columns)",
                value = TRUE
            ),
            uiOutput(ns("text_selector")),
            div(
                id = ns("delete_div"),
                style = "display:none;",
                actionButton(ns("delete_button"), "Delete"),
                br(),
                br()
            ),
            div(
                id = ns("edit_buttons"),
                style = "display:none;",
                actionButton(
                    ns("save_button"),
                    "Save Changes",
                    class = "btn-success"
                ),
                actionButton(
                    ns("cancel_button"),
                    "Cancel",
                    class = "btn-secondary"
                )
            )
        ),
        column(
            width = 9,
            uiOutput(ns("preview_area"))
        )
    )
}

# --- Server Function (Modified) ---
mod_server <- function(id, api) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        loc <- reactiveValues()

        loc$text_df <- data.frame(
            document_id = integer(),
            document_name = character(),
            document_type = character(),
            document_path = character(),
            document_text = character(),
            document_description = character(),
            stringsAsFactors = FALSE
        )

        read_pdf_stream <- function(file_path) {
            pdftools::pdf_text(file_path, raw = TRUE)
        }

        observeEvent(input$file_input, {
            req(input$file_input)
            files <- input$file_input

            # Use a progress bar for user feedback during upload
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "Reading files...", value = 0)

            text_content_list <- list()
            for (i in 1:nrow(files)) {
                progress$inc(1 / nrow(files), detail = files$name[i])
                # Use readtext for all files
                temp_content <- readtext::readtext(files$datapath[i])
                text_content_list[[i]] <- temp_content
            }

            # Combine all readtext results
            text_content <- do.call(rbind, text_content_list)

            # Get current max ID so that new IDs can be assigned on top of it
            max_id <- if (nrow(loc$text_df) > 0) {
                max(loc$text_df$document_id)
            } else {
                0
            }

            # Construct the dataframe with readtext results
            new_data <- data.frame(
                document_id = (max_id + 1):(max_id + nrow(files)),
                document_name = files$name,
                document_type = files$type,
                document_path = files$datapath,
                document_text = enc2utf8(text_content$text),
                document_description = "",
                stringsAsFactors = FALSE
            )

            # If PDF stream processing is requested, replace PDF text
            if (input$pdf_stream == TRUE) {
                for (i in 1:nrow(new_data)) {
                    if (new_data$document_type[i] == "application/pdf") {
                        stream_content <- read_pdf_stream(new_data$document_path[
                            i
                        ])
                        # Convert to single string if multiple pages
                        if (length(stream_content) > 1) {
                            stream_content <- paste(
                                stream_content,
                                collapse = "\n\n"
                            )
                        }
                        new_data$document_text[i] <- enc2utf8(stream_content)
                    }
                }
            }

            loc$text_df <- rbind(loc$text_df, new_data)
            shinyjs::reset("file_input")
        })

        # Render UI for selecting text
        output$text_selector <- renderUI({
            if (nrow(loc$text_df) > 0) {
                choices <- c(
                    "Select a document..." = "",
                    setNames(
                        loc$text_df$document_id,
                        loc$text_df$document_name
                    )
                )
                selectInput(
                    ns("text_select"),
                    "Select Text",
                    choices = choices,
                    selected = ""
                )
            } else {
                selectInput(
                    ns("text_select"),
                    "Select Text",
                    choices = c("No documents available" = ""),
                    selected = ""
                )
            }
        })

        # Show/hide delete button based on selection
        observeEvent(input$text_select, {
            if (!is.null(input$text_select) && input$text_select != "") {
                shinyjs::show("delete_div")
            } else {
                shinyjs::hide("delete_div")
                shinyjs::hide("edit_buttons")
            }
        })

        output$preview_area <- renderUI({
            if (
                !is.null(input$text_select) &&
                    input$text_select != "" &&
                    nrow(loc$text_df) > 0
            ) {
                selected_text <- loc$text_df[
                    loc$text_df$document_id == as.integer(input$text_select),
                ]
                req(nrow(selected_text) > 0)

                # Show editable form with updateOn argument
                tagList(
                    h4("Document Preview"),
                    textInput(
                        ns("edit_title"),
                        "Document Name",
                        value = selected_text$document_name,
                        width = "100%",
                        updateOn = "blur"
                    ),
                    textAreaInput(
                        ns("edit_description"),
                        "Description",
                        value = selected_text$document_description,
                        rows = 3,
                        width = "100%",
                        resize = "vertical",
                        updateOn = "blur"
                    ),
                    textAreaInput(
                        ns("edit_content"),
                        "Content",
                        value = selected_text$document_text,
                        rows = 100,
                        width = "100%",
                        height = "100%",
                        resize = "vertical",
                        updateOn = "blur"
                    )
                )
            } else {
                # Show empty state when no selection
                tagList(
                    h4("Document Preview"),
                    p("Select a document to view and edit its content.")
                )
            }
        })

        # Show save/cancel buttons when any edit is made
        observeEvent(
            c(input$edit_title, input$edit_description, input$edit_content),
            {
                req(input$text_select, nrow(loc$text_df) > 0)
                # Show/hide appropriate buttons
                shinyjs::show("edit_buttons")
            }
        )

        observeEvent(input$save_button, {
            req(input$text_select)

            # Update the data frame
            row_index <- which(
                loc$text_df$document_id == as.integer(input$text_select)
            )
            loc$text_df[row_index, "document_name"] <- input$edit_title
            loc$text_df[
                row_index,
                "document_description"
            ] <- input$edit_description
            loc$text_df[row_index, "document_text"] <- input$edit_content

            # Update the select input choices
            updateSelectInput(
                session,
                "text_select",
                choices = stats::setNames(
                    loc$text_df$document_id,
                    loc$text_df$document_name
                ),
                selected = input$text_select
            )

            # Hide edit buttons after saving
            shinyjs::hide("edit_buttons")
        })

        observeEvent(input$cancel_button, {
            # Hide edit buttons after canceling
            shinyjs::hide("edit_buttons")
        })

        observeEvent(input$delete_button, {
            req(input$text_select, nrow(loc$text_df) > 0)

            # Remove the selected row
            loc$text_df <- loc$text_df[
                loc$text_df$document_id != as.integer(input$text_select),
            ]

            # Update select input
            if (nrow(loc$text_df) > 0) {
                updateSelectInput(
                    session,
                    "text_select",
                    choices = stats::setNames(
                        loc$text_df$document_id,
                        loc$text_df$document_name
                    )
                )
            }
        })
    })
}
