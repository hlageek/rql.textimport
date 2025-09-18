# --- Prerequisites ---
# Ensure these packages are installed:
# install.packages(c("shiny", "shinyjs", "readtext", "pdftools", "dplyr", "stats"))

library(shiny)
library(shinyjs)
library(readtext)
library(pdftools)
library(dplyr)
library(stats)

# --- Helper Function: Coordinate-Based PDF Parser ---
# This is the advanced parser from our previous discussion.
# It should be available to the server function.
parse_pdf_coordinates <- function(
    file_path,
    num_columns = 2,
    keep_empty_lines = TRUE
) {
    pages_text <- pdf_text(file_path, raw = TRUE)

    # Combine pages into a single text block
    combined_text <- paste(pages_text, collapse = "\n\n")

    return(combined_text)
}


# --- UI Function (Unchanged) ---
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        column(
            width = 4,
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
            uiOutput(ns("text_selector")),
            actionButton(ns("edit_button"), "Edit"),
            actionButton(ns("delete_button"), "Delete"),
            br(),
            br(),
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
            ),
            br(),
            div(
                id = ns("pdf_tools"),
                style = "display:none;",
                # Changed the button label for clarity
                actionButton(
                    ns("parse_columns_button"),
                    "Re-parse PDF with pdftools",
                    class = "btn-info btn-sm"
                )
            )
        ),
        column(
            width = 8,
            uiOutput(ns("text_display")),
            uiOutput(ns("content_area"))
        )
    )
}

# --- Server Function (Modified) ---
mod_server <- function(id, api) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        loc <- reactiveValues(edit_mode = FALSE)

        # **CHANGE 1: Added 'document_path' to store the original file location**
        loc$text_df <- data.frame(
            document_id = integer(),
            document_name = character(),
            document_type = character(),
            document_path = character(), # <-- NEW COLUMN
            document_text = character(),
            document_description = character(),
            stringsAsFactors = FALSE
        )

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

                # The readtext function is used here for initial text extraction
                temp_content <- readtext::readtext(files$datapath[i])
                temp_content$text <- enc2utf8(temp_content$text)
                text_content_list[[i]] <- temp_content
            }
            text_content <- do.call(rbind, text_content_list)

            max_id <- if (nrow(loc$text_df) > 0) {
                max(loc$text_df$document_id)
            } else {
                0
            }

            # **CHANGE 2: Save the 'datapath' for each file**
            new_data <- data.frame(
                document_id = (max_id + 1):(max_id + nrow(files)),
                document_name = files$name,
                document_type = files$type,
                document_path = files$datapath, # <-- SAVING THE PATH
                document_text = text_content$text,
                document_description = "",
                stringsAsFactors = FALSE
            )
            loc$text_df <- rbind(loc$text_df, new_data)
        })

        # Render UI for selecting text (unchanged)
        output$text_selector <- renderUI({
            req(nrow(loc$text_df) > 0)
            selectInput(
                ns("text_select"),
                "Select Text",
                choices = setNames(
                    loc$text_df$document_id,
                    loc$text_df$document_name
                )
            )
        })

        # ... (Other UI rendering parts remain the same) ...
        # (text_display and content_area rendering are unchanged)
        output$text_display <- renderUI({
            req(input$text_select, nrow(loc$text_df) > 0)
            selected_text <- loc$text_df[
                loc$text_df$document_id == as.numeric(input$text_select),
            ]
            req(nrow(selected_text) > 0)

            tagList(
                h3(selected_text$document_name),
                p(selected_text$document_description)
            )
        })
        output$content_area <- renderUI({
            req(input$text_select, nrow(loc$text_df) > 0)
            selected_text <- loc$text_df[
                loc$text_df$document_id == as.numeric(input$text_select),
            ]
            req(nrow(selected_text) > 0)

            if (loc$edit_mode) {
                # Show editable form
                tagList(
                    h4("Edit Document"),
                    textInput(
                        ns("edit_title"),
                        "Document Name",
                        value = selected_text$document_name,
                        width = "100%"
                    ),
                    textAreaInput(
                        ns("edit_description"),
                        "Description",
                        value = selected_text$document_description,
                        rows = 3,
                        width = "100%",
                        resize = "vertical"
                    ),
                    textAreaInput(
                        ns("edit_content"),
                        "Content",
                        value = selected_text$document_text,
                        rows = 20,
                        width = "100%",
                        height = "500px",
                        resize = "vertical"
                    )
                )
            } else {
                # Show preview with preserved whitespace
                tagList(
                    tags$pre(
                        style = "white-space: pre-wrap; font-family: inherit; 
                                word-wrap: break-word; background: none; 
                                border: none; padding: 0; margin: 0;",
                        selected_text$document_text
                    )
                )
            }
        })
        observe({
            req(input$text_select, nrow(loc$text_df) > 0)
            selected_text <- loc$text_df[
                loc$text_df$document_id == as.numeric(input$text_select),
            ]

            if (
                nrow(selected_text) > 0 &&
                    loc$edit_mode &&
                    grepl(
                        "pdf|application/pdf",
                        selected_text$document_type,
                        ignore.case = TRUE
                    )
            ) {
                shinyjs::show("pdf_tools")
            } else {
                shinyjs::hide("pdf_tools")
            }
        })
        observeEvent(input$edit_button, {
            req(input$text_select, nrow(loc$text_df) > 0)
            loc$edit_mode <- TRUE

            # Show/hide appropriate buttons
            shinyjs::hide("edit_button")
            shinyjs::hide("delete_button")
            shinyjs::show("edit_buttons")
        })
        observeEvent(input$save_button, {
            req(input$text_select)

            # Update the data frame
            row_index <- which(
                loc$text_df$document_id == as.numeric(input$text_select)
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

            # Exit edit mode
            loc$edit_mode <- FALSE

            # Show/hide appropriate buttons
            shinyjs::show("edit_button")
            shinyjs::show("delete_button")
            shinyjs::hide("edit_buttons")
            shinyjs::hide("pdf_tools")
        })
        observeEvent(input$cancel_button, {
            loc$edit_mode <- FALSE

            # Show/hide appropriate buttons
            shinyjs::show("edit_button")
            shinyjs::show("delete_button")
            shinyjs::hide("edit_buttons")
            shinyjs::hide("pdf_tools")
        })
        observeEvent(input$text_select, {
            loc$edit_mode <- FALSE

            # Show/hide appropriate buttons
            shinyjs::show("edit_button")
            shinyjs::show("delete_button")
            shinyjs::hide("edit_buttons")
            shinyjs::hide("pdf_tools")
        })
        observeEvent(input$delete_button, {
            req(input$text_select, nrow(loc$text_df) > 0)

            # Remove the selected row
            loc$text_df <- loc$text_df[
                loc$text_df$document_id != as.numeric(input$text_select),
            ]

            # Update select input or hide if no documents remain
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

        # **CHANGE 3: Reworked the entire PDF parsing logic**
        observeEvent(input$parse_columns_button, {
            req(input$text_select)
            shinyjs::disable("parse_columns_button") # Disable button during processing

            # Find the original file path from our data frame
            selected_doc <- loc$text_df[
                loc$text_df$document_id == input$text_select,
            ]
            file_path <- selected_doc$document_path

            # Check if the temporary file still exists
            if (is.null(file_path) || !file.exists(file_path)) {
                showNotification(
                    "Original file not found. Please re-upload the PDF.",
                    type = "error"
                )
                shinyjs::enable("parse_columns_button")
                return()
            }

            showNotification(
                "Parsing PDF with pdftools... this may take a moment.",
                type = "message"
            )

            # Use tryCatch to handle potential errors from the parsing function
            parsed_content <- tryCatch(
                {
                    parse_pdf_coordinates(file_path)
                },
                error = function(e) {
                    showNotification(
                        paste("PDF parsing failed:", e$message),
                        type = "error"
                    )
                    return(NULL) # Return NULL on failure
                }
            )

            # If parsing was successful, update the editor
            if (!is.null(parsed_content)) {
                updateTextAreaInput(
                    session,
                    "edit_content",
                    value = parsed_content
                )
                showNotification(
                    "PDF successfully re-parsed!",
                    type = "message"
                )
            }

            shinyjs::enable("parse_columns_button") # Re-enable the button
        })
    })
}
