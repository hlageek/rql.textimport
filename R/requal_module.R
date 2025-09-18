#' wordcloud UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList
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
                actionButton(
                    ns("parse_columns_button"),
                    "Attempt to Parse Multiple Column PDF",
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

#' wordcloud Server Functions
#'
#' @param id Internal parameters for {shiny}.
#' @param api API connection object.
#' @export
mod_server <- function(id, api) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns
        loc <- reactiveValues(edit_mode = FALSE)

        # Initialize text data frame
        loc$text_df <- data.frame(
            document_id = integer(),
            document_name = character(),
            document_type = character(),
            document_text = character(),
            document_description = character(),
            stringsAsFactors = FALSE
        )

        # Read text files and update reactive data frame
        observeEvent(input$file_input, {
            req(input$file_input)
            files <- input$file_input$datapath

            # Detect encoding for each file and read with proper encoding
            text_content_list <- list()

            for (i in seq_along(files)) {
                # Detect encoding first
                detected_encoding <- readtext::encoding(files[i])

                # Handle case where encoding returns multiple values or NULL
                if (
                    is.null(detected_encoding) || length(detected_encoding) == 0
                ) {
                    file_encoding <- "UTF-8"
                } else {
                    # Take the first encoding if multiple are returned
                    file_encoding <- detected_encoding[1]
                    # Handle empty string or invalid encoding
                    if (is.na(file_encoding) || file_encoding == "") {
                        file_encoding <- "UTF-8"
                    }
                }

                # Read with detected encoding
                temp_content <- readtext::readtext(
                    files[i],
                    encoding = file_encoding
                )

                # Ensure UTF-8 conversion
                temp_content$text <- enc2utf8(temp_content$text)

                text_content_list[[i]] <- temp_content
            }

            # Combine all text content
            text_content <- do.call(rbind, text_content_list)

            # Get current max ID to avoid conflicts
            max_id <- if (nrow(loc$text_df) > 0) {
                max(loc$text_df$document_id)
            } else {
                0
            }

            # Create new data
            new_data <- data.frame(
                document_id = (max_id + 1):(max_id + length(files)),
                document_name = input$file_input$name,
                document_type = input$file_input$type,
                document_text = text_content$text,
                document_description = "", # Add a description column
                stringsAsFactors = FALSE
            )

            # Append to existing data
            loc$text_df <- rbind(loc$text_df, new_data)
        })

        # Render UI for selecting text
        output$text_selector <- renderUI({
            req(nrow(loc$text_df) > 0)

            selectInput(
                ns("text_select"),
                "Select Text",
                choices = stats::setNames(
                    loc$text_df$document_id,
                    loc$text_df$document_name
                )
            )
        })

        # Render text display header
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

        # Display selected text content as HTML or editable form
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

        # Show/hide PDF tools based on document type
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

        # Toggle edit mode when edit button is clicked
        observeEvent(input$edit_button, {
            req(input$text_select, nrow(loc$text_df) > 0)
            loc$edit_mode <- TRUE

            # Show/hide appropriate buttons
            shinyjs::hide("edit_button")
            shinyjs::hide("delete_button")
            shinyjs::show("edit_buttons")
        })

        # Save edits and update data frame
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

        # Cancel edit mode
        observeEvent(input$cancel_button, {
            loc$edit_mode <- FALSE

            # Show/hide appropriate buttons
            shinyjs::show("edit_button")
            shinyjs::show("delete_button")
            shinyjs::hide("edit_buttons")
            shinyjs::hide("pdf_tools")
        })

        # Reset edit mode when text selection changes
        observeEvent(input$text_select, {
            loc$edit_mode <- FALSE

            # Show/hide appropriate buttons
            shinyjs::show("edit_button")
            shinyjs::show("delete_button")
            shinyjs::hide("edit_buttons")
            shinyjs::hide("pdf_tools")
        })

        # Handle PDF column parsing
        observeEvent(input$parse_columns_button, {
            req(input$edit_content)

            # Apply parsing function to current content
            parsed_content <- parse_multiple_columns(input$edit_content)

            # Update the textarea with parsed content
            updateTextAreaInput(session, "edit_content", value = parsed_content)
        })

        # Delete selected document
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
    })
}


#' Parses a PDF using coordinate data for accurate column and layout reconstruction.
#'
#' This function leverages the precise x/y coordinates of text within a PDF
#' to robustly reconstruct the correct reading order, accurately handling
#' multi-column layouts, tables, and mixed-structure documents. It requires
#' the 'pdftools' and 'dplyr' packages.
#'
#' @param file_path The path to the PDF file.
#' @param num_columns The expected number of columns for layout detection.
#'   This helps in determining the column boundaries. Defaults to 2.
#' @param keep_empty_lines A boolean to control whether to preserve vertical gaps
#'   (approximated empty lines) between text blocks. Defaults to TRUE.
#'
#' @return A single character string with the text correctly ordered.

parse_pdf_coordinates <- function(
    file_path,
    num_columns = 2,
    keep_empty_lines = TRUE
) {
    # 1. DATA EXTRACTION
    # ------------------
    # Use pdf_data() to get a list of data frames, one per page, with text and coordinates.
    # We bind them all into a single data frame for processing.
    data <- do.call(rbind, pdf_data(file_path))

    if (nrow(data) == 0) {
        return("")
    }

    # Get the median page width to define column boundaries
    page_width <- median(data$space_w) # space_w is page width in pdftools

    # 2. COLUMN DETECTION & ASSIGNMENT
    # --------------------------------
    # Determine column breaks based on page width.
    # For 2 columns, the break is at the halfway point. For 3, at 1/3 and 2/3, etc.
    column_breaks <- (1:(num_columns - 1)) * (page_width / num_columns)

    # Assign each piece of text to a column based on its horizontal position (x-coordinate).
    # The 'findInterval' function is perfect for this.
    data$column <- findInterval(data$x, column_breaks) + 1

    # 3. RECONSTRUCTION OF READING ORDER
    # ----------------------------------
    # The correct reading order is page by page, then column by column, then by
    # vertical position (y), and finally horizontal position (x) for words on the same line.
    # We use dplyr's arrange() for a clean and readable sort.
    data_ordered <- data %>%
        arrange(page_num, column, y, x)

    # 4. TEXT ASSEMBLY
    # ----------------
    # Rebuild the text from the sorted data frame.
    final_text <- ""
    last_y <- -1
    last_page <- 1

    # We add a buffer to the y-coordinate comparison to handle slight misalignments.
    line_height_buffer <- median(data$height) * 0.5

    for (i in 1:nrow(data_ordered)) {
        row <- data_ordered[i, ]

        # Check for a new page or a significant vertical jump in the same column
        is_new_line <- (row$page_num != last_page) ||
            (row$y > last_y + line_height_buffer)

        if (is_new_line && i > 1) {
            # Add newlines. If keep_empty_lines is TRUE, add extra newlines for large gaps.
            if (keep_empty_lines) {
                # Calculate the vertical gap since the last line of text
                gap <- row$y - last_y
                # A typical line height is the median height of characters
                median_line_height <- median(data$height)
                # If the gap is larger than 2x the median line height, add an extra newline
                if (
                    gap > (median_line_height * 2) && row$page_num == last_page
                ) {
                    final_text <- paste0(final_text, "\n\n")
                } else {
                    final_text <- paste0(final_text, "\n")
                }
            } else {
                final_text <- paste0(final_text, "\n")
            }
        } else if (i > 1) {
            final_text <- paste0(final_text, " ")
        }

        final_text <- paste0(final_text, row$text)
        last_y <- row$y
        last_page <- row$page_num
    }

    return(final_text)
}
