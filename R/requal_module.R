# --- UI Function ---
mod_ui <- function(id) {
    ns <- NS(id)
    tagList(
        shinyjs::useShinyjs(),
        column(
            width = 3,
            wellPanel(
                # Top-level format selector
                selectInput(
                    ns("import_format"),
                    "Import Format",
                    choices = c(
                        "Document files" = "documents",
                        "File Paths & Wildcards" = "filepaths",
                        "Structured file" = "structured_file",
                        "Web URLs" = "urls"
                    ),
                    selected = "documents"
                ),

                # Dynamic input based on format selection
                uiOutput(ns("dynamic_input")),

                # Encoding options (not for URLs)
                conditionalPanel(
                    condition = sprintf(
                        "input['%s'] != 'urls'",
                        ns("import_format")
                    ),
                    create_encoding_panel(ns)
                ),

                # Field specification for structured formats
                uiOutput(ns("field_inputs")),

                # PDF stream option
                conditionalPanel(
                    condition = sprintf(
                        "['documents','filepaths','archives'].includes(input['%s'])",
                        ns("import_format")
                    ),
                    checkboxInput(
                        ns("pdf_stream"),
                        "PDF raw stream order (recommended for multiple columns)",
                        value = TRUE
                    )
                ),
                # Process button
                actionButton(
                    ns("process_input"),
                    "Process input",
                    class = "btn-primary",
                    width = "100%"
                )
            )
        ),
        column(
            width = 9,
            uiOutput(ns("text_selector")),
            create_action_buttons(ns),
            div(
                style = "height: 100vh; overflow-y: auto;",
                uiOutput(ns("preview_area"))
            )
        )
    )
}

# --- Server Function ---
mod_server <- function(id, api) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Initialize reactive values
        loc <- reactiveValues(
            text_df = create_empty_dataframe(),
            original_values = list(title = "", description = "", content = ""),
            pending_file = NULL, # Store file info while waiting for field selection
            modal_fields = character(0)
        )

        # --- Dynamic UI Outputs ---
        output$dynamic_input <- renderUI({
            switch(
                input$import_format,
                "documents" = create_document_input(ns),
                "filepaths" = create_filepath_input(ns),
                "structured_file" = create_structured_file_input(ns),
                "archive" = create_archive_input(ns),
                "urls" = create_url_input(ns)
            )
        })

        output$field_inputs <- renderUI({
            # Remove the field inputs from the sidebar since we'll use modal instead
            return(NULL)
        })

        output$text_selector <- renderUI({
            choices <- if (nrow(loc$text_df) > 0) {
                c(
                    "Select a document..." = "",
                    setNames(loc$text_df$document_id, loc$text_df$document_name)
                )
            } else {
                c("No documents available" = "")
            }

            selectInput(
                ns("text_select"),
                "Select Text",
                choices = choices,
                selected = ""
            )
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

                tagList(
                    h4("Transcript Preview"),
                    textInput(
                        ns("edit_title"),
                        "Transcript Name",
                        value = selected_text$document_name,
                        width = "100%",
                        updateOn = "change"
                    ),
                    textAreaInput(
                        ns("edit_description"),
                        "Description",
                        value = selected_text$document_description,
                        rows = 3,
                        width = "100%",
                        resize = "vertical",
                        updateOn = "change"
                    ),
                    textAreaInput(
                        ns("edit_content"),
                        "Content",
                        value = selected_text$document_text,
                        width = "100%",
                        autoresize = TRUE,
                        resize = "vertical",
                        updateOn = "change"
                    )
                )
            } else {
                tagList(
                    p("Select a transcript to preview and edit its content.")
                )
            }
        })

        # --- Event Observers ---
        observeEvent(input$file_input, {
            req(input$file_input, input$import_format == "documents")
            loc$pending_file <- list(files = input$file_input)
        })
        observeEvent(input$process_input, {
            req(loc$pending_file)

            files <- loc$pending_file$files
            req(files)

            # Process files
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "Reading files...", value = 0)

            all_content <- list()
            for (i in 1:nrow(files)) {
                progress$inc(1 / nrow(files), detail = files$name[i])
                all_content[[i]] <- process_with_readtext(files$datapath[i])
            }

            combined_content <- do.call(rbind, all_content)
            add_documents_to_df(
                combined_content,
                files$name,
                files$type,
                files$datapath
            )

            if (input$pdf_stream) {
                handle_pdf_stream_processing()
            }

            loc$pending_file <- NULL
        })
        observeEvent(input$process_input, {
            req(input$file_input, input$import_format == "structured_file")

            files <- input$file_input

            # Check if the file needs field specification
            needs_specification <- needs_field_specification(
                files$datapath,
                files$type
            )

            # Show modal for the structured file
            if (needs_specification) {
                file_info <- detect_file_fields(files$datapath, files$type)

                if (file_info$success) {
                    # Store file info for processing after modal
                    loc$pending_file <- list(files = files)
                    loc$modal_fields <- file_info$fields

                    showModal(create_field_selection_modal(
                        ns,
                        file_info$fields,
                        files$name
                    ))
                } else {
                    showNotification(
                        paste(
                            "Could not detect fields in file:",
                            files$name,
                            if (!file_info$success) {
                                paste("Error:", file_info$error)
                            } else {
                                ""
                            }
                        ),
                        type = "warning"
                    )
                }
            } else {
                showNotification(
                    paste(
                        "Could not handle file:",
                        files$name
                    ),
                    type = "warning"
                )
            }

            shinyjs::reset("file_input")
        })

        # Modal event handlers for fields detection in structured files
        observeEvent(input$modal_process, {
            req(loc$pending_file, input$modal_text_field)

            files <- loc$pending_file$files # Use the stored files

            if (input$modal_text_field == "") {
                showNotification(
                    "Please select a text field.",
                    type = "warning"
                )
                return()
            }

            removeModal()

            files <- loc$pending_file$files
            text_field <- input$modal_text_field
            docid_field <- if (input$modal_docid_field == "") {
                NULL
            } else {
                input$modal_docid_field
            }

            # Process the file with selected fields
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(
                message = "Processing file with selected fields...",
                value = 0
            )

            all_content <- list()
            for (i in 1:nrow(files)) {
                progress$inc(1 / nrow(files), detail = files$name[i])
                all_content[[i]] <- process_with_readtext(
                    files$datapath[i],
                    text_field = text_field,
                    docid_field = docid_field
                )
            }

            combined_content <- do.call(rbind, all_content)
            add_documents_to_df(
                combined_content,
                files$name,
                files$type,
                files$datapath
            )

            # Handle PDF stream processing if needed
            if (input$pdf_stream) {
                handle_pdf_stream_processing()
            }

            # Clear pending file
            loc$pending_file <- NULL
            loc$modal_fields <- character(0)

            showNotification("File processed successfully!", type = "message")
            # Clear pending file
            loc$pending_file <- NULL
            loc$modal_fields <- character(0)

            showNotification("File processed successfully!", type = "message")
        })

        observeEvent(input$modal_cancel, {
            removeModal()
            loc$pending_file <- NULL
            loc$modal_fields <- character(0)
            showNotification("File import cancelled.", type = "message")
        })

        observeEvent(input$process_input, {
            req(input$filepath_input, input$import_format == "filepaths")

            filepaths <- trimws(strsplit(input$filepath_input, "\n")[[1]])
            filepaths <- filepaths[filepaths != ""]

            if (length(filepaths) == 0) {
                showNotification("No valid file paths found.", type = "warning")
                return()
            }

            process_filepaths(filepaths)
            updateTextAreaInput(session, "filepath_input", value = "")
        })

        observeEvent(input$process_input, {
            req(input$url_input, input$import_format == "urls")

            urls <- sanitize_urls(input$url_input)
            if (length(urls) == 0) {
                showNotification("No valid URLs found.", type = "warning")
                return()
            }

            process_urls(urls)
            updateTextAreaInput(session, "url_input", value = "")
        })

        # --- UI State Management ---
        observeEvent(input$text_select, {
            if (!is.null(input$text_select) && input$text_select != "") {
                shinyjs::show("delete_div")
                selected_text <- loc$text_df[
                    loc$text_df$document_id == as.integer(input$text_select),
                ]
                if (nrow(selected_text) > 0) {
                    loc$original_values <- list(
                        title = selected_text$document_name,
                        description = selected_text$document_description,
                        content = selected_text$document_text
                    )
                }
            } else {
                shinyjs::hide(c("delete_div", "edit_buttons"))
                loc$original_values <- list(
                    title = "",
                    description = "",
                    content = ""
                )
            }
        })

        observeEvent(
            c(input$edit_title, input$edit_description, input$edit_content),
            {
                req(input$text_select, input$text_select != "")

                changes_detected <- any(
                    !is.null(input$edit_title) &&
                        input$edit_title != loc$original_values$title,
                    !is.null(input$edit_description) &&
                        input$edit_description !=
                            loc$original_values$description,
                    !is.null(input$edit_content) &&
                        input$edit_content != loc$original_values$content
                )

                if (changes_detected) {
                    shinyjs::show("edit_buttons")
                } else {
                    shinyjs::hide("edit_buttons")
                }
            }
        )

        observeEvent(input$save_button, {
            req(input$text_select)

            row_index <- which(
                loc$text_df$document_id == as.integer(input$text_select)
            )
            loc$text_df[
                row_index,
                c("document_name", "document_description", "document_text")
            ] <-
                list(
                    input$edit_title,
                    input$edit_description,
                    input$edit_content
                )

            loc$original_values <- list(
                title = input$edit_title,
                description = input$edit_description,
                content = input$edit_content
            )

            updateSelectInput(
                session,
                "text_select",
                choices = c(
                    "Select a transcript..." = "",
                    setNames(loc$text_df$document_id, loc$text_df$document_name)
                ),
                selected = input$text_select
            )

            shinyjs::hide("edit_buttons")
            showNotification(
                "Transcript updated successfully!",
                type = "message"
            )
        })

        observeEvent(input$cancel_button, {
            selected_text <- loc$text_df[
                loc$text_df$document_id == as.integer(input$text_select),
            ]
            if (nrow(selected_text) > 0) {
                updateTextInput(
                    session,
                    "edit_title",
                    value = selected_text$document_name
                )
                updateTextAreaInput(
                    session,
                    "edit_description",
                    value = selected_text$document_description
                )
                updateTextAreaInput(
                    session,
                    "edit_content",
                    value = selected_text$document_text
                )
            }
            shinyjs::hide("edit_buttons")
        })

        observeEvent(input$delete_button, {
            req(input$text_select, nrow(loc$text_df) > 0)

            loc$text_df <- loc$text_df[
                loc$text_df$document_id != as.integer(input$text_select),
            ]

            if (nrow(loc$text_df) > 0) {
                updateSelectInput(
                    session,
                    "text_select",
                    choices = c(
                        "Select a document..." = "",
                        setNames(
                            loc$text_df$document_id,
                            loc$text_df$document_name
                        )
                    ),
                    selected = ""
                )
            }

            showNotification("Document deleted successfully!", type = "message")
        })

        # --- Module-scoped Helper Functions ---
        get_next_id <- function() {
            if (nrow(loc$text_df) > 0) max(loc$text_df$document_id) + 1 else 1
        }

        get_encoding <- function() {
            if (input$encoding_option == "auto") NULL else input$encoding_manual
        }

        process_with_readtext <- function(
            file_path,
            is_remote = FALSE,
            text_field = NULL,
            docid_field = NULL
        ) {
            args <- list(
                file = file_path,
                encoding = get_encoding(),
                verbosity = 1,
                cache = is_remote
            )

            # Add field specifications if provided
            if (!is.null(text_field) && text_field != "") {
                args$text_field <- text_field
            }
            if (!is.null(docid_field) && docid_field != "") {
                args$docid_field <- docid_field
            }

            tryCatch(
                {
                    do.call(readtext::readtext, args)
                },
                error = function(e) {
                    # Fallback to UTF-8 if auto-detection fails
                    if (is.null(args$encoding)) {
                        args$encoding <- "UTF-8"
                        tryCatch(
                            {
                                do.call(readtext::readtext, args)
                            },
                            error = function(e2) {
                                create_error_document(file_path, e2$message)
                            }
                        )
                    } else {
                        create_error_document(file_path, e$message)
                    }
                }
            )
        }

        add_documents_to_df <- function(
            new_docs,
            file_names = NULL,
            file_types = NULL,
            source_paths = NULL
        ) {
            if (nrow(new_docs) == 0) {
                return()
            }

            next_id <- get_next_id()
            num_docs <- nrow(new_docs)

            # Ensure text column is character and handle encoding safely
            text_content <- if (is.character(new_docs$text)) {
                iconv(new_docs$text, from = "UTF-8", to = "UTF-8", sub = "byte")
            } else {
                iconv(
                    as.character(new_docs$text),
                    from = "UTF-8",
                    to = "UTF-8",
                    sub = "byte"
                )
            }

            # Ensure we have unique transcript names
            if (length(file_names) != nrow(new_docs)) {
                file_names <- paste(
                    file_names,
                    seq_len(nrow(new_docs)),
                    sep = "_"
                )
            }
            new_data <- data.frame(
                document_id = next_id:(next_id + num_docs - 1),
                document_name = file_names %||% new_docs$doc_id,
                document_type = file_types %||% rep("text/plain", num_docs),
                document_path = source_paths %||% rep("", num_docs),
                document_text = text_content,
                document_description = "",
                stringsAsFactors = FALSE
            )

            loc$text_df <- rbind(loc$text_df, new_data)
        }

        handle_pdf_stream_processing <- function() {
            pdf_rows <- which(loc$text_df$document_type == "application/pdf")
            for (i in pdf_rows) {
                stream_content <- pdftools::pdf_text(
                    loc$text_df$document_path[i],
                    raw = TRUE
                )
                if (length(stream_content) > 1) {
                    stream_content <- paste(stream_content, collapse = "\n\n")
                }
                loc$text_df[i, "document_text"] <- iconv(
                    stream_content,
                    from = "UTF-8",
                    to = "UTF-8",
                    sub = "byte"
                )
            }
        }

        # Check if file needs field specification
        needs_field_specification <- function(file_path, file_type = NULL) {
            if (is.null(file_type)) {
                ext <- tolower(tools::file_ext(file_path))
            } else {
                ext <- tolower(file_type)
            }
            grepl("csv|tsv|json|xlsx|xls|xml", ext)
        }

        process_filepaths <- function(filepaths) {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "Processing file paths...", value = 0)

            all_docs <- list()
            for (i in seq_along(filepaths)) {
                progress$inc(1 / length(filepaths), detail = filepaths[i])
                content <- process_with_readtext(filepaths[i])
                if (nrow(content) > 0) {
                    content$source_path <- filepaths[i]
                    all_docs[[length(all_docs) + 1]] <- content
                }
            }

            if (length(all_docs) > 0) {
                combined <- do.call(rbind, all_docs)
                descriptions <- paste("From path:", combined$source_path)
                add_documents_to_df(
                    combined,
                    combined$doc_id,
                    rep("text/plain", nrow(combined)),
                    combined$source_path
                )

                # Update descriptions
                start_id <- get_next_id() - nrow(combined)
                for (i in 1:nrow(combined)) {
                    loc$text_df[
                        loc$text_df$document_id == (start_id + i - 1),
                        "document_description"
                    ] <- descriptions[i]
                }

                showNotification(
                    paste(
                        "Successfully processed",
                        nrow(combined),
                        "documents"
                    ),
                    type = "message"
                )
            } else {
                showNotification(
                    "No documents found matching the patterns.",
                    type = "warning"
                )
            }
        }

        process_urls <- function(urls) {
            progress <- shiny::Progress$new()
            on.exit(progress$close())
            progress$set(message = "Processing URLs...", value = 0)

            all_docs <- list()
            for (i in seq_along(urls)) {
                progress$inc(1 / length(urls), detail = urls[i])
                content <- process_with_readtext(urls[i], is_remote = TRUE)
                content$url <- urls[i]
                all_docs[[i]] <- content
            }

            combined <- do.call(rbind, all_docs)
            names <- paste("URL:", sapply(combined$url, basename))
            descriptions <- paste("Fetched from:", combined$url)

            add_documents_to_df(
                combined,
                names,
                rep("text/html", nrow(combined)),
                combined$url
            )

            # Update descriptions
            start_id <- get_next_id() - nrow(combined)
            for (i in 1:nrow(combined)) {
                loc$text_df[
                    loc$text_df$document_id == (start_id + i - 1),
                    "document_description"
                ] <- descriptions[i]
            }

            showNotification(
                paste("Successfully processed", length(urls), "URLs"),
                type = "message"
            )
        }

        # Return reactive data frame
        return(reactive({
            loc$text_df
        }))
    })
}
