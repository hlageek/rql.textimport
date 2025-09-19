# --- Helper Functions (outside module scope) ---

# Function to detect file structure and available fields
detect_file_fields <- function(file_path, file_type = NULL) {
  tryCatch(
    {
      # Detect file type if not provided
      if (is.null(file_type)) {
        ext <- tools::file_ext(file_path)
        file_type <- switch(
          tolower(ext),
          "csv" = "text/csv",
          "tsv" = "text/tsv",
          "json" = "application/json",
          "xml" = "application/xml",
          "xlsx" = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "xls" = "application/vnd.ms-excel",
          "unknown"
        )
      }
      print(file_type)
      fields <- switch(
        file_type,
        "text/csv" = {
          # Read first few rows to get column names, handle files without headers
          sample_data <- utils::read.csv(
            file_path,
            nrows = 5,
            stringsAsFactors = FALSE,
            header = TRUE
          )
          colnames(sample_data)
        },
        "text/tsv" = {
          sample_data <- utils::read.delim(
            file_path,
            nrows = 5,
            stringsAsFactors = FALSE,
            header = TRUE
          )
          colnames(sample_data)
        },
        "application/json" = {
          # Read the entire JSON file to get field names
          json_content <- readLines(file_path, warn = FALSE)
          json_text <- paste(json_content, collapse = "\n")
          first_obj <- jsonlite::fromJSON(json_text)
          if (is.list(first_obj)) {
            names(first_obj)
          } else {
            character(0)
          }
        },
        "application/xml" = {
          # Use xml2 package to parse XML structure
          library(xml2)
          doc <- read_xml(file_path)
          # Extract field names using XPath or similar logic
          xml_fields <- xml_find_all(doc, "//*")
          unique(xml_name(xml_fields))
        },
        character(0)
      )

      return(list(success = TRUE, fields = fields, file_type = file_type))
    },
    error = function(e) {
      return(list(
        success = FALSE,
        error = e$message,
        fields = character(0)
      ))
    }
  )
}

create_empty_dataframe <- function() {
  data.frame(
    document_id = integer(),
    document_name = character(),
    document_type = character(),
    document_path = character(),
    document_text = character(),
    document_description = character(),
    stringsAsFactors = FALSE
  )
}

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

sanitize_urls <- function(url_text) {
  if (is.null(url_text) || url_text == "") {
    return(character(0))
  }

  urls <- strsplit(url_text, "\n")[[1]]
  urls <- trimws(urls[urls != ""])

  sanitized <- sapply(
    urls,
    function(url) {
      # Add protocol if missing
      if (!grepl("^https?://", url, ignore.case = TRUE)) {
        url <- paste0("https://", url)
      }

      # Remove trailing non-alphanumeric characters
      url <- gsub("[^[:alnum:]]+$", "", url)
      # Validate basic structure
      if (grepl("^https?://[[:alnum:].-]+", url, ignore.case = TRUE)) {
        url
      } else {
        NA
      }
    },
    USE.NAMES = FALSE
  )

  sanitized[!is.na(sanitized)]
}

create_error_document <- function(file_path, error_msg) {
  data.frame(
    doc_id = basename(file_path),
    text = paste("Error reading file:", error_msg),
    stringsAsFactors = FALSE
  )
}


# --- UI Helper Functions ---
create_encoding_panel <- function(ns) {
  wellPanel(
    style = "background-color: #f8f9fa; margin-top: 10px;",
    h5("Encoding Options"),
    radioButtons(
      ns("encoding_option"),
      NULL,
      choices = c(
        "Auto-detect encoding" = "auto",
        "Specify encoding" = "manual"
      ),
      selected = "auto",
      inline = FALSE
    ),
    conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'manual'",
        ns("encoding_option")
      ),
      selectInput(
        ns("encoding_manual"),
        "Encoding",
        choices = c(
          "UTF-8" = "UTF-8",
          "ISO-8859-1 (Latin-1)" = "ISO-8859-1",
          "Windows-1252" = "windows-1252",
          "UTF-16" = "UTF-16",
          "ASCII" = "ASCII"
        ),
        selected = "UTF-8"
      )
    )
  )
}

create_action_buttons <- function(ns) {
  div(
    id = ns("preview_buttons"),
    style = "display:flex;",
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
        "Update transcript",
        class = "btn-success"
      ),
      actionButton(ns("cancel_button"), "Cancel", class = "btn-secondary")
    )
  )
}

create_document_input <- function(ns) {
  div(
    fileInput(
      ns("file_input"),
      "Select or drag & drop document files",
      multiple = TRUE,
      accept = c(
        ".txt",
        ".html",
        ".pdf",
        ".odt",
        ".doc",
        ".docx",
        ".rtf"
      )
    )
  )
}

create_filepath_input <- function(ns) {
  tagList(
    textAreaInput(
      ns("filepath_input"),
      "Enter file paths or wildcards (one per line)",
      placeholder = "/path/to/documents/*.txt\n/path/to/data/*.json\n~/Documents/report.pdf",
      rows = 6,
      width = "100%"
    )
  )
}

create_structured_file_input <- function(ns) {
  fileInput(
    ns("file_input"),
    "Select a structured file",
    multiple = FALSE,
    accept = c(
      ".csv",
      ".tsv",
      ".xlsx",
      ".xls",
      ".json"
    )
  )
}

create_url_input <- function(ns) {
  tagList(
    textAreaInput(
      ns("url_input"),
      "Enter URLs (one per line)",
      placeholder = "https://example.com/example.html\nhttps://another-site.org/another-page.html",
      rows = 6,
      width = "100%"
    )
  )
}

create_field_selection_modal <- function(ns, fields, file_name) {
  modalDialog(
    title = paste("Configure Fields for", file_name),
    size = "m",

    if (length(fields) > 0) {
      tagList(
        p(
          "Select the fields that contain your document text and ID:",
          style = "margin-bottom: 15px;"
        ),

        selectInput(
          ns("modal_text_field"),
          "Text Field (required)",
          choices = c(
            "Select field..." = "",
            setNames(fields, fields)
          ),
          width = "100%"
        ),

        selectInput(
          ns("modal_docid_field"),
          "Document ID Field (optional)",
          choices = c("None" = "", setNames(fields, fields)),
          width = "100%"
        ),

        # Preview section
        hr(),
        h5("Available Fields:"),
        div(
          style = "background-color: #f8f9fa; padding: 10px; border-radius: 4px; max-height: 150px; overflow-y: auto;",
          tags$code(paste(fields, collapse = ", "))
        )
      )
    } else {
      div(
        class = "alert alert-warning",
        h4("No fields detected"),
        p(
          "Could not automatically detect fields in this file. You may need to specify field names manually or check the file format."
        )
      )
    },

    footer = tagList(
      actionButton(ns("modal_cancel"), "Cancel", class = "btn-secondary"),
      actionButton(
        ns("modal_process"),
        "Process File",
        class = "btn-primary"
      )
    ),

    easyClose = FALSE
  )
}
