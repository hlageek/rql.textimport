# PDF tools for raw stream order
read_pdf_stream <- function(file_path) {
  pages_text <- pdftools::pdf_text(file_path, raw = TRUE)

  # Combine pages into a single text block
  combined_text <- paste(pages_text, collapse = "\n\n")

  return(combined_text)
}
