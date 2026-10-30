library(dplyr)
library(ggplot2)
library(readr)

# Turn a ggplot into text that can be embedded straight in the page
plot_to_uri <- function(p, width = 6, height = 4) {
  f <- tempfile(fileext = ".png")
  ggsave(f, p, width = width, height = height, dpi = 110)
  knitr::image_uri(f)
}

# Turn a data frame into an HTML table (stored as text)
table_to_html <- function(df) {
  as.character(knitr::kable(df, format = "html"))
}

html_table <- function(df, row_class = "", col_class = "") {
  row_class <- rep_len(row_class, nrow(df))
  col_class <- rep_len(col_class, ncol(df))
  head <- paste0("<th class='", col_class, "'>", names(df), "</th>", collapse = "")
  body <- vapply(seq_len(nrow(df)), function(i) {
    values <- vapply(df, function(col) as.character(col[i]), character(1))
    cells <- paste0("<td class='", col_class, "'>", values, "</td>", collapse = "")
    paste0("<tr class='", row_class[i], "'>", cells, "</tr>")
  }, character(1))
  paste0("<table class='hl-table'><thead><tr>", head, "</tr></thead><tbody>",
         paste(body, collapse = ""), "</tbody></table>")
}
