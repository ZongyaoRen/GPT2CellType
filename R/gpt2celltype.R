#' gpt2celltype: Cell type annotation with manual API requests
#'
#' @param input Either the differential gene table from Seurat's FindAllMarkers(), or a custom list of gene markers.
#' @param tissuename Optional name of the tissue type.
#' @param model The GPT model name (e.g., "gpt-4").
#' @param base_url Custom API URL. Default is 'https://api.openai.com/v1/'.
#' @param api_key OpenAI API key.
#' @param topgenenumber Number of top marker genes to include for annotation (default = 10).
#' @return A vector of predicted cell types or the generated prompt.
#' @import httr
#' @export
gpt2celltype <- function(input, tissuename = NULL, model = "gpt-4",
                         base_url = "https://api.openai.com/v1/",
                         api_key = Sys.getenv("OPENAI_API_KEY"),
                         topgenenumber = 10) {
  #check API KEY
  if (api_key == "") {
    stop("Error: API key not provided. Set OPENAI_API_KEY in environment or pass explicitly.")
  } else {
    print("Note: OpenAI API key found: returning the cell type annotations.")
  }

  # Process input data
  if (class(input) == 'list') {
    input <- sapply(input, paste, collapse = ',')
  } else {
    input <- input[input$avg_log2FC > 0, , drop = FALSE]
    input <- tapply(input$gene, list(input$cluster),
                    function(i) paste0(i[1:topgenenumber], collapse = ','))
  }

  # Build prompt
  prompt <- paste0(
    'Identify cell types of ', tissuename,
    ' cells using the following markers separately for each row. ',
    'Only provide the cell type name. Do not show numbers before the name.\n',
    paste0(names(input), ': ', unlist(input), collapse = '\n')
  )

  # Split into chunks if necessary
  cutnum <- ceiling(length(input) / 30)
  cid <- if (cutnum > 1) as.numeric(cut(1:length(input), cutnum)) else rep(1, length(input))

  # Initialize result list
  allres <- sapply(1:cutnum, function(i) {
    id <- which(cid == i)
    success <- FALSE
    while (!success) {
      # Prepare API request
      response <- httr::POST(
        url = paste0(base_url, "chat/completions"),
        httr::add_headers(`Authorization` = paste("Bearer", api_key)),
        encode = "json",
        body = list(
          model = model,
          messages = list(
            list(role = "user", content = paste0(
              "Identify cell types of ", tissuename,
              " cells using the following markers separately for each row. ",
              "Only provide the cell type name. Do not show numbers before the name.\n",
              paste(input[id], collapse = '\n')
            ))
          )
        )
      )

      # Handle response
      if (httr::status_code(response) == 200) {
        result <- content(response)$choices[[1]]$message$content
        res <- strsplit(result, '\n')[[1]]
        if (length(res) == length(id)) success <- TRUE
      } else {
        warning("API request failed. Retrying...")
      }
    }
    names(res) <- names(input)[id]
    res
  }, simplify = FALSE)

  print('Note: It is always recommended to check the results returned by GPT-4 in case of\n AI hallucination, before going to downstream analysis.')
  return(gsub(',$', '', unlist(allres)))
}
