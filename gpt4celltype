#' Cell type annotation with OpenAI GPT models

gptcelltype <- function(input, tissuename = NULL, model = 'gpt-4', 
                        topgenenumber = 10, base_url = NULL) {
  # Fetch the OpenAI API key
  OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")
  if (OPENAI_API_KEY == "") {
    print("Note: OpenAI API key not found: returning the prompt itself.")
    API.flag <- 0
  } else {
    API.flag <- 1
  }

  # Process input: Custom list or Seurat markers
  if (class(input) == 'list') {
    input <- sapply(input, paste, collapse = ',')
  } else {
    input <- input[input$avg_log2FC > 0, , drop = FALSE]
    input <- tapply(input$gene, list(input$cluster), 
                    function(i) paste0(i[1:topgenenumber], collapse = ','))
  }

  # Return the prompt if no API key is found
  if (!API.flag) {
    message <- paste0(
      'Identify cell types of ', tissuename, 
      ' cells using the following markers separately for each\n row. Only provide the cell type name. Do not show numbers before the name.\n Some can be a mixture of multiple cell types. ',  
      "\n", paste0(names(input), ':', unlist(input), collapse = "\n")
    )
    return(message)
  } else {
    print("Note: OpenAI API key found: returning the cell type annotations.")
    
    # Break input into smaller chunks (to avoid token limits)
    cutnum <- ceiling(length(input) / 30)
    if (cutnum > 1) {
      cid <- as.numeric(cut(1:length(input), cutnum))	
    } else {
      cid <- rep(1, length(input))
    }

    # Perform API calls
    allres <- sapply(1:cutnum, function(i) {
      id <- which(cid == i)
      flag <- 0
      while (flag == 0) {
        # Make the API call with the custom base_url if provided
        k <- openai::create_chat_completion(
          model = model,
          base_url = base_url,  # Add custom base_url here
          message = list(
            list("role" = "user", "content" = paste0(
              'Identify cell types of ', tissuename, 
              ' cells using the following markers separately for each\n row. Only provide the cell type name. Do not show numbers before the name.\n Some can be a mixture of multiple cell types.\n',
              paste(input[id], collapse = '\n')
            ))
          )
        )
        res <- strsplit(k$choices[,'message.content'], '\n')[[1]]
        if (length(res) == length(id))
          flag <- 1
      }
      names(res) <- names(input)[id]
      res
    }, simplify = FALSE)

    print('Note: It is always recommended to check the results returned by GPT-4 in case of\n AI hallucination, before going to downstream analysis.')
    return(gsub(',$', '', unlist(allres)))
  }
}
