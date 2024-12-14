‘’’//R

library(gpt2celltype)
library(httr)

# Load input data
all_markers <- readRDS("all_markers.rds")

# Run gpt2celltype with a custom API URL
res <- gpt2celltype(
  input = all_markers,
  tissuename = "Your Tissue",
  model = "gpt-4",
  base_url = "your_custom_url",
  api_key = "your_api_key_here"
)

# Check result 
print(res)

‘’’
