library(gpt2celltype)
library(httr)

# 输入数据
all_markers <- readRDS("all_markers.rds")

# 运行 gpt2celltype
res <- gpt2celltype(
  input = all_markers,
  tissuename = "Your Tissue",
  model = "gpt-4",
  base_url = "your_custom_url",
  api_key = "your_api_key_here"
)

# 查看结果
print(res)
