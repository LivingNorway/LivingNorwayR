# Render the rmd file
rmarkdown::render(paste0(here::here(),"/tests/test.Rmd"))
# read the html file and find the tags
library(XML)
text=XML::htmlParse(paste0(here::here(),"/tests/test.html"))
(result_title <- lapply(text['//span[@class="LN_title"]'],xmlValue))
(result_ind <- lapply(text['//span[@class="LN_individualName"]'],xmlValue))
(result_language <- lapply(text['//span[@class="LN_language"]'],xmlValue))
(result_Creator <- lapply(text['//span[@class="LN_creator"]'],xmlValue))
