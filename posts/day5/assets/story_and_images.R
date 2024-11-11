library(tidyverse)

# Getting images from stable diffusion-----------------------------
# source(here::here("req_single_image.R"))

journey <- tibble(
  location = c(
    "London, England",
    "Suez, Egypt",
    "Bombay, India",
    "Calcutta, India",
    "Hong Kong",
    "Yokohama, Japan",
    "San Francisco, United States",
    "New York City, United States",
    "Liverpool, England",
    "London, England"
  ),
  situation_quotation = c(
    "Fogg accepts a £20,000 wager to travel around the world in 80 days. 'The unforeseen does not exist.'",
    "Detective Fix begins to pursue Fogg, suspecting him of bank robbery.",
    "Fogg and Passepartout encounter a disrupted railway line and arrange for an elephant to continue their journey.",
    "Fogg rescues Aouda, a widow who is to be sacrificed, saying, 'I have yet twelve hours to save her!'",
    "Fix convinces Passepartout to delay Fogg in Hong Kong by getting him drunk, hoping to catch him.",
    "Passepartout performs with a Japanese circus after being separated from Fogg.",
    "Fogg and his companions face an attack by Sioux warriors while traveling by train across the American plains.",
    "Fogg charters a steamer after missing a scheduled ship to cross the Atlantic.",
    "Fogg is briefly arrested by Fix in Liverpool, delaying him even further.",
    "Fogg arrives just in time to win the wager after realizing a time zone miscalculation gained him a day."
  ),
  landmark = c(
    "Tower of London",
    "Suez Canal",
    "Gateway of India",
    "Victoria Memorial",
    "Victoria Peak",
    "Sankeien Garden",
    "Golden Gate Bridge",
    "Statue of Liberty",
    "Liverpool Cathedral",
    "British Museum"
  )
)

# journey$prompt <- paste0(journey$situation_quotation, " in ", journey$location)
# 
# 
# drawing_instructions <- "This scene should be in realistic illustration style with influences from classic adventure literature and Victorian-era visual art."
# 
# reqs <- lapply(
#   journey$prompt,
#   function(x){
#     req_single_image(x, drawing_instructions)
#   }
# )
# 
# resps <- httr2::req_perform_parallel(reqs, on_error = "continue")
# 
# # All images
# new_all_imgs <- lapply(resps, get_image)
# 
# # Save
# lapply(
#   1:length(new_all_imgs),
#   function(i){
#     magick::image_write(magick::image_read(new_all_imgs[[i]]), 
#                         path = here::here(paste0("posts/day5/assets/", "image", i, ".png")))
#   }
# )


