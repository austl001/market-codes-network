
# Loading packages --------------------------------------------------------

library(pdftools)
library(tidyverse)
library(stringr)
library(igraph)
library(networkD3)
library(htmlwidgets)


# Getting docs and extracting text ----------------------------------------

csd_docs <- list.files("documents", pattern = "CSD")

csd_text <- list()

for (csd in csd_docs) {
  csd_url <- paste0(getwd(), "/documents/", csd)
  temp <- paste(pdf_text(csd_url), collapse = " ")
  csd_text <- append(csd_text, temp)
}

csd_list <- str_sub(csd_docs, 1, 8)

csd_count_df <- NULL

for (i in 1:length(csd_list)) {
  
  temp_df <- data.frame(
    CsdRef = csd_list[i],
    CsdEntity = csd_list,
    CsdCount = str_count(csd_text[i], csd_list)
  )
  
  csd_count_df <- rbind(csd_count_df, temp_df)
  
}
    
head(csd_count_df)


# Creating network graphs -------------------------------------------------

csd_network <- csd_count_df %>% 
  mutate(SelfReference = CsdRef == CsdEntity) %>% 
  filter(CsdCount > 0, !SelfReference) %>% 
  select(-SelfReference)

write_csv(csd_network, "csd_network.csv")

csd_network %>% 
  graph_from_data_frame(directed = FALSE) %>% 
  plot(
    vertex.size = degree(graph_from_data_frame(csd_network), mode = "all"), 
    layout = layout.fruchterman.reingold, 
    main = "fruchterman.reingold"
    )

csd_network %>% 
  graph_from_data_frame(directed = FALSE) %>% 
  plot(
    vertex.size = degree(graph_from_data_frame(csd_network), mode = "all"), 
    layout = layout.circle, 
    main = "circle"
  )


interactive_graph <- csd_network %>% 
  filter(
    !CsdRef %in% "CSD 0301",
    !CsdEntity %in% "CSD 0301"
    ) %>% 
  simpleNetwork(
    height = "200px", 
    width = "200px",
    linkDistance = 1000,
    charge = -900,
    fontSize = 20,
    zoom = TRUE
  )
  
saveWidget(interactive_graph, "interactiveNetwork.html")


  
csd_network %>% 
  group_by(CsdEntity) %>% 
  summarise(CsdCount = sum(CsdCount)) %>% 
  ggplot() + 
  geom_col(aes(reorder(CsdEntity, CsdCount), CsdCount)) + 
  coord_flip()

csd_network %>% 
  ggplot() + 
  geom_bar(aes(fct_infreq(CsdEntity))) + 
  coord_flip()


csd_network


# Character count ---------------------------------------------------------

char_count <- data.frame(
  "Csd" = csd_list,
  "CharacterCount" = unlist(lapply(csd_text, nchar))
)

arrange(char_count, CharacterCount)

char_count %>% 
  ggplot() + 
  geom_col(aes(fct_reorder(Csd, CharacterCount), CharacterCount)) + 
  scale_y_continuous(label = scales::comma) + 
  coord_flip() + 
  theme_minimal()


csd_402 <- csd_text[which(csd_list=="CSD 0402")]

fileConn <- file("csd_402.txt")
write_lines(csd_402, "csd_402.text")


scales::comma(sum(char_count["CharacterCount"]))


  