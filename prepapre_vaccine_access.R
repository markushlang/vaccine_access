
# load packages
library(tidyverse)
library(janitor)
library(circlize)
library(svglite)
library(cowplot)
library(see)
library(rio)

# load data
url <- "https://da7af2c8-d9b0-47a3-a3f6-89c3c3bfa02c.filesusr.com/ugd/356854_74d8e82a79e94c47a828aa1c45da6134.xlsx?dn=COVID19%20Vaccine%20Arrangements_07%20Sep%202021.xlsx"
vaccine_access <- rio::import(url,sheet = "Primary Arrangements")

# calculate vaccine earnings
vaccine_earnings <- vaccine_access %>%
  janitor::clean_names() %>%
  dplyr::select(buyer_recipient,
                vaccine_candidate,
                finalized_commitment,
                price_in_usd_million) %>%
  dplyr::filter(finalized_commitment == "Yes") %>%
  dplyr::select(-finalized_commitment) %>%
  dplyr::filter(price_in_usd_million != "Not available") %>%
  dplyr::mutate(price_in_usd_million = as.numeric(price_in_usd_million)) %>%
  dplyr::group_by(buyer_recipient,vaccine_candidate) %>%
  dplyr::summarise(price_in_usd_million = sum(price_in_usd_million)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(price_in_usd_million = as.numeric(price_in_usd_million))

# circlize function
get_circlize <- function(developer=TRUE,matrix=TRUE,color=TRUE){
  
  chord <- data.frame(xtabs(price_in_usd_million ~ buyer_recipient + vaccine_candidate, matrix))
  chord$Freq <- chord$Freq
  
  link.col <- ifelse(chord$vaccine_candidate == developer,color, "#d3d3d3")
  grid.col <- ifelse(chord$vaccine_candidate == developer,color,"#d3d3d3")
  names(grid.col) <- chord$vaccine_candidate
  
  circos.clear()
  
  circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
  par(mar = rep(0, 4))
  
  chordDiagram(chord, transparency = 0.25, 
               directional = 1,
               annotationTrack = "grid", preAllocateTracks = 1,
               direction.type = c("arrows", "diffHeight"),
               col = link.col,
               grid.col = "grey",
               # annotationTrack = "grid", annotationTrackHeight = c(0.05, 0.1),
               link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE) 
  
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    sector.name = get.cell.meta.data("sector.index")
    circos.text(mean(xlim), ylim[1] + 0.2, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    # circos.axis(h = "top", labels.cex = 0.35, minor.ticks=3,major.tick.percentage = 0.5, sector.index = sector.name, track.index = 2)
  }, bg.border = NA)
  
  circos.clear()
  
}

# Pfizer/BioNTech Graph
svg("pfizer_biontech.svg")
get_circlize(developer = "Pfizer/BioNTech",
             matrix = vaccine_earnings,
             color = "#732c57")
dev.off()

# Moderna Graph
svg("moderna.svg")
get_circlize(developer = "Moderna",
             matrix = vaccine_earnings,
             color = "#b1ba61")
dev.off()

# Gamaleya Graph
svg("gamaleya.svg")
get_circlize(developer = "Gamaleya",
             matrix = vaccine_earnings,
             color = "#53887b")
dev.off()

# AstraZeneca/Oxford Graph
svg("astrazeneca.svg")
get_circlize(developer = "AstraZeneca/Oxford",
             matrix = vaccine_earnings,
             color = "#3b618c")
dev.off()

# Johnson & Johnson Graph
svg("j_and_j.svg")
get_circlize(developer = "Johnson & Johnson",
             matrix = vaccine_earnings,
             color = "#1e3c53")
dev.off()

# vaccine earnings per developer
developer_earnings <- vaccine_earnings %>%
  dplyr::group_by(vaccine_candidate) %>%
  dplyr::summarise(price_in_usd_million = sum(price_in_usd_million)) %>%
  dplyr::ungroup()

# graph developer #505f94

ggplot(developer_earnings,
       aes(x = price_in_usd_million/1000,y = reorder(vaccine_candidate,price_in_usd_million))) + 
  geom_col(fill = "#505f94") + 
  scale_x_continuous(limits = c(0,55),
                     expand = c(0, 0),
                     position = "top") +
  theme_modern() + 
  labs(
    x = NULL, y = NULL,
    title = "Covid-19 Vaccine Revenue (by developer), $bn",
    subtitle = "Last update: September 7, 2021",
    caption = "Data: Global Health Centre (Graduate Institute Geneva). Calculations and Graph: Markus Lang / @markushlang\n  www.knowledgeportalia.org/covid19-vaccine-arrangements"
  ) +
  theme(aspect.ratio = 3.2/7,
        text = element_text(family = "Helvetica"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "#b7c5d1"),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(hjust = 0,margin = margin(l=3), size = 10),
        legend.position = c(0, 1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = rel(1), color = "gray8"),
        axis.line.x  = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          size = rel(1.5), 
          face = "bold")) -> developer_earnings

save_plot(developer_earnings, 
          file = "developer_earnings.svg", 
          base_width = 12, 
          base_height = 8)

# vaccine earnings by country
country_earnings <- vaccine_access %>%
  janitor::clean_names() %>%
  dplyr::select(developer_location,
                finalized_commitment,
                price_in_usd_million) %>%
  dplyr::filter(finalized_commitment == "Yes") %>%
  dplyr::select(-finalized_commitment) %>%
  dplyr::filter(price_in_usd_million != "Not available") %>%
  dplyr::mutate(price_in_usd_million = as.numeric(price_in_usd_million)) %>%
  dplyr::group_by(developer_location) %>%
  dplyr::summarise(price_in_usd_million = sum(price_in_usd_million)) %>%
  dplyr::ungroup() 
  
# graph country
ggplot(country_earnings,
       aes(x = price_in_usd_million/1000,y = reorder(developer_location,price_in_usd_million))) + 
  geom_col(fill = "#5fbed0") + 
  scale_x_continuous(limits = c(0,55),
                     expand = c(0, 0),
                     position = "top") +
  theme_modern() + 
  labs(
    x = NULL, y = NULL,
    title = "Covid-19 Vaccine Revenue (by developer location), $bn",
    subtitle = "Last update: September 7, 2021",
    caption = "Data: Global Health Centre (Graduate Institute Geneva). Calculations and Graph: Markus Lang / @markushlang\n  www.knowledgeportalia.org/covid19-vaccine-arrangements"
  ) +
  theme(aspect.ratio = 3.2/7,
        text = element_text(family = "Helvetica"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "#b7c5d1"),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(hjust = 0,margin = margin(l=3), size = 10),
        legend.position = c(0, 1), 
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = rel(1), color = "gray8"),
        axis.line.x  = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(
          size = rel(1.5), 
          face = "bold")) -> country_earnings

save_plot(country_earnings, 
          file = "country_earnings.svg", 
          base_width = 12, 
          base_height = 8)
