devtools::install_github("rstudio/r2d3")
devtools::install_github("r-lib/remotes")
devtools::install_github("dreamRs/r2d3maps")
devtools::install_github("ropenscilabs/rnaturalearthdata")
install.packages("rnaturalearthhires",
                 repos = "http://packages.ropensci.org",
                 type = "source")
devtools::install_github("ropenscilabs/rnaturalearth")
install.packages("rnaturalearth")
install.packages("reactable")
install.packages("htmltools")

library( r2d3maps )
library( rnaturalearth )
library("rnaturalearthdata")
library( dplyr )
library( ggplot2 )
library("sf")
library("maps")
library("tools")
install.packages(c("tidyverse", "ggstatsplot"))
library("ggstatsplot")
install.packages("ggstatsplot")
theme_set(theme_bw())
library(viridis)
library(hrbrthemes)
library(reactable)
library(htmltools)
library(stringr)
library(gsubfn)
reactable(data)

india <- ne_states(country = "india", returnclass = "sf")
d3_map(shape = india) %>%
  add_labs(title = "india")


world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

(sites <- data.frame(longitude = c(-80.144005, -80.109), latitude = c(26.479005, 
                                                                      26.83)))

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

(sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                   crs = 4326, agr = "constant"))

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)



states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
head(states)

aoi_boundary_HARV <- st_read(
  "C:\\Users\\ShanmugA\\Desktop\\Learn_Maps\\india_administrative_assembly_constituencies_boundary\\india_administrative_assembly_constituencies_boundary.shp")

Ind_boundary_HARV <- cbind(aoi_boundary_HARV, st_coordinates(st_centroid(aoi_boundary_HARV)))
TN_map<-Ind_boundary_HARV %>% filter(st_name=='TAMIL NADU')
Chennai_map<-Ind_boundary_HARV %>% filter(st_name=='TAMIL NADU' & dt_code=='2')


View(aoi_boundary_HARV)
ggplot() + 
  geom_sf(data = aoi_boundary_HARV, size = 3, color = "black", fill = "cyan1") + 
  ggtitle("AOI Boundary Plot") + 
  coord_sf()

xlim(L,R) ylim(S,N)

ggplot(data = Chennai_map) +
  geom_sf() +
  geom_sf(data = Chennai_map, aes(fill = shape_area)) + 
  geom_label(data = Chennai_map, aes(X, Y, label = ac_name), size = 2, fontface = "bold") +
  coord_sf(xlim = c(80.18, 80.310), ylim = c(12.965, 13.15), expand = FALSE)

pnorm(2.03038)

Assembly2021<-read.csv("C:\\Users\\ShanmugA\\Desktop\\Learn_Maps\\Assembly2021.csv")
colnames(Assembly2021)[1]<-"PartyName"
Assembly2021<-Assembly2021 %>% mutate(PartyShort=gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',PartyName,perl = TRUE)) %>% mutate(Name_w_Party=paste(Name," (", PartyShort, ")",sep = ""))
is_outlier <- function(x) {return(x > quantile(x, 0.75) + 1.5 * IQR(x) & pnorm((x - mean(x))/sd(x)) >=0.95 )}

# View(Assembly2021)
Assembly2021 <- Assembly2021 %>% 
                  group_by(Constituency) %>% 
                  mutate(
                    is_outlier=ifelse(is_outlier(DownloadCount), DownloadCount, as.numeric(NA))
                    )
Assembly2021$Name_w_Party[which(is.na(Assembly2021$is_outlier))] <- as.numeric(NA)
Assembly2021$PartyShort[which(is.na(Assembly2021$is_outlier))] <- as.numeric(NA)

ggplot(Assembly2021, aes(y=DownloadCount, x=factor(Constituency))) + 
  geom_boxplot() + 
  facet_wrap(~ factor(Constituency), nrow = 1, scales = "free") +
  geom_text(aes(label=Name_w_Party),na.rm=TRUE,nudge_y=0.05,nudge_x=0.03) +
  theme_bw()


data <- read.csv("C:\\Users\\ShanmugA\\Desktop\\Learn_Maps\\Affidavit_Count.csv",
                 stringsAsFactors = FALSE)
data <- tibble(data)
glimpse(data)

data <- data %>% 
            select(constituency,partyName,candidateName,downloadCount) %>% 
            mutate(candidateName = gsub("[.]", " ", candidateName, perl = TRUE),
                   candidateName = gsub("[,]", " ", candidateName, perl = TRUE),
                   candidateName = gsub("^ *|(?<= ) | *$", "", candidateName, perl = TRUE),
                   candidateName=str_to_title(candidateName)
                   )

top_count<-data %>% arrange(desc(downloadCount)) %>% filter(downloadCount>=2000)

str_to_title("arun kumar")

# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "14px", fill = "#00bfc4", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "6px", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}


table<-reactable(
  top_count,
  pagination = FALSE,
  defaultSorted = "downloadCount",
  columns = list(
    constituency = colDef(
      name = "Constituency",
      style = list(fontFamily = "calibri", whiteSpace = "pre"),
    ),
    partyName = colDef(
      name = "Party",
      style = list(fontFamily = "calibri")
    ),
    candidateName = colDef(
      name = "Candidate",
      style = list(fontFamily = "calibri", whiteSpace = "pre")
    ),
    downloadCount = colDef(
      name = "# of Downloads",
      defaultSortOrder = "desc",
      # Render the bar charts using a custom cell render function
      cell = function(value) {
        width <- paste0(value * 100 / max(data$downloadCount,na.rm=T), "%")
        # Add thousands separators
        value <- format(value, big.mark = ",")
        # Fix each label using the width of the widest number (incl. thousands separators)
        value <- format(value, width = 9, justify = "right")
        bar_chart(value, width = width, fill = "#3fc1c9")
      },
      # And left-align the columns
      align = "left",
      style = list(fontFamily = "courier", whiteSpace = "pre")
    )
  ),
  compact = T
)

html_file <- "C:\\Users\\ShanmugA\\Desktop\\Learn_Maps\\table.html"
saveWidget(widget = table, file = html_file, selfcontained = FALSE)