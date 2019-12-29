needs(tidyverse, magrittr, scales) # load dependencies

data <- read.csv("tiger_population.csv") # import dataset

# barchart of the tiger population
ggplot(data, aes(Year, Tiger_Population)) +
  geom_bar(stat="identity") +
  scale_x_continuous(breaks = c(1970, 1980, 1990, 2000, 2010, 2019)) +
  theme_bw()

## knitting pattern ##
# we will convert the barchart into a tileplot where each tile is representing a knitting stitch
# for the pattern we use 25 rows of 30 columns
columns = 30 
rows = 25

# first we project the data on a scale from 1 to the number of stiches we have for the pattern
tcolumn <- data.frame(Year = data$Year, 
                      trow = round(rescale(data$Tiger_Population, to = c(1, rows))))
# now we rescale the years
trow <- data.frame(Year = data$Year, 
                   tcolumn = round(rescale(data$Year, to = c(1, columns))))

# now we match the rescaled data
transformed <- left_join(trow, tcolumn) 

# now for the tile plot data set: for the length we want the number of rows of the project (more than the rows of the pattern), and for every stitch in a row an entry to represent the width. 
# because we are knitting a loop, we will just knit as many rows as we need for the loop to be the perfect size. This doesn't affect the size of the chart.
# we also add a column for the pattern we set to zero for now
grid <- data.frame(columns=rep(1:60, 40), rows = rep(1:40, each=60), pattern=0) %>% 
  arrange(rows,columns)

# last but not least, we add a new row with the column label we want to plot onto the grids
# actually we could just use the column number for this, but there is a little hurdle: when knitting you add row by row like a printer, you start at column one and go through to the last one, and then you'll add a new stitch on the last column which now becomes your first for the new row and knit you way "backwards" to column one of the first row
# this is why we have to reverse the label for every second row:
grid$label <- grid$rows
is.even <- function(x) x %% 2 == 0 # define even numbers
grid[is.even(grid$columns),] <- grid %>% filter(is.even(columns)) %>% group_by(columns) %>% mutate(label = rev(label))

# now we combine the grid data set with the data that tells us how many stitches are to be colored
# for each row in the transformed data set this simple for-loop finds all the rows in the grid data set between stitch 5 and stitch 5 plus the knitting row number and has a column number between 20 and the 20 plus the stitch number that need to be colored plus three, then overwrites the zero in the pattern column to one
for(i in 1:nrow(transformed)){
  grid$pattern[grid$rows %in% 5:(5+transformed$trow[i]) & grid$columns %in% (20 + transformed$tcolumn[i]):(20 + (transformed$tcolumn[i] + 3))] <- 1
}

# now for the plot
ggplot(grid, aes(x = columns, y = rows, fill = as.factor(pattern))) + 
  geom_tile(size = 0.5, color = "black") +
  coord_equal() +
  geom_text(aes(label = label)) +
  scale_x_continuous(breaks = seq(0, max(grid$columns), by = 1), expand=c(0,0), sec.axis = dup_axis()) +
  theme(legend.position = "none") +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_text(size=12),
        axis.title.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(size=22)) +
  scale_fill_manual(values = c("#f2f2f2", "#FE503A")) +
  ggtitle("Tiger Populations since 1970")

# save the plot as a pdf
ggsave("tiger_loop.pdf", plot = last_plot(),
       width = 50, height = 40, units = "cm",
       dpi = 300, limitsize = TRUE, device = "pdf")
