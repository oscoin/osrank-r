# import libraries. if clean setup, use install.packages() before library()
library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("lubridate")
library("rmarkdown")
library("data.table")
library("igraph")

# download dependencies dataset from https://libraries.io/data (if file doesn't exist)
destfile="dependencies-1.4.0.csv"
fn <- "https://zenodo.org/record/2536573/files/Libraries.io-open-data-1.4.0.tar.gz"
if(!file.exists(destfile)){
  res <- tryCatch(download.file(fn,
                                destfile="dependencies-1.4.0.tar.gz",
                                method="auto"),
                  error=function(e) 1)
}

# uncompress dependencies
untar("dependencies-1.4.0.tar.gz",files="dependencies-1.4.0.csv")

# read dependencies dataset from https://libraries.io/data
# sample with nrows instead of importing the whole file
dependencies <- read.csv(file = "dependencies-1.4.0.csv", 
                         header = TRUE, colClasses=c("NULL", NA, NA, NA, NA,
                                                     NA, NA, NA, NA, 
                                                     "NULL", "NULL", NA)
#                         , nrows = 1000
                         ) 

# focus on a package manager & create affiliations dataset
# here I am using PyPi as an example
deps <- dependencies %>% 
  select(Platform, Project.Name, Project.ID, Dependency.Name, Dependency.Project.ID) %>% 
  filter(Platform == "Homebrew")

# select only unique dependencies (ie count dependencies across versions)
unique.deps <- unique(deps)

# out-degrees for each project
out.degrees <- unique.deps %>%
  group_by(Project.Name, Project.ID) %>% 
  summarise(Frequency = n()) 

# in-degrees for each project
in.degrees <- unique.deps %>%
  group_by(Dependency.Name, Dependency.Project.ID) %>% 
  summarise(Frequency = n()) 

# merging in-degrees and out-degrees
descriptives <- merge(out.degrees, in.degrees, by.x = "Project.Name", 
                      by.y = "Dependency.Name", all = TRUE)

# drop duplicate names
descriptives <- subset(descriptives, select = c(Project.Name, Frequency.x, 
                                                Frequency.y))

# fix column names for in-degrees and out-degrees
names(descriptives) <- c("Project.Name", "Out.Degrees", "In.Degrees")

# replace NAs with zeros
descriptives[is.na(descriptives)] <- 0

# prepare distributions for in and out degrees
plot.out <- descriptives %>%
  group_by(Out.Degrees) %>%
  summarise(Packages = n()) %>%
  arrange(desc(Packages))

plot.in <- descriptives %>%
  group_by(In.Degrees) %>%
  summarise(Packages = n()) %>%
  arrange(desc(Packages))

# frequency distribution plots for in and out degrees
ggplot(plot.out, aes(Packages)) +
  geom_histogram(bins = 200)

ggplot(plot.in, aes(Packages)) +
  geom_histogram(bins = 200)

# density distribution plots for in and out degrees
ggplot(plot.out, aes(Packages, stat(density))) +
  geom_freqpoly(binwidth = 10)

ggplot(plot.in, aes(Packages, stat(density))) +
  geom_freqpoly(binwidth = 1)

# create adjacency matrix for a package manager
adj.matrix <- graph_from_adjacency_matrix(get.adjacency(
  graph.edgelist(as.matrix(unique.deps[,c("Project.Name", 
                                          "Dependency.Name")]), 
                 directed=TRUE)), mode = "directed")

# create edges data-frame for Gephi (vizualtion software)
edges.df <-unique.deps[,c("Project.Name", "Dependency.Name")]

colnames(edges.df) <- c("Source", "Target")

write.csv(edges.df, file = "edges.csv", row.names = FALSE)

# calculate page_rank and fix names
page.rank.list <- page_rank(adj.matrix, algo = c("prpack"), directed = TRUE, 
                       damping = 0.85)[1]

page.rank.df <- data.frame(attributes(page.rank.list$vector)$names, 
                           as.vector(page.rank.list$vector))

write.csv(page.rank.df, file = "homebrew-pagerank.csv", row.names = FALSE)

names(page.rank.df) <- c("Package.Name", "Page.Rank")

# calculate katz centrality
# katz.df <- alpha.centrality(adj.matrix, alpha=0.5, sparse=FALSE)