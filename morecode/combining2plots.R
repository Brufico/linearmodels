#' ---
#' title: "Align two plots on a page"
#' subtitle: "https://github.com/hadley/ggplot2.wiki.git"
#' author: "baptiste edited this page on 11 Feb · 7 revisions"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' output:
#'         html_document:
#'                 theme: readable
#'                 number_sections: yes
#'         word_document:
#'              fig_caption: yes
#'              fig_width: 9
#'              toc: no
#'              number_sections: yes
#'              theme: readable
#' ---


#+ options , include = FALSE
knitr::opts_chunk$set(echo = TRUE)



#'
#' Goal
#' =====

#' Two plots with different meaning (y-scale, geom, etc.) need to be aligned for
#' they share a common x-axis.
#' There are two stategies: use facetting, or create two separate plots and
#' combine them on a page.


#'
#' Dummy Facetting
#' ==============
#'

#' It is often possible to obtain good results by creating a dummy facetting of
#' the data as in the following example:

library(ggplot2)

# create data
x <- seq(1992, 2002, by=2)
set.seed(1234)
d1 <- data.frame(x=x, y=rnorm(length(x)))
xy <- expand.grid(x=x, y=x) # allcombinations of values of x
d2 <- data.frame(x=xy$x, y=xy$y, z= jitter(xy$x + xy$y))
# add panel specification in the data
d1$panel <- "a"
d2$panel <- "b"
d1$z <- d1$x

# bind d1 and d2 together
d <- rbind(d1, d2)

p <- ggplot(data = d, mapping = aes(x = x, y = y)) +
        facet_grid(panel~., scale="free") +
        # the 2 geoms use different data subsets
        geom_line(data = d1, stat = "identity") +
        geom_tile(data = d2, mapping=aes(colour=z, fill=z), stat = "identity")
p



#'
#' Combining two plots
#' ===================

#' Alternatively, it may be easier to produce separate plots, and align them on
#' the page. We illustrate the procedure with the gtable package, used
#' internally for the layout of ggplot objects.

#' BUT the second method does not align here the x-axis


library(ggplot2)

# same data
x <- seq(1992, 2002, by=2)
set.seed(1234)
d1 <- data.frame(x=x, y=rnorm(length(x)))
xy <- expand.grid(x=x, y=x)
d2 <- data.frame(x=xy$x, y=xy$y, z= jitter(xy$x + xy$y))

# define plots
p1 <-  ggplot(data = d1, mapping = aes(x = x, y = y)) +
        geom_line(stat = "identity")

p2 <-  ggplot(data = d2, mapping = aes(x=x, y=y, fill=z)) +
        geom_tile()

## convert plots to gtable objects
library(gtable)
library(grid) # low-level grid functions are required
g1 <- ggplotGrob(p1)
g1 <- gtable_add_cols(g1, unit(0,"mm")) # add a column for missing legend
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2, size="first") # stack the two plots
g$widths <- unit.pmax(g1$widths, g2$widths) # use the largest widths
# center the legend vertically
g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
grid.newpage()
grid.draw(g)



