
#
#
#
# Share a legend between multiple plots using grid.arrange
# ==========================================================
#
# Goal: share a legend between multiple plots that do not also share axes
#
# See this example at RPubs:sjackman/grid_arrange_shared_legend.
#
# It is sometimes possible to obtain good results by creating a dummy faceting
# of the data as in Align-two-plots-on-a-page, but this technique is not
# entirely appropriate when the plots do not also share axes. For greater
# control, you can also use Grid to place the plots and the legends in an
# arbitrary layout.
#
# create four plots p1, p2, p3 and p4 save the legend of p1 as a separate grob
# strip the legends from the plots draw the four plots and the legend below them




library(ggplot2)
library(gridExtra)
library(grid)


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

        plots <- list(...)
        position <- match.arg(position)
        g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x) x + theme(legend.position="none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)

        combined <- switch(position,
                           "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                                  legend,
                                                  ncol = 1,
                                                  heights = unit.c(unit(1, "npc") - lheight, lheight)),
                           "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                                 legend,
                                                 ncol = 2,
                                                 widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
        grid.newpage()
        grid.draw(combined)

}


dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
p1 <- qplot(carat, price, data = dsamp, colour = clarity)
p2 <- qplot(cut, price, data = dsamp, colour = clarity)
p3 <- qplot(color, price, data = dsamp, colour = clarity)
p4 <- qplot(depth, price, data = dsamp, colour = clarity)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 4, nrow = 1)
grid_arrange_shared_legend(p1, p2, p3, p4, ncol = 2, nrow = 2)
