#'Extract hexadecimal colors from a color palette
#'
#'@param name	The name of color palette from RColorBrewer package
#'@param reverse Whether or not reverse the order of colors
#'@return hexadecimal colors
#'@importFrom RColorBrewer brewer.pal brewer.pal.info
#'@export
#'@examples
#'palette2colors("Reds")
palette2colors=function (name, reverse = FALSE)
{
     colors = brewer.pal(RColorBrewer::brewer.pal.info[rownames(brewer.pal.info) ==
                                                            name, "maxcolors"], name)
     if (reverse)
          colors = rev(colors)
     colors
}
