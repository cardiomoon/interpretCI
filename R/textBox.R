#' Draw textbox
#'
#' Draw textbox
#' @param string string
#' @param color font color
#' @param lcolor line color
#' @param bg background color
#' @param lwd numeric line width
#' @param width numeric box width
#' @param bold,italic logical
#' @param fontsize numeric font size
#' @param space space between lines
#' @param fontname name of font
#' @importFrom officer fp_border
#' @importFrom flextable flextable delete_part width color bg bold italic
#' border_outer fontsize line_spacing font
#' @return A flextable
#' @export
#' @examples
#' string="Good Morning!"
#' textBox(string,italic=TRUE)
textBox=function(string,color="black",lcolor="red",bg="cornsilk",lwd=1,width=10,
                 bold=FALSE,italic=FALSE,fontsize=11,space=1.5,fontname){
     d1=data.frame(d1=string)
     myborder=fp_border(color=lcolor,width=lwd)
     ft=flextable(d1) %>% delete_part() %>%
          color(color=color) %>%
          bg(bg=bg) %>%
          border_outer(part="all",border=myborder) %>%
          fontsize(size=fontsize) %>%
          line_spacing(space=space)

     if(bold) ft=bold(ft)
     if(italic) ft=italic(ft)
     if(!missing(fontname)) ft=font(ft,fontname=fontname)

     ft %>% width(width=12)
}
