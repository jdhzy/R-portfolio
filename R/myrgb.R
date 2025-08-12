library(magick)
library(ggplot2)
library(gridExtra)
#' construct the RGB color histograms for an image via magick(...) functions
#' @export

myrgb = function(image, show = FALSE, trim = FALSE) {
  img_data = image_data(image, channels = "rgb")
  
  R = as.integer(img_data[1, , ])
  G = as.integer(img_data[2, , ])
  B = as.integer(img_data[3, , ])
  
  if (trim) {
    R = R[R != 255]
    G = G[G != 255]
    B = B[B != 255]
  }
  
  if (show) {
    df = data.frame(
      R = R,
      G = G,
      B = B
    )
    
    p1 = ggplot(df, aes(x = R)) +
      geom_histogram(aes(y = after_stat(count / sum(count))), bins = 30, fill = 'red', col = 'black') +
      ylim(0, 1) +
      ggtitle("Red Channel")
    
    p2 = ggplot(df, aes(x = G)) +
      geom_histogram(aes(y = after_stat(count / sum(count))), bins = 30, fill = 'green', col = 'black') +
      ylim(0, 1) +
      ggtitle("Green Channel")
    
    p3 = ggplot(df, aes(x = B)) +
      geom_histogram(aes(y = after_stat(count / sum(count))), bins = 30, fill = 'blue', col = 'black') +
      ylim(0, 1) +
      ggtitle("Blue Channel")
    
    grid.arrange(p1, p2, p3, nrow = 1)
  }
  
  return(list(R = R, G = G, B = B))
}