make_hex <- function() {
  imgurl <- system.file("ReliabilityTheoryImg.png", package="ReliabilityTheory")
  hexSticker::sticker(imgurl,
                      s_x = 0.975,
                      s_y = 1.525,
                      s_width = 0.85,
                      s_height = 0.9,
                      package="ReliabilityTheory",
                      p_x = 1,
                      p_y = 0.575,
                      p_color = "orange",
                      p_family = "serif",
                      p_fontface = "bold",
                      p_size = 5,
                      h_fill = "white",
                      h_color = "orange",
                      url = "www.louisaslett.com",
                      u_size = 1.5,
                      u_family = "mono",
                      white_around_sticker = TRUE,
                      filename="inst/ReliabilityTheoryHex.png",
                      dpi=600)
}
