jojo_palette <- list(
  PhantonBlood = c("#7b3fb0","#4d3b9f","#617bc1","#a34838","#e3be4c"), #https://www.color-hex.com/color-palette/79970
  BattleTendency = c("#5e4734","#854545","#4e6746","#c7b050","4e6c87"), #https://www.color-hex.com/color-palette/79971
  StardustCrusaders= c("#b25942","#e3cb67","#476b43","#323630","#c5c5c5"), #https://www.color-hex.com/color-palette/79972
  DiamondUnbreakable = c("#480f7c","#6d4190","#fad30a","#c73f75","#b71154"), #https://www.schemecolor.com/diamond-is-unbreakable.php
  StoneOcean = c("#ecc88c","#50bd47","#dfc356","#d1a045","#373749"), #https://www.color-hex.com/color-palette/109687
  Jonathan = c("#060329","#1e12d0","#1470e8","#7d12d0","593112"), #https://www.color-hex.com/color-palette/39076
  Joseph = c("#ffe735","#70bd14","#9041cc","#632c8d","#417ecc"), #https://www.color-hex.com/color-palette/39078
  Jotaro1 = c("#e5ddff","#3f78A3","#805acc","#c234e3","#33435d"), #https://www.pinterest.com/pin/709879960009812954/
  Jotaro2 = c("#ecc88c","#50bd47","#dfc356","#d1a045","#373740"), #https://www.color-hex.com/color-palette/109687
  Dio = c("#50ecea","#00c0c0","#283bf7","#d1cea7","	#ffd801"), #https://www.color-hex.com/color-palette/96066
  Josuke = c("#243182","#eeb737","#fa5d55","#922956","#a86585"), #https://www.colourlovers.com/palette/4516087/Josuke_Higashikata
  Rohan = c("#dcb5b0","#c777b6","#f7ef95","#97db8c","#263231"), #https://jojo.fandom.com/f/p/4400000000000092610
  Giovana = c("#ffd666","#ffe2ba","#d66cff","#3c53ff","#3d3396"), #https://www.colourlovers.com/palette/4710946/Giorno_Giovanna?widths=1
  Koichi = c("#225544","#9d9f9d","#49305e","#d99233","#98b2e9"),
  Jolyne = c("#f272ca","#a2b711","#44b2b1","#0f6496","#141e39"), #https://jojo.fandom.com/f/p/4400000000000092610
  Fugo = c("#2e3e8c","#f7b25c","#aabe57","#b43b7f","#5e3c9b"), #https://jojo.fandom.com/f/p/4400000000000092610
  Trish = c("#5a487a","#2d243d","#fbd125","#dd458c","#eyyfae"), #https://www.color-hex.com/color-palette/39081
  Diavolo = c("#765db5","#409c62","#fc8bb9","#f3ee85","9fee60"), #https://jojo.fandom.com/f/p/4400000000000092610
  Yasuho = c("#fbedbe","#f87d99","#f394bf","#7dd7fb","#6b3ba7"), #https://www.color-hex.com/color-palette/38956
  VinegarDoppio = c("#f77199","#fcd1a1","#cf81d2","51ac7a","#68549d"), #https://www.color-hex.com/color-palette/91476
  SmokeyBrown = c("#2b3d38","#ada247","#cac9c4","#55816f","#5f4439") #https://www.color-hex.com/color-palette/113255
)

jojo_stand <- function(name, n, type = c("discrete", "continuous")) {
  type <- match.arg(type)
  
  pal <- jojo_palette[[name]]
  if (is.null(pal))
    stop("Palette not found.")
  
  if (missing(n)) {
    n <- length(pal)
  }
  
  if (type == "discrete" && n > length(pal)) {
    stop("Number of requested colors greater than what palette can offer")
  }
  
  out <- switch(type,
                continuous = grDevices::colorRampPalette(pal)(n),
                discrete = pal[1:n]
  )
  structure(out, class = "palette", name = name)
}

#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.stand <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))
  
  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")
  
  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}