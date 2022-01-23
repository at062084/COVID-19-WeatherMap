



#rgb$HEX <- rgb(rgb$R,rgb$B,rgb$G, maxColorValue = 255) 
#write.csv(rgb, file="../experiments/colors.csv", row.names=FALSE, quote=FALSE)

library(colorspace)
library(dplyr)


rgb <- read.csv("../experiments/colors.csv", stringsAsFactors=FALSE)
rgb$HEX <- rgb(rgb$R,rgb$G,rgb$B, maxColorValue=255)

write.csv(rgb, file="../experiments/colors.csv", row.names=FALSE, quote=FALSE)

str(rgb)
summary(rgb)

choose_color()

colCon <- data.frame(rbind(
  c(  0, 0, 0),
  c( 10, 5, 0),
  c( 20,10, 0),
  c( 30,15, 0),
  c( 40,20, 0),
  c( 50,25, 0),
  c( 60,30, 0),
  c( 70,35, 0),
  c( 80,40, 0),
  c( 90,45, 0),
  c(100,50, 0),
  c(110,55, 0),
  c(115,60, 0),
  c(105,65,20),
  c(100,75,30),
  c (95,85,60),
  c( 90,95,90),
  c( 85,85,90),
  c( 80,75,90)
))
colnames(colCon)=c("C","L","H")
csHCL <- polarLUV(colCon$L, colCon$C, colCon$H, names=paste(colCon$L, colCon$C, colCon$H, sep="_"))
csHEX <- hex(csHCL, fixup=TRUE)
hclplot(csHEX, type="sequential", lwd=1, cex=1, pch=16)
specplot(csHEX, lwd=1, rgb=TRUE, type="b", cex=2, pch=16)

colRGB <- data.frame(rbind(
  c(1,0,0,0),
  c(2,5,0,1),
  c(3,10,1,2),
  c(4,17,1,4),
  c(5,27,2,6),
  c(6,40,3,9),
  c(7,56,4,12),
  c(8,76,6,16),
  c(9,101,8,20),
  c(10,130,10,25),
  c(11,165,13,30),
  c(12,206,16,35),
  c(13,245,22,40),
  c(14,239,48,35),
  c(15,284,85,30),
  c(16,269,154,25),
  c(17,222,244,20),
  c(18,167,184,15),
  c(19,122,135,5)
))
colnames(colRGB)=c("n", "R","G","B")



cwmRGB <- read.csv("cwmRGB19.csv")
dim(cwmRGB)
csRGB <- colorspace::RGB(R=as.matrix(cwmRGB)/255, names=paste(cwmRGB$R, cwmRGB$G, cwmRGB$B, sep="_"))
csHEX <- hex(csRGB, fixup=TRUE)
specplot(csHEX, lwd=1, rgb=TRUE, type="b", cex=2, pch=16)

hclplot(csHEX, type="sequential", lwd=1, cex=1, pch=16)





colPal <- c('#160705', '#5E1C0C', '#8B2909', '#A12E05', '#B63502', '#CE4010', '#EE552F', '#FC6A4F', 
            '#EF6C25', '#FFAA22', '#FFDB01', '#FAF842', '#D5FE04', '#CEF524', '#8AFB1E', '#3EFE05', '#3CE418', '#32CB0E')
colPal <- c('#160705', '#5E1C0C', '#50366F', '#633763', '#EF0005', '#8B2909', '#A12E05', '#B54162', '#B63502',  '#BA0518', '#BE3274', '#CE4010', '#EE552F', '#FC6A4F', 
            '#EF6C25', '#FFAA22', '#FFDB01', '#FAF842', '#D5FE04', '#CEF524', '#8AFB1E', '#3EFE05', '#3CE418', '#32CB0E')
hclplot(colPal, type="sequential", lwd=1, cex=1, pch=16)
specplot(colPal, lwd=1, rgb=TRUE, type="b", cex=2, pch=16)

csrgb <- rgb %>% dplyr::filter(SchemaID=="Schema01")
csRGB <- RGB(csrgb$R,csrgb$G,csrgb$B,csrgb$Name)
hclplot(csrgb$HEX, type="sequential", lwd=1, cex=1, pch=16)
specplot(csrgb$HEX, lwd=1, rgb=TRUE, type="b", cex=2, pch=16)
csHCL <- as(csRGB,"polarLUV")


# Schema03 red H[-111,104] ?
# Schema13 red H[12]
# Schema01 redyellow H[-113,104]  !!!
# Schema05 yellow H[66,590]
# Schema18 yellow-green H[59,198]
# Schema07 green H[124]]
# Schema14 green H[8,297]
# Schema16 green H[126]


csHCL <- as(csRGB,"polarLUV")
hclplot(csHCL)


class(csRGB)
specplot(csrgb$HEX, lwd=1, rgb=TRUE, type="b", cex=2, pch=16)

plot(csHCL)






rgb1 <- rgb %>% dplyr::arrange(R,G,B)
specplot((rgb %>% dplyr::filter(SchemaID=="Schema04"))$HEX, lwd=1, rgb=TRUE, type="b", cex=2, pch=16)



plot(csRGB)

csHLS <- as(csRGB,"HLS")
plot(csHLS)
show(csHLS)

csHCL <- as(csRGB,"polarLUV")
plot(csHCL)
hclplot(csHCL)

