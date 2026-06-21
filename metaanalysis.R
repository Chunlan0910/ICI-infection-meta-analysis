library("meta")
library(metafor)
data=read.csv("grade3 TEAE.csv",header = T)
data=read.csv("grade3 TRAE.csv",header = T)
data=read.csv("anygrade TRAE.csv",header = T)
data=read.csv("anygrade TEAE.csv",header = T)
ORR=metagen(log(OR), lower = log(L), upper = log(U), sm="RR",
            data=data,studlab=paste(data$Author,data$Year,sep="-"),
            common = TRUE,
            random = TRUE)
summary(ORR)
data$vaccine_n_N <- paste0(data$N, "/", data$ALT)
data$placebo_n_N <- paste0(data$N1, "/", data$ALT1)
ORR$vaccine_n_N <- paste0(data$N, "/", data$ALT)
ORR$placebo_n_N <- paste0(data$N1, "/", data$ALT1)
pdf("anygrade.pdf",
    width = 22 / 2.54,   
    height = 42 / 2.54,  
    paper = "special",  
    onefile = FALSE)     
forest(ORR,
         digits = 4,
         spacing = 0.80,
         family = "sans",
         fontsize = 10,
         lwd = 2,
         col.diamond.common = "maroon",
         col.diamond.lines.common = "maroon",
         col.diamond.random = "maroon",
         col.diamond.lines.random = "maroon",
         col.square = "skyblue",
         col.study = "lightslategray",
         lty.common = 0,
         lty.random = 0,
         plotwidth = "6cm",
         colgap.forest.left = "0.1cm", 
         colgap.forest.right = "0.1cm", 
         just.forest = "right",
         colgap.left = "0.1cm",
         colgap.right = "0.1cm",
         shade = TRUE,
         vertices = TRUE,
         leftcols = c("studlab", "vaccine_n_N", "placebo_n_N"),
         leftlabs = c("Study", "ICIs\n(n/N)", "control\n(n/N)"),
         label.left = "Favours ICIs",
         label.right = "Favours control",
         grid = NULL,         
        ) 
dev.off()
#subgroup analysis
mg1v <- metagen(log(OR),lower = log(L), upper = log(U),
                data = data, sm="RR", studlab=paste(data$Author,data$Year,sep="-"),
                common = T,subgroup =  TypeICI, print.subgroup.name = FALSE)
summary(mg1v)
pdf("anygrade.pdf",
    width = 22 / 2.54,   
    height = 54 / 2.54, 
    paper = "special",   
    onefile = FALSE)     
forest(mg1v,
       digits = 4,
       spacing = 0.8,
       family = "sans",
       fontsize = 10,
       lwd = 2,
       col.diamond.common = "maroon",
       col.diamond.lines.common = "maroon",
       col.diamond.random = "maroon",
       col.diamond.lines.random = "maroon",
       col.square = "skyblue",
       col.study = "lightslategray",
       lty.common = 0,
       lty.random = 0,
       plotwidth = "6cm",
       colgap.forest.left = "0.1cm", 
       colgap.forest.right = "0.1cm", 
       just.forest = "right",
       colgap.left = "0.1cm",
       colgap.right = "0.1cm",
       shade = TRUE,
       vertices = TRUE,
       leftcols = c("studlab", "vaccine_n_N", "placebo_n_N"),
       leftlabs = c("Study", "ICIs\n(n/N)", "control\n(n/N)"),
       label.left = "Favours ICIs",
       label.right = "Favours control",
       grid = NULL,     
) 
dev.off()
#Publication bias
pdf("angrade.pdf",
    width = 13 / 2.54,   
    height = 10 / 2.54, 
    paper = "a4",  
    onefile = FALSE)   
funnel(ORR,common = F) 
dev.off()
#egger
metabias(ORR,method="linreg", plotit = T,k.min = 7)
dev.off()
#trim-and-fill
#1轮廓增强漏斗图Contour-enhanced funnel plot
tiff('grade3 Contour-enhanced.tiff', width=5000,height=12200,res=300)
funnel(ORR, common = T, pch =16,
       contour.levels = c(0.9, 0.95, 0.99),
       col.contour = c("darkgray", "gray", "lightgray"))
legend(0.45, 0.35, 
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),bty="n")
dev.off
#剪补法trim-and-fill
pdf("grade3 trim-and-fill RR.pdf",
    width = 22 / 2.54,   
    height = 48 / 2.54,  
    paper = "special",  
    onefile = FALSE)    
tf1 <- trimfill(ORR, ma.common = TRUE, 
                common = FALSE, random = TRUE) 
forest(tf1)
tf1
dev.off
tiff('grade3 trim-and-fill.tiff', width=5000,height=12200,res=300)
funnel(tf1, common = T, pch =16,
       contour.levels = c(0.9, 0.95, 0.99),
       col.contour = c("darkgray", "gray", "lightgray"))
legend(0.45, 0.35, 
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),bty="n")
dev.off
#Sensitivity analysis
metainf(ORR, studlab=paste(data$Author,data$Year,sep="-"),pooled = "fixed") 
forest(metainf(ORR, pooled = "random"),
       digits = 4,
       spacing = 0.8,#
       family = "sans",
       fontsize = 10)
dev.off()
