# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

library(data.table)
library(MVN)
library(lavaan)
library(effsize)
library(hexbin)

source("functions.R")
source("colors.R")  # use pre-defined colors

################################################################################

VERSION <- "2020-04-16"

recoded <- fread(paste0("data/export_", VERSION, ".csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", -99, "NA"), stringsAsFactors=FALSE)

################################################################################

### histogram organization size ########
organization_size <- as.integer(recoded$OrganizationSize)
organization_size_table <- table(organization_size, useNA="always")

quartz(type="pdf", file="figures/histogram_organization-size.pdf", width=22, height=10) # prevents unicode issues in pdf
#pdf("figures/histogram_organization-size.pdf", bg="transparent", width=12, height=8, encoding="utf8")
par(
  bg="white",
  #mar = c(1.2, 1.0, 0.1, 0.0)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.5, 0.0, 0.0)+0.1,  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.5,
  cex.main=1.5,
  cex.sub=1.5,
  cex.lab=1.5,
  cex.axis=1.5
)

barplot(organization_size_table, ylim=c(0, 700), xaxt="n", yaxt="n", border="white", col="white")
for (y in seq(0, 700, by=100)) {
  segments(x0=-1, y0=y, x1=8.4, y1=y, lty=1, lwd=1, col=gray_lighter)
}
barplot(organization_size_table, ylim=c(0, 700), add=TRUE, names.arg=c("0-9", "10-99", "100-999", "1,000-9,999", "10,000-99,999", "\u2265 100,000", "n/a"))
#title(xlab="Organization size", font.lab=3)
title(ylab="Number of responses", font.lab=3)

par(mfrow = c(1, 1))
dev.off() 
########################################

### histogram education ################
education <- recoded$Education+1
education_table <- table(education, useNA="always")

quartz(type="pdf", file="figures/histogram_education.pdf", width=22, height=10) # prevents unicode issues in pdf
#pdf("figures/histogram_organization-size.pdf", bg="transparent", width=12, height=8, encoding="utf8")
par(
  bg="white",
  #mar = c(1.2, 1.0, 0.1, 0.0)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.5, 0.0, 0.0)+0.1,  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.5,
  cex.main=1.5,
  cex.sub=1.5,
  cex.lab=1.5,
  cex.axis=1.5
)

barplot(education_table, ylim=c(0, 1000), xaxt="n", yaxt="n", border="white", col="white")
for (y in seq(0, 1000, by=100)) {
  segments(x0=-1, y0=y, x1=7.2, y1=y, lty=1, lwd=1, col=gray_lighter)
}
barplot(education_table, ylim=c(0, 1000), add=TRUE, names.arg=c("No post-sec.", "Some post-sec.", "Undergraduate", "Masters", "PhD", "n/a"))
#title(xlab="Organization size", font.lab=3)
title(ylab="Number of responses", font.lab=3)

par(mfrow = c(1, 1))
dev.off() 
########################################

### test hypothesis 1 ##################

# WHO5B
recoded$WHO5B <- as.integer(recoded$WHO5B1) + as.integer(recoded$WHO5B2) + as.integer(recoded$WHO5B3) + as.integer(recoded$WHO5B4) + as.integer(recoded$WHO5B5)

shapiro.test(recoded$WHO5B)
# W = 0.96531, p-value < 2.2e-16

# WHO5S
recoded$WHO5S <- as.integer(recoded$WHO5S1) + as.integer(recoded$WHO5S2) + as.integer(recoded$WHO5S3) + as.integer(recoded$WHO5S4) + as.integer(recoded$WHO5S5)

shapiro.test(recoded$WHO5S)
# W = 0.97818, p-value < 2.2e-16

# WHO5B vs. WHO5S
WHO5B_WHO5S <- recoded[,c("WHO5B", "WHO5S")]
WHO5B_WHO5S <- WHO5B_WHO5S[complete.cases(WHO5B_WHO5S)]
n <- nrow(WHO5B_WHO5S)
n
# 2,194

wilcox.test(WHO5B_WHO5S$WHO5S,
            WHO5B_WHO5S$WHO5B,
            alternative="two.sided", paired=T, correct=T)
# V = 645610, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cliff.delta(WHO5B_WHO5S$WHO5S,
            WHO5B_WHO5S$WHO5B,
            paired=TRUE)
# delta estimate: -0.1247475 (negligible)
# 95 percent confidence interval:
#   inf         sup 
#   -0.15846992 -0.09073436

length(which(WHO5B_WHO5S$WHO5S < WHO5B_WHO5S$WHO5B))
# 1103
length(which(WHO5B_WHO5S$WHO5S == WHO5B_WHO5S$WHO5B))
# 346
length(which(WHO5B_WHO5S$WHO5S > WHO5B_WHO5S$WHO5B))
# 745

cor.test(WHO5B_WHO5S$WHO5B, WHO5B_WHO5S$WHO5S, method="spearman")
# S = 1223100000, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.3051127
# Warning message: Cannot compute exact p-value with ties

########################################

### test hypothesis 2 ##################

# we omit items 7 and 9

# HPQB
recoded$HPQB <- as.integer(recoded$HPQB1) - as.integer(recoded$HPQB2) - as.integer(recoded$HPQB3) - as.integer(recoded$HPQB4) - as.integer(recoded$HPQB5) - as.integer(recoded$HPQB6) + as.integer(recoded$HPQB8)

shapiro.test(recoded$HPQB)
# W = 0.98803, p-value = 1.919e-12

# HPQS
recoded$HPQS <- as.integer(recoded$HPQS1) - as.integer(recoded$HPQS2) - as.integer(recoded$HPQS3) - as.integer(recoded$HPQS4) - as.integer(recoded$HPQS5) - as.integer(recoded$HPQS6) + as.integer(recoded$HPQS8)

shapiro.test(recoded$HPQS)
# W = 0.98472, p-value = 3.253e-14

# HPQB vs. HPQS
HPQB_HPQS <- recoded[,c("HPQB", "HPQS")]
HPQB_HPQS <- HPQB_HPQS[complete.cases(HPQB_HPQS)]
n <- nrow(HPQB_HPQS)
n
# 2,078

# shift scale
min_hpq <- min(HPQB_HPQS$HPQB, min(HPQB_HPQS$HPQS))
min_hpq
# -23
HPQB_HPQS$HPQB <- HPQB_HPQS$HPQB+abs(min_hpq)
HPQB_HPQS$HPQS <- HPQB_HPQS$HPQS+abs(min_hpq)

wilcox.test(HPQB_HPQS$HPQS,
            HPQB_HPQS$HPQB,
            alternative="two.sided", paired=T, correct=T)
# V = 566520, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0

cliff.delta(HPQB_HPQS$HPQS,
            HPQB_HPQS$HPQB,
            paired=TRUE)
# delta estimate: -0.1290975 (negligible)
# 95 percent confidence interval:
#   inf         sup 
# -0.16370614 -0.09417155 

length(which(HPQB_HPQS$HPQS < HPQB_HPQS$HPQB))
# 1068
length(which(HPQB_HPQS$HPQS == HPQB_HPQS$HPQB))
# 293
length(which(HPQB_HPQS$HPQS > HPQB_HPQS$HPQB))
# 717

cor.test(HPQB_HPQS$HPQS, HPQB_HPQS$HPQB, method="spearman")
# S = 767920000, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#   rho 
# 0.4865112 
# Warning message: Cannot compute exact p-value with ties

########################################

### plots ##############################

h <- hist(recoded$WHO5B, breaks=20, density=10, col=gray_selected, xlab="WHO5B", main="") 
xfit <- seq(min(recoded$WHO5B, na.rm=TRUE), max(recoded$WHO5B, na.rm=TRUE), length = 40) 
yfit <- dnorm(xfit, mean = mean(recoded$WHO5B, na.rm=TRUE), sd = sd(recoded$WHO5B, na.rm=TRUE)) 
yfit <- yfit * diff(h$mids[1:2]) * length(recoded$WHO5B) 
lines(xfit, yfit, col = "black", lwd = 2)

#qqline(recoded$WHO5B)
#qqnorm(recoded$WHO5B)

h <- hist(recoded$WHO5S, breaks=20, density=10, col=gray_selected, xlab="WHO5S", main="") 
xfit <- seq(min(recoded$WHO5S, na.rm=TRUE), max(recoded$WHO5S, na.rm=TRUE), length = 40) 
yfit <- dnorm(xfit, mean = mean(recoded$WHO5S, na.rm=TRUE), sd = sd(recoded$WHO5S, na.rm=TRUE)) 
yfit <- yfit * diff(h$mids[1:2]) * length(recoded$WHO5S) 
lines(xfit, yfit, col = "black", lwd = 2)

plot(isoreg(WHO5B_WHO5S$WHO5B, WHO5B_WHO5S$WHO5S), col=rgb(60,60,60,50,maxColorValue=255), pch=16, cex=2.5)

quartz(type="pdf", file="figures/histogram_WHO5.pdf", width=22, height=10) # prevents unicode issues in pdf
#pdf("figures/histogram_organization-size.pdf", bg="transparent", width=12, height=8, encoding="utf8")
par(
  bg="white",
  #mar = c(1.2, 1.0, 0.1, 0.0)+0.1, # subplot margins (bottom, left, top, right)
  #omi = c(0.2, 0.5, 0.0, 0.0)+0.1,  # outer margins in inches (bottom, left, top, right)
  mfrow = c(1, 1),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.5,
  cex.main=1.5,
  cex.sub=1.5,
  cex.lab=1.5,
  cex.axis=1.5
)

h_b <- hist(WHO5B_WHO5S$WHO5B, breaks=20, density=10, col=gray_dark, border=gray_dark, xlab="WHO5B (striped/dashed) vs. WHO5S (solid)", main="", ylim=c(0,300), xlim=c(5, 30)) 
h_s <- hist(WHO5B_WHO5S$WHO5S, breaks=20, col=gray_light_transparent, border=gray_dark, xlab="", ylab="", main="", ylim=c(0,300), xlim=c(5, 30), add=TRUE)

mean_b <- mean(WHO5B_WHO5S$WHO5B)
xfit_b <- seq(min(WHO5B_WHO5S$WHO5B), max(WHO5B_WHO5S$WHO5B), length=50) 
yfit_b <- dnorm(xfit_b, mean=mean_b, sd=sd(WHO5B_WHO5S$WHO5B)) 
yfit_b <- yfit_b * diff(h_b$mids[1:2]) * length(WHO5B_WHO5S$WHO5B) 
lines(xfit_b, yfit_b, col=gray_dark, lwd=2, lty=2)

mean_s <- mean(WHO5B_WHO5S$WHO5S)
xfit_s <- seq(min(WHO5B_WHO5S$WHO5S), max(WHO5B_WHO5S$WHO5S), length=50) 
yfit_s <- dnorm(xfit_s, mean=mean_s, sd=sd(WHO5B_WHO5S$WHO5S)) 
yfit_s <- yfit_s * diff(h$mids[1:2]) * length(WHO5B_WHO5S$WHO5S) 
lines(xfit_s, yfit_s, col=gray_dark, lwd=2, lty=1)

#segments(mean_b, 0, mean_b, 182, col=gray_dark, lwd=2, lty=2)
#segments(mean_s, 0, mean_s, 161, col=gray_dark, lwd=2, lty=1)

par(mfrow = c(1, 1))
dev.off() 

plot(isoreg(WHO5B_WHO5S$WHO5B, WHO5B_WHO5S$WHO5S), col=rgb(60,60,60,50,maxColorValue=255), pch=16, cex=2.5)

quartz(type="pdf", file="figures/histogram_WHO5_HPQ.pdf", width=22, height=16) # prevents unicode issues in pdf
#pdf("figures/histogram_organization-size.pdf", bg="transparent", width=12, height=8, encoding="utf8")
par(
  bg="white",
  mar = c(1.3, 0.3, 0.3, 0.1)+0.1, # subplot margins (bottom, left, top, right)
  omi = c(0.2, 0.6, 0.0, 0.0)+0.1,  # outer margins in inches (bottom, left, top, right)
  mfrow = c(2, 2),
  #pin = (width, height)
  #mfcol # draw in columns
  # increase font size
  cex=1.5,
  cex.main=1.5,
  cex.sub=1.5,
  cex.lab=1.5,
  cex.axis=1.5
)

x_range <- c(0, 30)
x_breaks <- seq(0, 30)
x_labels <- seq(0, 30, 5)
y_range <- c(0, 300)
label_cex <- 2.4

plot_segments <- function() {
  for (y in seq(0, 300, by=50)) {
    segments(x0=-1, y0=y, x1=30, y1=y, lty=1, lwd=1, col=gray_lighter)
  }
} 

hist(WHO5B_WHO5S$WHO5B, breaks=x_breaks, col="white", border="white", xlab="", xaxt='n', ylab="", main="", ylim=y_range, xlim=x_range) 
plot_segments()
h_wb <- hist(WHO5B_WHO5S$WHO5B, breaks=x_breaks, col=gray_lighter, border=gray_dark, xlab="", ylab="", xaxt='n', main="", ylim=y_range, xlim=x_range, add=TRUE) 
mean_wb <- mean(WHO5B_WHO5S$WHO5B)
median_wb <- median(WHO5B_WHO5S$WHO5B)
xfit_wb <- seq(min(WHO5B_WHO5S$WHO5B), max(WHO5B_WHO5S$WHO5B), length=50) 
yfit_wb <- dnorm(xfit_wb, mean=mean_wb, sd=sd(WHO5B_WHO5S$WHO5B)) 
yfit_wb <- yfit_wb * diff(h_wb$mids[1:2]) * length(WHO5B_WHO5S$WHO5B) 
lines(xfit_wb, yfit_wb, col=gray_dark, lwd=2, lty=2)
segments(mean_wb, -10, mean_wb, 182, col=gray_dark, lwd=2, lty=2)
segments(median_wb, -10, median_wb, 180, col=gray_dark, lwd=3, lty=1)
axis(1, at=x_labels, labels=rep("", 7))
mtext(side=3, "WHO5 before switch", line=-3.5, cex=label_cex) # \u2193

hist(WHO5B_WHO5S$WHO5S, breaks=x_breaks, col="white", border="white", xlab="", xaxt='n', ylab="", yaxt='n', main="", ylim=y_range, xlim=x_range) 
plot_segments()
h_ws <- hist(WHO5B_WHO5S$WHO5S, breaks=x_breaks, col=gray_lighter, border=gray_dark, xlab="", xaxt='n', ylab="", yaxt='n', main="", ylim=y_range, xlim=x_range, add=TRUE)
mean_ws <- mean(WHO5B_WHO5S$WHO5S)
median_ws <- median(WHO5B_WHO5S$WHO5S)
xfit_ws <- seq(min(WHO5B_WHO5S$WHO5S), max(WHO5B_WHO5S$WHO5S), length=50) 
yfit_ws <- dnorm(xfit_ws, mean=mean_ws, sd=sd(WHO5B_WHO5S$WHO5S)) 
yfit_ws <- yfit_ws * diff(h_ws$mids[1:2]) * length(WHO5B_WHO5S$WHO5S) 
lines(xfit_ws, yfit_ws, col=gray_dark, lwd=2, lty=2)
segments(mean_ws, -10, mean_ws, 161, col=gray_dark, lwd=2, lty=2)
segments(median_ws, -10, median_ws, 159, col=gray_dark, lwd=3, lty=1)
axis(1, at=x_labels, labels=rep("", 7))
mtext(side=3, "WHO5 since switch", line=-3.5, cex=label_cex)

hist(HPQB_HPQS$HPQB, breaks=x_breaks, col="white", border="white", xlab="", ylab="", main="", ylim=y_range, xlim=x_range) 
plot_segments()
h_hb <- hist(HPQB_HPQS$HPQB, breaks=x_breaks, col=gray_lighter, border=gray_dark, xlab="", ylab="", main="", ylim=y_range, xlim=x_range, add=TRUE) 
mean_hb <- mean(HPQB_HPQS$HPQB)
median_hb <- median(HPQB_HPQS$HPQB)
xfit_hb <- seq(min(HPQB_HPQS$HPQB), max(HPQB_HPQS$HPQB), length=50) 
yfit_hb <- dnorm(xfit_hb, mean=mean_hb, sd=sd(HPQB_HPQS$HPQB)) 
yfit_hb <- yfit_hb * diff(h_hb$mids[1:2]) * length(HPQB_HPQS$HPQB) 
lines(xfit_hb, yfit_hb, col=gray_dark, lwd=2, lty=2)
segments(mean_hb, -10, mean_hb, 197, col=gray_dark, lwd=2, lty=2)
segments(median_hb, -10, median_hb, 196.5, col=gray_dark, lwd=3, lty=1)
mtext(side=3, "HPQ before switch", line=-3.5, cex=label_cex)

hist(HPQB_HPQS$HPQS, breaks=x_breaks, col="white", border="white", xlab="", ylab="", yaxt='n', main="", ylim=y_range, xlim=x_range) 
plot_segments()
h_hs <- hist(HPQB_HPQS$HPQS, breaks=x_breaks, col=gray_lighter, border=gray_dark, xlab="", ylab="", yaxt='n', main="", ylim=y_range, xlim=x_range, add=TRUE) 
mean_hs <- mean(HPQB_HPQS$HPQS)
median_hs <- median(HPQB_HPQS$HPQS)
xfit_hs <- seq(min(HPQB_HPQS$HPQS), max(HPQB_HPQS$HPQS), length=50) 
yfit_hs <- dnorm(xfit_hs, mean=mean_hs, sd=sd(HPQB_HPQS$HPQS)) 
yfit_hs <- yfit_hs * diff(h_hs$mids[1:2]) * length(HPQB_HPQS$HPQS) 
lines(xfit_hs, yfit_hs, col=gray_dark, lwd=2, lty=2)
segments(mean_hs, -10, mean_hs, 158, col=gray_dark, lwd=2, lty=2)
segments(median_hs, -10, median_hs, 157.5, col=gray_dark, lwd=3, lty=1)
mtext(side=3, "HPQ since switch", line=-3.5, cex=label_cex)

par(mfrow = c(1, 1))
dev.off() 
