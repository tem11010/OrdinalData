# The is the script to create the liability threshold figure (figure 1) in Verhulst & Neale "Best Practices for Binary or Ordinal Data Analysis"
# The code was written by Brad Verhulst on June 2nd, 2020.

# call the rerquire packages
require(MASS)

# Tell R that we want 4 subfigures
par(mfrow = c(2,2))

# Figure 1a. Symmetrical 3 Category LTM figure
nTh <- 2
prev <- c((1:nTh)/(nTh+1))
quants1<-qnorm(prev)

z1 <- -300:300/100

plot(z1, dnorm(z1), type = "l", lwd = 3, col = 'red', xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", bty = 'n', ylim = c(0, .45), main = "a) 3 Category Symmetrical \n Liability Distribution")

polygon(c(seq(-3, quants1[1], by = .01),rev(seq(-3, quants1[1], by = .01))), c(dnorm(seq(-3, quants1[1], by = .01)), rep(0, length(seq(-3, quants1[1], by = .01)))), density = c(20), angle = c(-45), col = "red")
polygon(c(seq(3, quants1[2], by = -.01),rev(seq(3, quants1[2], by = -.01))), c(dnorm(seq(3, quants1[2], by = -.01)), rep(0, length(seq(3, quants1[2], by = -.01)))), density = c(20), angle = c(45), col = "blue")
polygon(c(seq(quants1[1], quants1[2], by = .01),rev(seq(quants1[1], quants1[2], by = .01))), c(dnorm(seq(quants1[1], quants1[2], by = .01)), rep(0, length(seq(quants1[1], quants1[2], by = .01)))), density = c(20), angle = c(90), col = "purple")

lines((z1), dnorm(z1), lwd = 3, col = "red")

lines( c(quants1[1],quants1[1]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants1[2],quants1[2]), c(.00, .42), col = c("black"), lwd = 2)

mtext("Liability", side = 1, at = 0, col = "black", line = 2, font = 3, family = "serif", cex = 1.5)
mtext("0", side = 1, at = -1.5, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("1", side = 1, at = 0, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("2", side = 1, at = 1.5, col = "black", line = 0, font = 3, family = "serif", cex = .75)

text(quants1[1], .45, expression(tau[1]),  col = "black", cex = .75)
text(quants1[2], .45, expression(tau[2]),  col = "black", cex = .75)


######
# Figure 1b. Asymmetrical 3 Category LTM figure


th <- (.5)/(nTh)
prevs <-  seq(.5, 1-th, th)
quants2<-qnorm( prevs)


z1 <- -300:300/100

plot(z1, dnorm(z1), type = "l", lwd = 3, col = 'red', xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", bty = 'n', ylim = c(0, .45), main = "b) 3 Category Asymmetrical \n Liability Distribution")

polygon(c(seq(-3, quants2[1], by = .01),rev(seq(-3, quants2[1], by = .01))), c(dnorm(seq(-3, quants2[1], by = .01)), rep(0, length(seq(-3, quants2[1], by = .01)))), density = c(20), angle = c(-45), col = "red")
polygon(c(seq(3, quants2[2], by = -.01),rev(seq(3, quants2[2], by = -.01))), c(dnorm(seq(3, quants2[2], by = -.01)), rep(0, length(seq(3, quants2[2], by = -.01)))), density = c(20), angle = c(45), col = "blue")
polygon(c(seq(quants2[1], quants2[2], by = .01),rev(seq(quants2[1], quants2[2], by = .01))), c(dnorm(seq(quants2[1], quants2[2], by = .01)), rep(0, length(seq(quants2[1], quants2[2], by = .01)))), density = c(20), angle = c(90), col = "purple")

lines((z1), dnorm(z1), lwd = 3, col = "red")

lines( c(quants2[1],quants2[1]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants2[2],quants2[2]), c(.00, .42), col = c("black"), lwd = 2)

mtext("Liability", side = 1, at = 0, col = "black", line = 2, font = 3, family = "serif", cex = 1.5)
mtext("0", side = 1, at = -1, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("1", side = 1, at = .3, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("2", side = 1, at = 1.8, col = "black", line = 0, font = 3, family = "serif", cex = .75)

text(quants2[1], .45, expression(tau[1]),  col = "black", cex = .75)
text(quants2[2], .45, expression(tau[2]),  col = "black", cex = .75)

######
# Figure 1c. Symmetrical 7 Category LTM figure

colors <- rainbow(7)

nTh <- 6
prev <- c((1:nTh)/(nTh+1))
quants1<-qnorm(prev)

plot(z1, dnorm(z1), type = "l", lwd = 3, col = 'red', xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", bty = 'n', ylim = c(0, .45), main = "c) 7 Category Symmetrical \n Liability Distribution")

polygon(c(seq(-3, quants1[1], by = .01),rev(seq(-3, quants1[1], by = .01))), c(dnorm(seq(-3, quants1[1], by = .01)), rep(0, length(seq(-3, quants1[1], by = .01)))), col = colors[1])
polygon(c(seq(quants1[1], quants1[2], by = .01),rev(seq(quants1[1], quants1[2], by = .01))), c(dnorm(seq(quants1[1], quants1[2], by = .01)), rep(0, length(seq(quants1[1], quants1[2], by = .01)))), col = colors[2])
polygon(c(seq(quants1[2], quants1[3], by = .01),rev(seq(quants1[2], quants1[3], by = .01))), c(dnorm(seq(quants1[2], quants1[3], by = .01)), rep(0, length(seq(quants1[2], quants1[3], by = .01)))), col = colors[3])
polygon(c(seq(quants1[3], quants1[4], by = .01),rev(seq(quants1[3], quants1[4], by = .01))), c(dnorm(seq(quants1[3], quants1[4], by = .01)), rep(0, length(seq(quants1[3], quants1[4], by = .01)))), col = colors[4])
polygon(c(seq(quants1[4], quants1[5], by = .01),rev(seq(quants1[4], quants1[5], by = .01))), c(dnorm(seq(quants1[4], quants1[5], by = .01)), rep(0, length(seq(quants1[4], quants1[5], by = .01)))), col = colors[5])
polygon(c(seq(quants1[5], quants1[6], by = .01),rev(seq(quants1[5], quants1[6], by = .01))), c(dnorm(seq(quants1[5], quants1[6], by = .01)), rep(0, length(seq(quants1[5], quants1[6], by = .01)))), col = colors[6])
polygon(c(seq(3, quants1[6], by = -.01),rev(seq(3, quants1[6], by = -.01))), c(dnorm(seq(3, quants1[6], by = -.01)), rep(0, length(seq(3, quants1[6], by = -.01)))), col = colors[7])

lines((z1), dnorm(z1), lwd = 3, col = "red")

lines( c(quants1[1],quants1[1]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants1[2],quants1[2]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants1[3],quants1[3]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants1[4],quants1[4]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants1[5],quants1[5]), c(.00, .42), col = c("black"), lwd = 2)
lines( c(quants1[6],quants1[6]), c(.00, .42), col = c("black"), lwd = 2)

mtext("Liability", side = 1, at = 0, col = "black", line = 2, font = 3, family = "serif", cex = 1.5)
mtext("0", side = 1, at = -1.75, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("1", side = 1, at = -0.80, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("2", side = 1, at = -0.39, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("3", side = 1, at = 0, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("4", side = 1, at = .39, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("5", side = 1, at = .8, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("6", side = 1, at = 1.75, col = "black", line = 0, font = 3, family = "serif", cex = .75)

text(quants1[1], .45, expression(tau[1]),  col = "black", cex = .75)
text(quants1[2], .45, expression(tau[2]),  col = "black", cex = .75)
text(quants1[3], .45, expression(tau[3]),  col = "black", cex = .75)
text(quants1[4], .45, expression(tau[4]),  col = "black", cex = .75)
text(quants1[5], .45, expression(tau[5]),  col = "black", cex = .75)
text(quants1[6], .45, expression(tau[6]),  col = "black", cex = .75)


######
# Figure 1d. Asymmetrical 3 Category LTM figure

th <- (.5)/(nTh)
prevs <-  seq(.5, 1-th, th)
quants2<-qnorm( prevs)


plot(z1, dnorm(z1), type = "l", lwd = 3, col = 'red', xaxt = "n", yaxt = "n", xlab = " ", ylab = " ", bty = 'n', ylim = c(0, .45), main = "d) 7 Category Asymmetrical \n Liability Distribution")

polygon(c(seq(-3, quants2[1], by = .01),rev(seq(-3, quants2[1], by = .01))), c(dnorm(seq(-3, quants2[1], by = .01)), rep(0, length(seq(-3, quants2[1], by = .01)))), col = colors[1])
polygon(c(seq(quants2[1], quants2[2], by = .01),rev(seq(quants2[1], quants2[2], by = .01))), c(dnorm(seq(quants2[1], quants2[2], by = .01)), rep(0, length(seq(quants2[1], quants2[2], by = .01)))), col = colors[2])
polygon(c(seq(quants2[2], quants2[3], by = .01),rev(seq(quants2[2], quants2[3], by = .01))), c(dnorm(seq(quants2[2], quants2[3], by = .01)), rep(0, length(seq(quants2[2], quants2[3], by = .01)))), col = colors[3])
polygon(c(seq(quants2[3], quants2[4], by = .01),rev(seq(quants2[3], quants2[4], by = .01))), c(dnorm(seq(quants2[3], quants2[4], by = .01)), rep(0, length(seq(quants2[3], quants2[4], by = .01)))), col = colors[4])
polygon(c(seq(quants2[4], quants2[5], by = .01),rev(seq(quants2[4], quants2[5], by = .01))), c(dnorm(seq(quants2[4], quants2[5], by = .01)), rep(0, length(seq(quants2[4], quants2[5], by = .01)))), col = colors[5])
polygon(c(seq(quants2[5], quants2[6], by = .01),rev(seq(quants2[5], quants2[6], by = .01))), c(dnorm(seq(quants2[5], quants2[6], by = .01)), rep(0, length(seq(quants2[5], quants2[6], by = .01)))), col = colors[6])
polygon(c(seq(3, quants2[6], by = -.01),rev(seq(3, quants2[6], by = -.01))), c(dnorm(seq(3, quants2[6], by = -.01)), rep(0, length(seq(3, quants2[6], by = -.01)))), col = colors[7])

lines((z1), dnorm(z1), lwd = 3, col = "red")

lines( c(quants2[1],quants2[1]), c(.00, .43), col = c("black"), lwd = 2)
lines( c(quants2[2],quants2[2]), c(.00, .43), col = c("black"), lwd = 2)
lines( c(quants2[3],quants2[3]), c(.00, .43), col = c("black"), lwd = 2)
lines( c(quants2[4],quants2[4]), c(.00, .43), col = c("black"), lwd = 2)
lines( c(quants2[5],quants2[5]), c(.00, .43), col = c("black"), lwd = 2)
lines( c(quants2[6],quants2[6]), c(.00, .43), col = c("black"), lwd = 2)

mtext("Liability", side = 1, at = 0, col = "black", line = 2, font = 3, family = "serif", cex = 1.5)
mtext("0", side = 1, at = -1, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("1", side = 1, at = .11, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("2", side = 1, at = .32, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("3", side = 1, at = .54, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("4", side = 1, at = .84, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("5", side = 1, at = 1.2, col = "black", line = 0, font = 3, family = "serif", cex = .75)
mtext("6", side = 1, at = 2.1, col = "black", line = 0, font = 3, family = "serif", cex = .75)

text(quants2[1], .45, expression(tau[1]),  col = "black", cex = .75)
text(quants2[2], .45, expression(tau[2]),  col = "black", cex = .75)
text(quants2[3], .45, expression(tau[3]),  col = "black", cex = .75)
text(quants2[4], .45, expression(tau[4]),  col = "black", cex = .75)
text(quants2[5], .45, expression(tau[5]),  col = "black", cex = .75)
text(quants2[6], .45, expression(tau[6]),  col = "black", cex = .75)

