library(orddom)

break.vals = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5)


#===============#
#               #
#   Study 8     #
#               #
#===============#

# data.rev = read.table(file="Revolver Data Study 2-3.txt", header=T)

data.rev <- read.table(file="../data/Revolver Data Study 2-3.txt", header=T)

Trent = data.rev$Trent
Hammer = data.rev$Hammer
Powder = data.rev$Powder
Bullet = data.rev$Bullet


par(mfrow=c(2,2))

hist(Trent, breaks=break.vals, ylim=c(0,50), main="Trent Caused", xlab="Answer")
hist(Hammer, breaks=break.vals, ylim=c(0,50), main="Hammer Caused", xlab="Answer")
hist(Powder, breaks=break.vals, ylim=c(0,50), main="Powder Caused", xlab="Answer")
hist(Bullet, breaks=break.vals, ylim=c(0,50), main="Bullet Caused", xlab="Answer")


Neutral.rev = rep(4, length(Trent))

wilcox.test(x=Trent, y=Neutral.rev, paired=T)    # V = 1296.5, p-value = 1.055e-10
wilcox.test(x=Hammer, y=Neutral.rev, paired=T)   # V = 182.5, p-value = 5.978e-05
wilcox.test(x=Powder, y=Neutral.rev, paired=T)   # V = 117, p-value = 5.927e-06
wilcox.test(x=Bullet, y=Neutral.rev, paired=T)   # V = 552.5, p-value = 0.469

orddom(x=Trent, y=Neutral.rev, paired=T)         # X>Y: 0.980, Y>X: 0.020
orddom(x=Hammer, y=Neutral.rev, paired=T)        # X>Y: 0.176, Y>X: 0.706
orddom(x=Powder, y=Neutral.rev, paired=T)        # X>Y: 0.098, Y>X: 0.745
orddom(x=Bullet, y=Neutral.rev, paired=T)        # X>Y: 0.510, Y>X: 0.353


# One-sided tests against neutral value

wilcox.test(x=Trent, mu=4, alternative="greater")   # V = 1296.5, p-value = 5.277e-11
wilcox.test(x=Hammer, mu=4, alternative="less")   # V = 182.5, p-value = 2.989e-05
wilcox.test(x=Powder, mu=4, alternative="less")   # V = 117, p-value = 2.963e-06
wilcox.test(x=Bullet, mu=4, alternative="less")   # V = 552.5, p-value = 0.7694
wilcox.test(x=Bullet, mu=4, alternative="greater")   # V = 552.5, p-value = 0.2345




#=================================#
#                                 #
#   Study 798 --Event Variation   #
#                                 #
#=================================#


data.rev.798 = read.table(file="Revolver Data Study 798.txt", header=T)

Trent.798 = data.rev.798$Trent
Hammer.798 = data.rev.798$Hammer
Powder.798 = data.rev.798$Powder
Bullet.798 = data.rev.798$Bullet


par(mfrow=c(2,2))

hist(Trent.798, breaks=break.vals, ylim=c(0,80), main="Trent Caused", xlab="Answer")
hist(Hammer.798, breaks=break.vals, ylim=c(0,80), main="Hammer Released Caused", xlab="Answer")
hist(Powder.798, breaks=break.vals, ylim=c(0,80), main="Powder Igniting Caused", xlab="Answer")
hist(Bullet.798, breaks=break.vals, ylim=c(0,80), main="Bullet Driven Caused", xlab="Answer")


Neutral.rev.798 = rep(4, length(Trent.798))

wilcox.test(x=Trent.798, y=Neutral.rev.798, paired=T)    # V = 3840, p-value = 2.835e-15
wilcox.test(x=Hammer.798, y=Neutral.rev.798, paired=T)   # V = 3715, p-value = 8.952e-11
wilcox.test(x=Powder.798, y=Neutral.rev.798, paired=T)   # V = 4034.5, p-value = 2.234e-15
wilcox.test(x=Bullet.798, y=Neutral.rev.798, paired=T)   # V = 3030.5, p-value = 2.274e-6

orddom(x=Trent.798, y=Neutral.rev.798, paired=T)         # X>Y: 0.924, Y>X: 0.054
orddom(x=Hammer.798, y=Neutral.rev.798, paired=T)        # X>Y: 0.870, Y>X: 0.130
orddom(x=Powder.798, y=Neutral.rev.798, paired=T)        # X>Y: 0.924, Y>X: 0.076
orddom(x=Bullet.798, y=Neutral.rev.798, paired=T)        # X>Y: 0.707, Y>X: 0.250
