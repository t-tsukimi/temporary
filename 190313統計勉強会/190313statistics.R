#190313統計勉強回

#パッケージの読み込み
library(ggplot2)

#有意水準0.05で検定を繰り返したときのFWER(family-wise error rate)の推移
alpha <- 1/6 #さいころ
alpha <- 0.05 #有意水準0.05の検定
FWER <- c()
for (i in 1:100) {
    FWER <- c(FWER, 1 - (1 - alpha) ^ i)    
}
FWER_result <- data.frame(times = 1:100, FWER = FWER)


pdf("fwer.pdf")
ggplot(FWER_result, aes(x = times, y = FWER)) + geom_line() + 
        theme_classic() + 
        theme(axis.title = element_text(size = 20, colour = "black"), 
              axis.text = element_text(size = 20, colour = "black"), 
              axis.line = element_line(colour = "black")) +
        xlab("Number of times")
dev.off()

#本当に有意差がなかったとしても、有意差ありと判断されるのか？
pvec <- c()
for (i in 1:100) {
        result <- t.test(rnorm(10, mean = 1, sd = 1), 
                         rnorm(10, mean = 1, sd = 1), 
                         paired = F, var.equal = T)
        pvec <- c(pvec, result$p.value) 
}

pdf("pvalue_hist.pdf")
ggplot(NULL, aes(x = pvec)) + 
        geom_histogram(bins = 20, fill = "white", colour = "black") +
        theme_classic() +
        theme(axis.title = element_text(size = 20, colour = "black"), 
              axis.text = element_text(size = 20, colour = "black"), 
              axis.line = element_line(colour = "black")) +
        xlab("P value")
dev.off()

