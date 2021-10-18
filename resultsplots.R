library(tidyverse)
library(rstanarm)
library(lattice)
library(plotly)
source("~/plotlydetails.R") # load credentials for plotly login

# function for caterpillar plot - lme4 only --------------
ggCaterpillar <- function(re, QQ=TRUE, likeDotplot=TRUE) {
  require(ggplot2)
  f <- function(x) {
    pv   <- attr(x, "postVar")
    cols <- 1:(dim(pv)[1])
    se   <- unlist(lapply(cols, function(i) sqrt(pv[i, i, ])))
    ord  <- unlist(lapply(x, order)) + rep((0:(ncol(x) - 1)) * nrow(x), each=nrow(x))
    pDf  <- data.frame(y=unlist(x)[ord],
                       ci=1.96*se[ord],
                       nQQ=rep(qnorm(ppoints(nrow(x))), ncol(x)),
                       ID=factor(rep(rownames(x), ncol(x))[ord], levels=rownames(x)[ord]),
                       ind=gl(ncol(x), nrow(x), labels=names(x)))
    
    if(QQ) {  ## normal QQ-plot
      p <- ggplot(pDf, aes(nQQ, y))
      p <- p + facet_wrap(~ ind, scales="free")
      p <- p + xlab("Standard normal quantiles") + ylab("Random effect quantiles")
    } else {  ## caterpillar dotplot
      p <- ggplot(pDf, aes(ID, y)) + coord_flip()
      if(likeDotplot) {  ## imitate dotplot() -> same scales for random effects
        p <- p + facet_wrap(~ ind)
      } else {           ## different scales for random effects
        p <- p + facet_grid(ind ~ ., scales="free_y")
      }
      p <- p + xlab("Levels") + ylab("Random effects")
    }
    
    p <- p + theme(legend.position="none")
    p <- p + geom_hline(yintercept=0)
    p <- p + geom_errorbar(aes(ymin=y-ci, ymax=y+ci), width=0, colour="black")
    p <- p + geom_point(size=1.0, colour="blue") 
    return(p)
  }
  
  lapply(re, f)
}










# results ------------------
# awareness
out3 <- readRDS("GDS_awareness_rstanarm_results.RDS")
print(out3)
str(data$awareness)

rstan::traceplot(out3$stanfit, inc_warmup = TRUE)
#launch_shinystan(out3, ppd = FALSE) #slow, use ppd = FALSE to speed up
lme4::ranef(out1a) # compare with lme4 model
rstanarm::ranef(out3)
# observed data ratios for comparison with model on plots
colombia <- mean(data$awareness[data$ncountry == 'Colombia'])
finland <- mean(data$awareness[data$ncountry == 'Finland'])
colombia / mean(data$awareness)
finland / mean(data$awareness)

# believe
out4 <- readRDS("GDS_believe_rstanarm_results.RDS")
print(out4)
rstan::traceplot(out4$stanfit, inc_warmup = TRUE)
#launch_shinystan(out4, ppd = FALSE) #slow, use ppd = FALSE to speed up
lme4::ranef(out1b) # compare with lme4 model
rstanarm::ranef(out4)
# observed data ratios for comparison with model on plots
colombia <- mean(data$believe[data$ncountry == 'Colombia'])
finland <- mean(data$believe[data$ncountry == 'Finland'])
colombia / mean(data$believe)
finland / mean(data$believe)

# relevance
out5 <- readRDS("GDS_relevance_rstanarm_results.RDS")
print(out5)
rstan::traceplot(out5$stanfit, inc_warmup = TRUE)
#launch_shinystan(out5, ppd = FALSE) #slow, use ppd = FALSE to speed up
lme4::ranef(out1c) # compare with lme4 model 
rstanarm::ranef(out5)
# observed data ratios for comparison with model on plots
colombia <- mean(data$relevance[data$ncountry == 'Colombia'])
israel <- mean(data$relevance[data$ncountry == 'Israel'])
colombia / mean(data$relevance)
israel / mean(data$relevance)

# drink_less
out6 <- readRDS("GDS_less_rstanarm_results.RDS")
print(out6)
rstan::traceplot(out6$stanfit, inc_warmup = TRUE)
str(out6$stanfit)
#pvp <- posterior_vs_prior(out6, pars = c("alpha"))
#pvp
#ps <- prior_summary(out6)
#ps


#launch_shinystan(out6, ppd = FALSE) #slow, use ppd = FALSE to speed up
lme4::ranef(out1d) # compare with lme4 model
rstanarm::ranef(out6)
# observed data ratios for comparison with model on plots
colombia <- mean(data$less[data$ncountry == 'Colombia'])
israel <- mean(data$less[data$ncountry == 'Israel'])
colombia / mean(data$less)
israel / mean(data$less)




# plots -------------------------------
# caterpillar plots for lme4 random effects
p1a <- ggCaterpillar(lme4::ranef(out1a,
                                 condVar = TRUE),
                     QQ = FALSE, 
                     likeDotplot = FALSE) 
p1b <- ggCaterpillar(lme4::ranef(out1b,
                                 condVar = TRUE),
                     QQ = FALSE, 
                     likeDotplot = FALSE) 
p1c <- ggCaterpillar(lme4::ranef(out1c,
                                 condVar = TRUE),
                     QQ = FALSE, 
                     likeDotplot = FALSE) 
p1d <- ggCaterpillar(lme4::ranef(out1a,
                                 condVar = TRUE),
                     QQ = FALSE, 
                     likeDotplot = FALSE) 

# ggplot caterpillar plots for stan random effects

stanresults <- list(out3,out4,out5,out6)
whichplot <- c("awareness","don\'t believe","relevance","drink less")
#x <- 1

stancat <- lapply(seq_along(stanresults), function(x) {
  res <- as.data.frame(rstanarm::ranef(stanresults[[x]]))
  res$grp <- as.character(levels(res$grp))
  res$grp[res$grp == "Macedonia, The Former Yugoslav Republic of"] <- "FYR Macedonia"
  res <- cbind(res$grp,round(res[c("condval")],3))
  stanout_sims <- extract(stanresults[[x]]$stanfit, permuted = TRUE)
  overall_intercept <- exp(median(stanout_sims[["alpha"]])) # overall intercept on original scale
  ci90 <- posterior_interval(stanresults[[x]], prob = 0.90, 
                             regex_pars = "^[b]")
  ci50 <- posterior_interval(stanresults[[x]], prob = 0.50, 
                             regex_pars = "^[b]")
  res <- cbind(res,round(ci90, 3),round(ci50, 3))
  res <- res[,c(1,2,3,5,6,4)]
  res.1 <- cbind(res[,c(1)],exp(res[,c(2:6)]))
  rownames(res.1) <- NULL
  colnames(res.1) <- c("country","median","lowest","low","high","highest")
  titlelab <- paste0("Ratio of individual country intercept \nto overall intercept for \"",whichplot[x],"\"") 
  myannotation <- paste0("(overall intercept on the original \nvariable scale = ", round(overall_intercept,2),")")
  
  p <- ggplot(res.1, aes(x = median, 
                       y = reorder(country,median))) + 
        geom_errorbarh(aes(xmax = high, 
                       xmin = low, 
                       size = 1, 
                       colour = 5, 
                       alpha = .8, 
                       height = .00)) +
        geom_errorbarh(aes(xmax = highest, 
                       xmin = lowest, 
                       colour = 1, 
                       alpha = .5, 
                       height = .00)) + 
        geom_point(colour = "red", 
               size = 1.5) +
        geom_vline(xintercept = 1, 
               size = .5, 
               alpha = .2) +
        scale_x_continuous(limits = c(0.6, 1.7)) + 
        theme(legend.position = "none") +
        theme(text = element_text(family = "Helvetica")) +
        labs(x = NULL, y = NULL) +
        annotate("text", 
             x = 1.3, 
             y = 8, 
             size = 2.5, 
             label = paste0(titlelab,"\n",myannotation)) +  
        theme(axis.title = element_text(size = 10)) +
        theme(axis.text.y = element_text(size = 6)) 
  p
})

p.awareness <- stancat[[1]]
p.believe <- stancat[[2]]
p.relevance <- stancat[[3]]
p.less <- stancat[[4]]

# plots to plotly
subp <- plotly::subplot(p.awareness, p.believe, p.relevance, p.less, nrows = 2, margin = 0.08, heights = c(0.5, 0.5))

chart_link <- api_create(subp, filename = "GDS-alcinfo-rstanarm")
chart_link













#
#
#
#
#
#
#
# not so useful stuff, kept for info --------
#### trying efa of 7 messages
# awaredat <- data.frame(
#   data$alcheartnew,
#   data$alclivernew,
#   data$alccancernew,
#   data$alcfatnew,
#   data$alcfreedaysnew,
#   data$alcmythnew,
#   data$alcviolencenew
# )
# str(awaredat)
# 
# awarefa <- psych::fa(awaredat,
#                      nfactors = 2,
#                      rotate = "oblimin")
# awarefa
# loadings(awarefa) # doesn't show anything very clearly, and probably also varies by country

