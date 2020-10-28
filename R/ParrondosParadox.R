#' Main function of analysis of Parrondos Paradox
#' @param runs number of iterations
#' @param noplays number of games or plays for one iteration
#' @param alpha toDo bias parameter of coins
#' @param profit0 toDo starting capital at n = 0
#' @param singlePlot plot a result after an amount of iterations
#' @export
parrondosParadox <- function(runs = 1,
                             noplays = 500,
                             alpha = 0.005,
                             profit0 = 0,
                             singlePlot = 100,
                             genSeq = "") {
  ret <- createDirs()
  genSeq <- toupper(gsub("[^AB]","",genSeq))
  seqLen <- nchar(genSeq)
  subTitle <- genSeq
  if(seqLen == 0){
    subTitle <- "Random Strategy"
  }
  opts = ggplot2::theme(
    legend.position = "bottom",
    legend.background = ggplot2::element_rect(colour = "black"),
    panel.background = ggplot2::element_rect(fill = "gray98"),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA),
    axis.line = ggplot2::element_line(size = 0.5, colour = "black"),
    axis.ticks = ggplot2::element_line(colour = "black"),
    panel.grid.major = ggplot2::element_line(colour = "gray75", linetype = 2),
    panel.grid.minor = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(colour = "gray25", size = 15),
    axis.text.x = ggplot2::element_text(colour = "gray25", size = 15),
    text = ggplot2::element_text(size = 20),
    plot.title = ggplot2::element_text(size = 35)
  )
  ####################################################################
  #EVOLUTION runs = 1 DISTRIBUTION runs > 10
  ####################################################################
  results2 = data.frame(
    Play = numeric(0),
    ProfitA = numeric(0),
    ProfitB = numeric(0),
    ProfitAB = numeric(0)
  )
  for (run in 1:runs) {
    results = data.frame(
      Play = 0,
      ProfitA = profit0,
      ProfitB = profit0,
      ProfitAB = profit0,
      GameSequence = ""
    )
    for (i in 1:noplays) {
      retA <- PlayGameA(profit = results[results$Play == (i - 1), 2],
                        x = alpha,
                        c = 0.5)
      retB <- PlayGameB(
        profit = results[results$Play == (i - 1), 3],
        x1 = alpha,
        c1 = 0.75,
        x2 = alpha,
        c2 = 0.1
      )
      if(nchar(genSeq) > 0) {
        ind <- i%%seqLen
        if(ind == 0) {
          ind <- seqLen
        }
        tmp <- substr(genSeq,start=ind,stop=ind)
        ret <- ifelse(tmp == "A", T, F)
      } else {
        ret <- runif(1) < 0.5
      }
      if (ret) {
        retAB <- PlayGameA(profit = results[results$Play == (i - 1), 4],
                           x = alpha,
                           c = 0.5)
        gameSeq <- "A"
      } else{
        retAB <- PlayGameB(
          profit = results[results$Play == (i - 1), 4],
          x1 = alpha,
          c1 = 0.75,
          x2 = alpha,
          c2 = 0.1
        )
        gameSeq <- "B"
      }
      tmpRes = data.frame(
        Play = i,
        ProfitA = retA,
        ProfitB = retB,
        ProfitAB = retAB,
        GameSequence = gameSeq
      )
      results = rbind(results, tmpRes)
    }
    if (run %% singlePlot == 1) {
      tmpRes = rbind(
        data.frame(
          Play = results$Play,
          Game = "A",
          Profit = results$ProfitA
        ),
        data.frame(
          Play = results$Play,
          Game = "B",
          Profit = results$ProfitB
        ),
        data.frame(
          Play = results$Play,
          Game = "A+B",
          Profit = results$ProfitAB
        )
      )
      tmpName <- paste("Evolution of profit games along",
                       noplays,
                       "plays, iteration",
                       run)
      ggplot2::ggplot(tmpRes,
                      ggplot2::aes(
                        Profit,
                        x = Play,
                        y = Profit,
                        color = Game
                      )) +
        ggplot2::scale_x_continuous(limits = c(0, noplays), "Plays") +
        ggplot2::scale_y_continuous(limits = c(-75, 75),
                                    expand = c(0, 0),
                                    "Profit") +
        ggplot2::labs(title = tmpName, subtitle = subTitle) +
        ggplot2::geom_line(size = 3) + opts
      tmpName <- paste0("single-res-of-run-", run, ".png")
      ret <- ggplot2::ggsave(filename = tmpName, path = "plotres")
      tmpName <- paste0("excelres/", gsub(".png$", ".xlsx", tmpName))
      ret <- writexl::write_xlsx(x = results, path = tmpName)
    }
    results2 = rbind(results2, results[results$Play == noplays,])
  }
  if(runs > 10) {
    tmpRes = rbind(
      data.frame(Game = "A", Profit = results2$ProfitA),
      data.frame(Game = "B", Profit = results2$ProfitB),
      data.frame(Game = "A+B", Profit = results2$ProfitAB)
    )
    results2[,"GameSequence"] <- NULL
    ggplot2::ggplot(tmpRes, ggplot2::aes(Profit, fill = Game)) +
      ggplot2::scale_x_continuous(limits = c(-150, 150), "Profit") +
      ggplot2::scale_y_continuous(limits = c(0, 0.035),
                                  expand = c(0, 0),
                                  "Density") +
      ggplot2::labs(title = paste("Parrondo's Paradox (", as.character(noplays), " plays)", sep =
                                    ""), subtitle = subTitle) +
      ggplot2::geom_density(alpha = .75) + opts
    tmpName <- paste0(
      format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),
      "-runs-",
      runs,
      "-per-plays-",
      noplays,
      "-dp.png"
    )
    ret <- ggplot2::ggsave(filename = tmpName, path = "plotres")
    tmpName <- paste0("excelres/", gsub(".png$", ".xlsx", tmpName))
    ret <- writexl::write_xlsx(x = results2, path = tmpName)
  }
}

PlayGameA <- function(profit, x, c) {
  if (runif(1) < c - x) {
    profit + 1
  } else {
    profit - 1
  }
}

PlayGameB <- function(profit, x1, c1, x2, c2) {
  if (profit %% 3 > 0) {
    PlayGameA(profit, x = x1, c = c1)
  } else {
    PlayGameA(profit, x = x2, c = c2)
  }
}

createDirs <- function() {
  if(!dir.exists("plotres")) {
    ret <- dir.create("plotres")
  }
  if(!dir.exists("excelres")) {
    ret <- dir.create("excelres")
  }
}