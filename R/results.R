#' Add details to igraphs to hold information about connection activity and agent belief
#' @param model network to apply details to
#' @return model with updated $graphs
#' @importFrom igraph graph_from_adjacency_matrix E E<- V V<- head_of tail_of
detailGraphs <- function(model) {
  # Need to check the iGraph node numbers follow the agents$id/graph ones
  agents <- model$model$agents
  N <- list(p = model$parameters$n_agents, d = model$parameters$n_decisions)

  # transpose
  g <- lapply(model$model$graphs, t)
  g <- lapply(g, graph_from_adjacency_matrix, weighted = T)

  for (i in 1:N$d) {

    active <- rep(0, N$p * N$p)
    # fill in colour for active advice (vectorised)
    active[0:(N$p - 1) * N$p + agents$advisor[agents$decision == i]] <- 'green'

    active <- matrix(active, N$p, N$p)
    active <- t(active)
    active <- as.character(active)
    # drop reciprocal connections
    active <- active[-(seq(1, N$p * N$p, N$p) + 0:(N$p - 1))]
    # overwrite active connections
    E(g[[i]])$active[active != 0] <- active[active != 0]

    # Sensitivity
    E(g[[i]])$sensitivity <- agents$sensitivity[head_of(g[[i]], E(g[[i]]))]

    # Bias difference
    E(g[[i]])$headBias <- agents$bias[head_of(g[[i]], E(g[[i]]))]
    E(g[[i]])$tailBias <- agents$bias[tail_of(g[[i]], E(g[[i]]))]
    E(g[[i]])$sharedBias <- abs(E(g[[i]])$headBias) +
      abs(E(g[[i]])$tailBias) *
      ifelse((E(g[[i]])$headBias < 0) == (E(g[[i]])$tailBias < 0), 1, -1)

    # colour vertices by bias
    V(g[[i]])$bias <- agents$bias[1:N$p]
    V(g[[i]])$biasColour <- ifelse(agents$bias[1:N$p] > 0,
                                   biasToColourString(agents$bias[1:N$p], 'b'),
                                   biasToColourString(agents$bias[1:N$p], 'r'))
  }

  model$model$graphs <- g

  model

}

#' A neat string of the parameters for a model for inclusion in graphs
#' @param model a simulation result model
settingsStr <- function(model) {
  timeElapsed <- difftime(model$times$end, model$times$start)

  paste0('Model parameters:\n',
         'Agents = ', model$parameters$n_agents,
         '; Decisions = ', model$parameters$n_decisions, '; ',
         'ConfidenceWeighted = ', model$parameters$conf, '; \n',
         'Sensitivity SD = ', model$parameters$sensitivitySD, '; ',
         'Bias mean (SD) = +/-', model$parameters$biasMean,
         ' (', model$parameters$biasSD, '); ',
         'Learning rate = ', model$parameters$learningRate, '\n',
         'Model run ', format(model$times$start, "%F_%H-%M-%S"), ' (',
         'runtime = ', round(as.numeric(timeElapsed), 1), 's)')
}

#' Calculate the colour strings for different weights
#' @param graph with weights to use
#' @return list of #RRGGBB codes with colours for the weights
#' @importFrom igraph E
weightToColourString <- function(graph) {
  colours <- E(graph)$weight

  limits <- list(low = .05 * 255, high = .95 * 255)
  colours <- 255 * colours
  colours <- 255 - round(pmin(limits$high, pmax(limits$low, colours)))

  colours <- paste0('#',
                    sprintf("%02X", colours),
                    sprintf("%02X", colours),
                    sprintf("%02X", colours))
  colours
}

#' Return a colour string made up of r/g/b as desired with saturation scaled by
#' b
#' @param b vector of values to use for saturations
#' @param colour any of 'r', 'g', and 'b' to show channels used for saturation
#'   (other channels are filled with FF)
#' @param maxVal maximum value for b
#' @param minVal minimum value for b
#' @param minSaturation minimum saturation value corresponding to minVal
#' @param maxSaturation maximum saturation value corresponding to maxVal
#' @return character vector same length as b containing colour strings
biasToColourString <- function(b, colour = c('r', 'g', 'b'),
                               maxVal = 2.5, minVal = 0,
                               minSaturation = .1, maxSaturation = .9) {
  # take the reciprocal of b so that more pronounced biases are more saturated
  x <- pmax(minVal, pmin(maxVal, abs(1/b)))

  x <- x / maxVal

  x <- pmax(x * 255 * maxSaturation, 255 * minSaturation)

  x <- sprintf('%02X', round(x))

  out <- rep('#', length(b))

  for (clr in c('r', 'g', 'b')) {

    if (clr == colour) {
      out <- paste0(out, 'FF')
    } else {
      out <- paste0(out, x)
    }
  }

  out
}

#' Plot an igraph of a network
#' @param model network to plot
#' @param i generation to plot
#' @param activeColours whether to highlight the active advice connections
#'
#' @return plot object
#'
#' @importFrom igraph E E<- V layout_in_circle
#'
#' @export
plotGraph <- function(model, i, activeColours = T) {

  title <- paste("Advice weights after decision", i - 1)

  lines <- c(3, 4, 5, 1) # line weights light->heavy
  cuts <- length(lines)

  # discrete weight categories for edges
  weight <- as.numeric(cut(E(model$model$graphs[[i]])$weight, cuts))

  # colour lines currently undergoing advice-taking
  # by default use the weight of the connection
  E(model$model$graphs[[i]])$active <-
    weightToColourString(model$model$graphs[[i]])

  plot(model$model$graphs[[i]],
       main = title,
       layout = layout_in_circle,
       vertex.color = V(model$model$graphs[[i]])$biasColour,
       edge.arrow.size = 0.5,
       edge.width = weight / model$parameters$n_agents * 5,
       edge.lty = lines[weight],
       edge.color = E(model$model$graphs[[i]])$active,
       edge.curved = 1 / model$parameters$n_agents)
}

#' Plot a double-graph of network connectivity at the first and last decisions
#' @param model network
#'
#' @importFrom graphics par
#'
#' @return NULL (invisible)
#' @export
networkGraph <- function(model) {
  par(mfrow = c(1,2))
  plotGraph(model, 1, activeColours = F)
  plotGraph(model, model$parameters$n_decisions, activeColours = F)
  invisible(NULL)
}

#' Calculation of bias-tie strength correlations
#' @param model network whose correlations should be calculated
#' @importFrom tibble tibble
#' @importFrom stats cor.test
.biasCorrelation <- function(model) {
  cors <- NULL
  for (d in 1:model$parameters$n_decisions) {
    tmp <- model$model$graphs[[d]]

    test <- cor.test(as.numeric(tmp[attr = 'sharedBias']),
                     as.numeric(tmp[attr = 'weight']))

    cors <- rbind(cors, tibble(decision = d,
                               r = test$estimate,
                               p = test$p.value,
                               ciL = test$conf.int[1],
                               ciH = test$conf.int[2]))
  }

  cors
}

#' Correlation between bias and tie strength at each decision point
#' @param model network to calculate correlations for
#' @return ggplot of correlations
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_hline geom_point geom_errorbar labs scale_y_continuous
#' @importFrom rlang .data
#' @export
biasGraph <- function(model) {
  cors <- .biasCorrelation(model)

  # Plot correlation
  ggplot(cors, aes(x = .data$decision,
                   y = .data$r, ymin = .data$ciL, ymax = .data$ciH,
                   colour = .data$p < .05)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_point() +
    geom_errorbar(width = 0) +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = 'Shared bias x Advice weight',
         subtitle = ifelse(model$parameters$conf,
                           'Confidence-weighted advisor updating',
                           'Agreement-only advisor updating'),
         caption = settingsStr(model))
}

#' Calculate the strength of the sensitivity-advice weight correlation for in- and out-degree
#' @param model network to calculate the correlations for
#' @importFrom tibble tibble
#' @importFrom stats cor.test
.sensitivityCorrelation <- function(model) {
  sens <- NULL
  for (d in 1:model$parameters$n_decisions) {
    tmp <- model$model$graphs[[d]]

    # rows give outdegree, columns indegree
    outdeg <- sapply(1:model$parameters$n_agents, function(i) {
      w <- tmp[attr = "weight"]
      w <- w[i, ]
      mean(w[-i]) # don't include self weight in average
    })
    indeg <- sapply(1:model$parameters$n_agents, function(i) {
      w <- tmp[attr = "weight"]
      w <- w[, i]
      mean(w[-i])
    })

    # Pull out the sensitivity from the columns of the graph edges
    sensVec <- c(tmp[attr = "sensitivity"][2, 1],
                 tmp[attr = "sensitivity"][1, -1])

    testOut <- cor.test(sensVec, outdeg)
    testIn <- cor.test(sensVec, indeg)

    sens <- rbind(sens, tibble(decision = d,
                               direction = "In",
                               r = testIn$estimate,
                               p = testIn$p.value,
                               ciL = testIn$conf.int[1],
                               ciH = testIn$conf.int[2]))
    sens <- rbind(sens, tibble(decision = d,
                               direction = "Out",
                               r = testOut$estimate,
                               p = testOut$p.value,
                               ciL = testOut$conf.int[1],
                               ciH = testOut$conf.int[2]))
  }

  sens
}

#' Correlation between sensitivity and tie strength
#' @param model network to calculate correlations for
#' @return ggplot of correlations
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot geom_hline geom_ribbon geom_line geom_rug labs scale_y_continuous
#' @importFrom rlang .data
#' @export
sensitivityGraph <- function(model) {
  sens <- .sensitivityCorrelation(model)

  ggplot(sens, aes(x = .data$decision,
                   y = .data$r, ymin = .data$ciL, ymax = .data$ciH,
                   fill = .data$direction, colour = .data$direction)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_ribbon(alpha = .25, colour = NA) +
    geom_line() +
    # rug plots to show significant divergences from 0
    geom_rug(data = dplyr::filter(sens, .data$p < .05, .data$direction == 'In'),
             sides = 't', size = model$parameters$n_decisions / 100 + 1) +
    geom_rug(data = dplyr::filter(sens, .data$p < .05, .data$direction == 'Out'),
             sides = 'b', size = model$parameters$n_decisions / 100 + 1) +
    scale_y_continuous(limits = c(-1, 1)) +
    labs(title = 'Sensitivity x Mean advice weight',
         subtitle = paste0(ifelse(model$parameters$conf,
                                  'Confidence-weighted advisor updating',
                                  'Agreement-only advisor updating'),
                           '\nRug marks show where p < .05'),
         caption = settingsStr(model))
}
