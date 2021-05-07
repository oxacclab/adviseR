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
  head_group <- NULL; tail_group <- NULL

  for (i in 1:N$d) {
    A <- agents[agents$decision == i, ]

    active <- rep(0, N$p * N$p)
    # fill in colour for active advice (vectorised)
    active[0:(N$p - 1) * N$p + A$advisor] <- 'green'

    active <- matrix(active, N$p, N$p)
    active <- t(active)
    active <- as.character(active)
    # drop reciprocal connections
    active <- active[-(seq(1, N$p * N$p, N$p) + 0:(N$p - 1))]
    # overwrite active connections
    E(g[[i]])$active[active != 0] <- active[active != 0]

    # Sensitivity
    E(g[[i]])$sensitivity <- A$sensitivity[head_of(g[[i]], E(g[[i]]))]

    # Bias difference
    E(g[[i]])$headBias <- A$bias[head_of(g[[i]], E(g[[i]]))]
    E(g[[i]])$tailBias <- A$bias[tail_of(g[[i]], E(g[[i]]))]
    E(g[[i]])$biasSimilarity <- 1 - abs(E(g[[i]])$headBias - E(g[[i]])$tailBias)

    # colour vertices by bias
    V(g[[i]])$bias <- A$bias
    V(g[[i]])$biasColour <- ifelse(
      A$bias > .5,
      biasToColourString(A$bias, 'b'),
      biasToColourString(1 - A$bias, 'r')
    )

    # group at time 1
    if (is.null(head_group))
      head_group <- A$bias[head_of(g[[i]], E(g[[i]]))] > .5
    if (is.null(tail_group))
      tail_group <- A$bias[tail_of(g[[i]], E(g[[i]]))] > .5

    E(g[[i]])$head_group <- head_group
    E(g[[i]])$tail_group <- tail_group
  }

  model$model$graphs <- g

  model

}

#' Return the ratio of weights of in-:out-group connections
#' @param g graph for which to determine the ratio
#' @param original_groups whether to use the original groups (\code{T}) or
#'   assign groups based on biases in the current graph (\code{F})
#' @return double representing the ratio of mean weights in the graph for all
#'   agents
#' @export
groupRatio <- function(g, original_groups = T) {
  a <- edge_attr(g)
  if (original_groups) {
    a$sameGroup = a$head_group == a$tail_group
  } else {
    a$sameGroup = (a$headBias > .5) == (a$tailBias > .5)
  }

  mean(a$weight[a$sameGroup]) / mean(a$weight[!a$sameGroup])
}

#' A neat string of the parameters for a model for inclusion in graphs
#' @param model a simulation result model
settingsStr <- function(model) {
  timeElapsed <- difftime(model$times$end, model$times$start)

  paste0('Model parameters:\n',
         'Agents = ', model$parameters$n_agents,
         '; Decisions = ', model$parameters$n_decisions,
         '; Weighted sampling (SD) = ', model$parameters$weighted_sampling_mean,
         ' (', model$parameters$weighted_sampling_sd, '); ',
         '; Confidence-weighted = ', model$parameters$confidence_weighted, ';\n',
         'Sensitivity SD = ', model$parameters$sensitivity_sd, '; ',
         'Bias mean (SD) = +/-', model$parameters$bias_mean,
         ' (', model$parameters$bias_sd, '); ',
         'Trust/Bias volatility (SD) = ',
         model$parameters$trust_volatility_mean,
         ' (', model$parameters$trust_volatility_sd, ') / ',
         model$parameters$bias_volatility_mean,
         ' (', model$parameters$bias_volatility_sd, ');\n',
         'Custom starting trust = ', model$parameters$starting_graph_type, '; ',
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
    if (clr %in% colour) {
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
#' @param layout layout for the graph
#' @inheritDotParams igraph::plot.igraph
#'
#' @return plot object
#'
#' @importFrom igraph E E<- V layout_nicely
#'
#' @export
plotGraph <- function(
  model,
  i,
  activeColours = T,
  layout = igraph::layout_nicely,
  ...
) {

  title <- paste("Advice weights after decision", i - 1)

  lines <- c(3, 4, 5, 1) # line weights light->heavy
  cuts <- length(lines)

  # discrete weight categories for edges
  weight <- as.numeric(cut(E(model$model$graphs[[i]])$weight, cuts))

  # colour lines currently undergoing advice-taking
  # by default use the weight of the connection
  E(model$model$graphs[[i]])$active <-
    weightToColourString(model$model$graphs[[i]])

  plot(
    model$model$graphs[[i]],
    main = title,
    layout = layout,
    vertex.color = V(model$model$graphs[[i]])$biasColour,
    edge.arrow.size = 0.5,
    edge.width = weight / model$parameters$n_agents * 5,
    edge.lty = lines[weight],
    edge.color = E(model$model$graphs[[i]])$active,
    edge.curved = 1 / model$parameters$n_agents,
    ...
  )
}

#' Plot multiple graphs of network connectivity to show network evolution
#' @param model network
#' @param timepoints either an integer representing the number of evenly-spaced
#'   time points (including first and final), or an integer vector giving the
#'   time points to inspect.
#' @inheritDotParams plotGraph
#'
#' @importFrom graphics par
#'
#' @return NULL (invisible)
#' @export
networkGraph <- function(model, timepoints = 2, ...) {
  nTP <- if (length(timepoints) == 1) timepoints else length(timepoints)
  if (nTP > 1 && length(timepoints) == 1) {
    timepoints <- round(seq(1, length(model$model$graphs), length.out = nTP))
  }
  if (nTP < 1)
    stop('At least one timepoint must be selected.')
  old <- switch(
    if (nTP > length(letters)) 'z' else letters[nTP],
    a = NULL,
    b = par(mfrow = c(1, 2)),
    c = par(mfrow = c(1, 3)),
    d = par(mfrow = c(2, 2)),
    par(mfrow = c(ceiling(nTP / 3), 3))
  )
  for (tp in timepoints) {
    plotGraph(model, tp, activeColours = F, ...)
  }
  if (!is.null(old)) {
    par(old)
  }
  invisible(NULL)
}

#' Calculation of bias-tie strength correlations
#' @param model network whose correlations should be calculated
#' @param use_starting_bias whether to use the starting bias as opposed to the
#'   current bias at each timestep
#' @importFrom tibble tibble
#' @importFrom stats cor.test
.biasCorrelation <- function(model, use_starting_bias = F) {
  cors <- NULL
  biases <- NULL
  for (d in 1:model$parameters$n_decisions) {
    tmp <- model$model$graphs[[d]]
    if (all(is.null(biases)) | !use_starting_bias)
      biases <- as.numeric(tmp[attr = 'biasSimilarity'])

    test <- cor.test(biases, as.numeric(tmp[attr = 'weight']))

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
#' @param use_starting_bias whether to use the starting bias as opposed to the
#'   current bias at each timestep
#' @return ggplot of correlations
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_hline geom_point geom_errorbar labs scale_y_continuous
#' @importFrom rlang .data
#' @export
biasGraph <- function(model, use_starting_bias = F) {
  cors <- .biasCorrelation(model, use_starting_bias = use_starting_bias)

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

#' Calculate the strength of the sensitivity-advice weight correlation for in-
#' and out-degree
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
#' @importFrom ggplot2 ggplot geom_hline geom_ribbon geom_line geom_rug labs
#'   scale_y_continuous
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

#' Return a graph of the biases of the model agents and their change over time.
#' @param model model to inspect
#' @param summaryFun function to use to summarise each group's bias
#' @importFrom dplyr select mutate
#' @importFrom rlang .data
#' @importFrom tidyr nest unnest
#' @importFrom ggplot2 ggplot geom_hline geom_line stat_summary geom_rect
#' scale_alpha_manual scale_y_continuous
#' @export
biasEvolution <- function(model, summaryFun = stats::median) {
  select(
    model$model$agents,
    .data$id,
    .data$decision,
    .data$bias
  ) %>%
    nest(d = -.data$id) %>%
    mutate(d = map(.data$d, ~mutate(., group = .data$bias[[1]]))) %>%
    unnest(cols = .data$d) %>%
    mutate(group = if_else(.data$group > .5, 'Right', 'Left')) %>%
    # Add in bias update flag info
    nest(d = -.data$decision) %>%
    mutate(
      skipBiasUpdate = bitwAnd(
        model$parameters$decision_flags[.data$decision],
        2
      ) != 2,
      skipBiasUpdate = factor(.data$skipBiasUpdate, levels = c(T, F))
    ) %>%
    unnest(cols = .data$d) %>%
    ggplot(aes(x = .data$decision, y = .data$bias, colour = .data$group)) +
    geom_rect(aes(
      xmin = .data$decision - .5,
      xmax = .data$decision + .5,
      ymin = -Inf, ymax = Inf,
      alpha = .data$skipBiasUpdate
    ), fill = 'grey85', colour = NA) +
    geom_hline(yintercept = .5, linetype = 'dashed') +
    geom_line(aes(group = paste0(.data$id)), alpha = .25) +
    stat_summary(geom = 'line', aes(group = .data$group),
                 size = 1, fun = summaryFun) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_alpha_manual(values = c(1, 0), breaks = c(T, F), drop = F)
}

#' Return a list with a variety of model graphs and stats
#' @param m model to inspect
#' @importFrom igraph edge_attr E V head_of
#' @importFrom dplyr mutate summarise group_by select if_else %>%
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_wider nest unnest
#' @importFrom purrr map_dbl map
#' @importFrom ggplot2 ggplot geom_hline geom_boxplot geom_line geom_segment
#'   geom_label scale_y_continuous coord_cartesian facet_grid labs
#' @importFrom rlang .data
#' @importFrom ez ezANOVA
#'
#' @export
inspectModel <- function(m) {
  weights <- m$model$graphs[[m$parameters$n_decisions]] %>%
    edge_attr() %>%
    as_tibble() %>%
    mutate(
      group = factor(if_else(.data$headBias > .5, 'Right', 'Left')),
      sameGroup = factor(if_else(
        (.data$headBias > .5) == (.data$tailBias > .5),
        'Same group',
        'Different groups'
      )),
      id = factor(head_of(
        m$model$graphs[[m$parameters$n_decisions]],
        E(m$model$graphs[[m$parameters$n_decisions]])
      ))
    ) %>%
    group_by(.data$id, .data$group, .data$sameGroup) %>%
    summarise(weight = mean(.data$weight), .groups = 'drop')

  p <- weights %>%
    nest(d = -.data$group) %>%
    mutate(
      p = tryCatch(
        map_dbl(.data$d, ~ t.test(weight ~ sameGroup, data = .)$p.value),
        error = function(e) {NA_real_}
      ),
      p = round(.data$p, 5)
    )

  dw <- .1

  list(

    biasGraph = biasGraph(m),

    aov = tryCatch(
      suppressWarnings({ezANOVA(
        data = weights,
        dv = quote(weight),
        wid = quote(id),
        within = quote(sameGroup),
        between = quote(group),
        type = 2
      )$ANOVA}),
      error = function(e) {NULL}
    ),

    aovGraph = weights %>%
      ggplot(aes(x = .data$sameGroup, y = .data$weight,
                 colour = .data$group, fill = .data$group)) +
      geom_hline(yintercept = m$parameters$starting_graph,
                 linetype = 'dashed') +
      geom_boxplot(outlier.shape = NA, size = 1, width = dw/2,
                   aes(group = .data$sameGroup),
                   colour = 'black') +
      geom_line(aes(group = .data$id)) +
      geom_segment(x = 1, xend = 2, y = .9, yend = .9, colour = 'black') +
      geom_label(y = .9, x = 1.5, colour = 'black', fill = 'white',
                 aes(label = paste0('p ', .data$p)),
                 data = p) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      coord_cartesian(clip = F) +
      facet_grid(~.data$group) +
      labs(x = 'Group membership with advisor', y = 'Weight given to advice'),
    # caption = "Simulated trust for average agents.  Simulated agents' trust in other agents at the end of the simulation. Individual lines show the average trust for an agent in those of the same or different group. Violins and boxplots show the distributions of these averages. The groups are arbitrarily named and separate agents by bias strength (whether the bias is positive or negative). Both groups contain some agents with pronounced biases and some with negligible biases.  The dashed line indicates the starting trust level between all agents in the simulation."

    # Plot bias evolution for each model
    biasEvolution = biasEvolution(m)
  )
}
