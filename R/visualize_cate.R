#' Plot Diagnostics for a CATE Object
#'
#' @description
#' Five plots (selectable by `which_plot`) currently available: a histogram of estimated conditional
#' average treatment effects (CATEs), a boxplot of CATEs stratified by study membership, a plot of
#' 95% confidence intervals for all CATEs sorted in order to CATE estimate value, a best linear
#' projection (only available when `estimation_method` is set to "causalforest"), and an
#' interpretation tree.
#'
#' @param object list. An object resulting from \link{estimate_cate}.
#' @param which_plot numeric vector. A vector indicating which plots should be generated.
#' @param ask logical. When TRUE, the user is asked before each plot, see
#'  \link[graphics:par]{par(ask = .)}.
#'
#' @export
#'
#' @examples
plot.cate <- function(object,
                      which_plot = 1:5,
                      ask = TRUE) {
  assertthat::assert_that(
    inherits(object, "cate"),
    msg = "use only with \"cate\" objects"
  )

  assertthat::assert_that(
    is.numeric(which_plot),
    all(dplyr::between(which_plot, 1, 5)),
    msg = "'which_plot' must be in 1:3"
  )

  model <- object$model
  covariate_col <- object$covariate_col

  if (3 %in% which_plot) {
    if (!("variance_estimates" %in% colnames(model))) {
      cli::cli_alert_warning(
        "Variance estimates missing from model output. 95% CI plot will not be produced."
      )
      which_plot <- setdiff(which_plot, 3)
    }
  }

  if (4 %in% which_plot) {
    if (!("causal_forest" %in% class(object$estimation_object))) {
      cli::cli_alert_warning(
        "Object of class 'causal_forest' required for best linear projection figure."
      )
      which_plot <- setdiff(which_plot, 4)
    }
  }

  show <- rep(FALSE, 5)
  show[which_plot] <- TRUE

  if (ask) {
    devAskNewPage(TRUE)
    on.exit(devAskNewPage(FALSE))
  }

  if (show[1]) {
    p <- ggplot2::ggplot(model, ggplot2::aes(x = tau_hat)) +
      ggplot2::geom_histogram(color="black", fill="white", bins=30) +
      ggplot2::xlab("CATE Estimate")
    print(p)
  }

  if (show[2]) {
    study_col <- object$study_col
    p <- ggplot2::ggplot(model, ggplot2::aes(x = !!rlang::sym(study_col), y = tau_hat)) +
      ggplot2::geom_boxplot() +
      ggplot2::xlab("Study ID") +
      ggplot2::ylab("CATE Estimate")
    print(p)
  }

  if (show[3]) {
    p <- model %>%
      dplyr::arrange(tau_hat) %>%
      tibble::rownames_to_column(var = "id_ord") %>%
      dplyr::mutate(id_ord = as.numeric(id_ord),
                    lower = tau_hat - 1.96 * sqrt(variance_estimates),
                    upper = tau_hat + 1.96 * sqrt(variance_estimates)) %>%
      ggplot2::ggplot(ggplot2::aes(x = id_ord, y = tau_hat)) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = lower,
                                          ymax = upper),
                             colour = "lightgray") +
      ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                          linetype = "dashed",
                          color = "blue") +
      ggplot2::geom_point() +
      ggplot2::xlab("Profile ID (by CATE magnitude)") +
      ggplot2::ylab("CATE Estimate (95% CI)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())
    print(p)

  }

  if (show[4]) {
    fm <- as.formula(paste("~ 1 + ", paste(covariate_col, collapse = "+")))
    feat <- model.matrix(fm, object$model)
    blpList <- grf::best_linear_projection(object$estimation_object, A = feat)

    blps <- jtools::plot_summs(blpList,
                               point.shape = FALSE)
    dfg <- blps$data
    dfg$study <- dfg$model

    p <- ggplot2::ggplot(dfg,
                         ggplot2::aes(x = study,
                                      y = estimate,
                                      color = term,
                                      group = term)) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0,
                          linetype = "dashed",
                          color = "black",
                          size = 0.65) +
      ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.75),
                               ggplot2::aes(ymin = conf.low, ymax = conf.high),
                               alpha = 0.85) +
      ggplot2::ggtitle("CATE Best Linear Projection by Covariate") +
      theme_MH +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())

    print(p)
  }

  if (show[5]) {
    fml <- as.formula(paste("tau_hat ~ ", paste(covariate_col, collapse="+")))
    int_tree <- rpart::rpart(fml, data = model)
    int_tree_pruned <- rpart::prune(int_tree,
                                    cp = int_tree$cptable[which.min(int_tree$cptable[,"xerror"]),
                                                          "CP"])
    rpart.plot::prp(int_tree, varlen = 15)
  }
}

#' Plot Variable Treatment Effect
#'
#' @param object list. An object resulting from \link{estimate_cate}.
#' @param covariate_name string. Name of a covariate included in dataset used to estimate tau_hat.
#'
#' @export
#'
#' @details
#' This function plots the value of the selected covariate for each observation in the dataset
#' against the value of tau_hat for the variable. This is what the findings mean...
#'
#' @examples
plot_vteffect <- function(object, covariate_name) {
  model <- object$model
  study_col <- object$study_col
  ggplot2::ggplot(model, ggplot2::aes(x = !!rlang::sym(covariate_name),
                                        y = tau_hat,
                                        color = !!rlang::sym(study_col))) +
    ggplot2::geom_point() +
    ggplot2::xlab(covariate_name) +
    ggplot2::ylab("CATE Estimate")
}

#' Plot Best Linear Projection
#'
#' @param object list. An object resulting from \link{estimate_cate}.
#'
#' @export
#'
#' @details
#' Additional details...
#'
#' @examples
plot_blp <- function(object) {
  fm <- as.formula(paste("~ 1 + ", paste(object$covariate_col, collapse = "+")))
  feat <- model.matrix(fm, object$model)
  blpList <- grf::best_linear_projection(object$causalforest, A = feat)

  blps <- jtools::plot_summs(blpList,
                             point.shape = FALSE)
  dfg <- blps$data
  dfg$study <- dfg$model

  ggplot2::ggplot(dfg,
                           ggplot2::aes(x = study,
                                        y = estimate,
                                        color = term,
                                        group = term)) +
    ggplot2::coord_flip() +
    ggplot2::geom_hline(yintercept = 0,
                        linetype = "dashed",
                        color = "black",
                        size = 0.65) +
    ggplot2::geom_pointrange(position = ggplot2::position_dodge(width = 0.75),
                             ggplot2::aes(ymin = conf.low, ymax = conf.high),
                             alpha = 0.85) +
    ggplot2::ggtitle("CATE Best Linear Projection by Covariate") +
    theme_MH +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(axis.text.y = ggplot2::element_blank())
}


