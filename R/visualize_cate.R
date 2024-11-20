#' Title
#'
#' @param cate_obj
#' @param which_plot
#' @param captions
#'
#' @return
#' @export
#'
#' @examples
plot.cate <- function(cate_obj,
                      which_plot = 1:3,
                      captions = c("Histogram of CATEs",
                                   "Boxplot of CATEs vs Study Membership",
                                   "Confidence Intervals for CATEs"),
                      ask = TRUE) {

  assertthat::assert_that(
    inherits(cate_obj, "cate"),
    msg = "use only with \"cate\" objects"
  )

  assertthat::assert_that(
    is.numeric(which_plot),
    all(dplyr::between(which_plot, 1, 3)),
    msg = "'which_plot' must be in 1:3"
  )

  show <- rep(FALSE, 3)
  show[which_plot] <- TRUE

  model <- cate_obj$model

  if (ask) {
    devAskNewPage(TRUE)
    on.exit(devAskNewPage(FALSE))
  }

  plots <- list()

  if (show[1]) {
    plots <- c(plots,
               list(ggplot2::ggplot(model, ggplot2::aes(x = tau_hat)) +
                      ggplot2::geom_histogram(color="black", fill="white", bins=30) +
                      ggplot2::xlab("CATE Estimate")))
  }

  if (show[2]) {
    site_col <- cate_obj$site_col
    plots <- c(plots,
               list(ggplot2::ggplot(model, ggplot2::aes(x = !!rlang::sym(site_col), y = tau_hat)) +
                      ggplot2::geom_boxplot() +
                      ggplot2::xlab("Study/Site ID") +
                      ggplot2::ylab("CATE Estimate")))
  }

  if (show[3]) {
    plots <- c(plots,
               list(model %>%
                      dplyr::arrange(tau_hat) %>%
                      tibble::rownames_to_column(var = "id_ord") %>%
                      dplyr::mutate(id_ord = as.numeric(id_ord),
                                    lower = tau_hat - 1.96 * sqrt(var_hat),
                                    upper = tau_hat + 1.96 * sqrt(var_hat)) %>%
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
                                     axis.ticks.x = ggplot2::element_blank())))
  }

  for (p in plots) {
    print(p)
  }
}

#' Title
#'
#' @param cate_obj
#'
#' @return
#' @export
#'
#' @examples
plot_interpretationtree <- function(cate_obj) {
    covariate_col <- cate_obj$covariate_col
    fml <- as.formula(paste("tau_hat ~ ", paste(covariate_col, collapse="+")))
    int_tree <- rpart::rpart(fml, data = model)
    int_tree_pruned <- rpart::prune(int_tree,
                                    cp = int_tree$cptable[which.min(int_tree$cptable[,"xerror"]),
                                                          "CP"])
    rpart.plot::prp(int_tree, varlen = 15)
}

#' Title
#'
#' @param cate_obj
#' @param covariate_name
#'
#' @return
#' @export
#'
#' @examples
plot_vteffect <- function(cate_obj, covariate_name) {
  model <- cate_obj$model
  ggplot2::ggplot(model, ggplot2::aes(x = !!rlang::sym(covariate_name),
                                        y = tau_hat,
                                        color = !!rlang::sym(site_col))) +
    ggplot2::geom_point() +
    ggplot2::xlab(covariate_name) +
    ggplot2::ylab("CATE Estimate")
}

#' Title
#'
#' @param cate_obj
#' @param combine
#'
#' @return
#' @export
#'
#' @examples
plot_blp <- function(cate_obj,
                     combine = FALSE) {
  fm <- as.formula(paste("~ 1 + ", paste(covariate_col, collapse = "+")))
  feat <- model.matrix(fm, cate_obj$model)
  blpList <- grf::best_linear_projection(cate_obj$causalforest, A = feat)

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


