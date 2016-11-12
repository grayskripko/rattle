ggVarImp <- function(model, ...) UseMethod("ggVarImp")

ggVarImp.randomForest <- function(model, 
                                  main="Random Forest Variable Importance",
                                  sub=genPlotTitleCmd(vector=TRUE),
                                  ...)
{
  randomForest::importance(model) %>%
    data.frame() %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    tidyr::gather(Measure, Importance, -Variable) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=Importance, fill=Variable)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::facet_wrap(~ Measure) +
    ggplot2::ggtitle(label=main) +
    ggplot2::xlab(label=sub) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                   legend.position="none")
  #  ggplot2::theme(axis.text.x=ggplot2::element_blank(),
  #                 axis.ticks.x=ggplot2::element_blank())  
}

ggVarImp.rpart <- function(model, main, sub, ...)
{
  model$variable.importance %>%
    data.frame() %>%
    magrittr::set_names("Importance") %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=Importance, fill=Variable)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                   legend.position="none")
}

ggVarImp.rxDForest <- function(model,
                               main="Big Data Random Forest Variable Importance",
                               sub=genPlotTitleCmd(vector=TRUE),
                               ...)
{
  model$importance %>%
    data.frame() %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=IncNodePurity, fill=Variable)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::ggtitle(label=main) +
    ggplot2::xlab(label=sub) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                   legend.position="none")
}
