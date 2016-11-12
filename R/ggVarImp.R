ggVarImp <- function(model, ...) UseMethod("ggVarImp")

ggVarImp.randomForest <- function(model, 
                                  main="Random Forest Variable Importance",
                                  sub=genPlotTitleCmd(vector=TRUE),
                                  ...)
{
  randomForest::importance(model) %>%
    data.frame() %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    dplyr::arrange(desc(MeanDecreaseAccuracy)) %>%
    tidyr::gather(Measure, Importance, -Variable) %>%
    dplyr::mutate(Variable=factor(Variable, levels=rev(unique(Variable)))) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=Importance, fill=Variable)) +
    ggplot2::geom_bar(stat="identity", position="identity") +
    ggplot2::facet_wrap(~ Measure) +
    ggplot2::ggtitle(label=main) +
    ggplot2::ylab(label=sub) +
    ggplot2::xlab("") +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                   legend.position="none")
}

ggVarImp.rpart <- function(model,
                           main="Decision Tree Variable Importance",
                           sub=genPlotTitleCmd(vector=TRUE),
                           ...)
{
  model$variable.importance %>%
    data.frame() %>%
    magrittr::set_names("Importance") %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    dplyr::arrange(desc(Importance)) %>%
    dplyr::mutate(Variable=factor(Variable, levels=rev(unique(Variable)))) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=Importance, fill=Variable)) +
    ggplot2::geom_bar(stat="identity", position="identity") +
    ggplot2::ggtitle(label=main) +
    ggplot2::ylab(label=sub) +
    ggplot2::xlab("") +
    ggplot2::coord_flip() +
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
    dplyr::arrange(desc(IncNodePurity)) %>%
    dplyr::mutate(Variable=factor(Variable, levels=rev(unique(Variable)))) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=IncNodePurity, fill=Variable)) +
    ggplot2::geom_bar(stat="identity", position="identity") +
    ggplot2::ggtitle(label=main) +
    ggplot2::ylab(label=sub) +
    ggplot2::xlab("") +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45, hjust=1),
                   legend.position="none")
}
