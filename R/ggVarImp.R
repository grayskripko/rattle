ggVarImp <- function(model, ...) UseMethod("ggVarImp")

ggVarImp.randomForest <- function(model)
{
  importance(model) %>%
    data.frame() %>%
    dplyr::mutate(Variable=row.names(.)) %>%
    tidyr::gather(Measure, Value, -Variable) %>%
    ggplot2::ggplot(ggplot2::aes(x=Variable, y=Value, fill=Variable)) +
    ggplot2::geom_bar(stat="identity") +
    ggplot2::facet_wrap(~ Measure) +
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank())  
}
