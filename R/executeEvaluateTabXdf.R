executeEvaluateTabXdf <- function()
{

  # NO LONGER REQUREIDE 160903

  
  ds <- crs$xdf.split[[2]]
  pr.cmd <- "rxPredict(crs$rpart, data=)"
  appendLog(Rtxt("Predict."), pr.cmd)
  eval(parse(text=pr.cmd))

  

  




  # Obtain some background information.

  mtypes <- getEvaluateModels() # The chosen model types in the Evaluate tab.

  # Check any pre-conditions.

  # Ensure a dataset exists.

  if (noDatasetLoaded()) return()

  # Ensure we have at least one model to evaluate, otherwise warn the
  # user and do nothing.

  if (is.null(listBuiltModels(crs$APRIORI)))
  {
    warnDialog(Rtxt("No models suitable for evaluation have been built.\n\n",
                    "Please build a suitable model before evaluation."))
    return()
  }

  if (is.null(mtypes))
  {
    warnDialog(Rtxt("No model has been specified.",
                    "\n\nPlease select one or more from the",
                    "list of models available."))
    return()
  }

  # Ensure we recognise the model type.

  if (length(setdiff(mtypes, union(crv$PREDICT, c("kmeans", "hclust")))) > 0)
  {
    errorDialog(sprintf(Rtxt("E121: A model type is not recognised.",
                             "We found the model types to be: %s Known models: %s"),
                        mtypes, crv$PREDICT),
                "\n", crv$support.msg)
    return()
  }

  # Ensure there is a model for each model type that is selected.

  if (sum(sapply(mtypes, function(x) is.null(crs[[x]]))) > 0)
  {
    errorDialog(sprintf(Rtxt("E120: Some model has not been built?",
                             "We found the model types to be: %s",
                             "The models not built: %s",
                             "This is a Rattle bug."),
                        mtypes, sapply(mtypes, function(x) is.null(crs[[x]]))),
                "\n", crv$support.msg)
    return()
  }

  #   Ensure the appropriate package is loaded (in the case, for
  #   example, when loading a project and going straight to Evaluate,
  #   and wanting to run predict.svm on new data).

  if (crv$ADA %in%  mtypes &&
      ! packageIsAvailable("ada", sprintf(Rtxt("evaluate a %s model"),
                                          commonName(crv$ADA))))
    return()
  if (crv$KSVM %in%  mtypes &&
      ! packageIsAvailable("kernlab", sprintf(Rtxt("evaluate a %s model"),
                                              commonName(crv$KSVM))))
    return()
  if (crv$RF %in%  mtypes &&
      ! packageIsAvailable("randomForest", sprintf(Rtxt("evaluate a %s model"),
                                                   commonName(crv$RF))))
    return()
  if (crv$GLM %in%  mtypes && "multinom" %in% class(crs$glm) &&
      ! packageIsAvailable("nnet", sprintf(Rtxt("evaluate a %s model"),
                                           paste(" Multinomial", commonName(crv$GLM)))))
    return()
  if (crv$NNET %in% mtypes &&
      ! packageIsAvailable("nnet", sprintf(Rtxt("evaluate a %s model"),
                                           commonName(crv$NNET))))
    return()
  if (crv$SURVIVAL %in% mtypes &&
      ! packageIsAvailable("survival", sprintf(Rtxt("evaluate a %s model"),
                                               commonName(crv$SURVIVAL))))
    return()

  if(theWidget("evaluate_score_radiobutton")$getActive())
    startLog(Rtxt("Score a dataset."))
  else
    startLog(Rtxt("Evaluate model performance."))

  # Identify the data on which evaluation is to be performed.

  testset0 <- "crs$dataset"
  testname <- crs$dataname

  # 081028 For included we only need the input variables and perhaps
  # the risk variable. But after changing the definition of the
  # arguments to getIncludedVariables, where risk=FALSE by default, I
  # forgot to set it to TRUE here. However, it seems to be working so
  # far, at least for glm! 081029 However, we need the target variable
  # in the list for confusion matrix and risk chart, for
  # example. 100530 But we don't need the target for scoring, and so
  # we should remove it if we are scoring.

  #included <- getIncludedVariables(target=FALSE)
  if (theWidget("evaluate_score_radiobutton")$getActive())
    included <- "c(crs$input)" # 20110102 getIncludedVariables(target=FALSE)
  else
    included <- "c(crs$input, crs$target)" # 20110102 getIncludedVariables()

  if (theWidget("evaluate_training_radiobutton")$getActive())
  {
    # Evaluate on training data

    if (crv$show.warnings && theWidget("data_sample_checkbutton")$getActive())
      infoDialog(Rtxt("You are using the training dataset to evaluate your model.",
                      "This will give you an optimistic estimate",
                      "of the performance of your model.",
                      "\n\nYou may want to choose",
                      "to sample the dataset and evaluate the model on the",
                      "test dataset, or else",
                      "load a separate test dataset from a CSV File or a",
                      "pre-existing R Dataset here."))

    if (theWidget("data_sample_checkbutton")$getActive())
      if (is.null(included))
        testset0 <- "crs$dataset[crs$sample,]"
      else
        testset0 <- sprintf("crs$dataset[crs$sample, %s]", included)
    else
      if (is.null(included))
        testset0 <- "crs$dataset"
      else
        testset0 <- sprintf("crs$dataset[,%s]", included)

    testname <- sprintf("%s [**%s**]", crs$dataname, Rtxt("train"))
  }
  else if (theWidget("evaluate_validation_radiobutton")$getActive())
  {
    # Evaluate on validation data

    if (is.null(included))
      testset0 <- "crs$dataset[crs$validate,]"
    else
      testset0 <- sprintf("crs$dataset[crs$validate, %s]", included)
    testname <- sprintf("%s [%s]", crs$dataname, Rtxt("validate"))
  }
  else if (theWidget("evaluate_testing_radiobutton")$getActive())
  {
    # Evaluate on test data

    if (is.null(included))
      if (newSampling())
        testset0 <- "crs$dataset[crs$test,]"
      else
        testset0 <- "crs$dataset[-crs$sample,]"
    else
      if (newSampling())
        testset0 <- sprintf("crs$dataset[crs$test, %s]", included)
      else
        testset0 <- sprintf("crs$dataset[-crs$sample, %s]", included)
    testname <- sprintf("%s [%s]", crs$dataname, Rtxt("test"))
  }
  else if (theWidget("evaluate_csv_radiobutton")$getActive())
  {
    # Evaluate on CSV or TXT data. We identify which from the
    # seperator specified from the Data tab.

    # We need to allow for the case where the loaded csv data does not
    # have the risk and target variables when we are scoring the data
    # (i.e., not when we are generating confusion charts and other
    # evaluations). For scoring, it is only natural that we do not
    # have the risk and target variables.

    filename <- theWidget("evaluate_filechooserbutton")$getFilename()
    crs$dwd <- ifelse(length(filename), dirname(filename), "")
    crs$mtime <- urlModTime(filename)

    if (is.null(filename)
        || ! file.exists(filename)
        || (! isWindows() && file.info(filename)$isdir))
    {
      errorDialog(Rtxt("You have requested that a CSV file be used",
                       "as your testing dataset, but you have not",
                       "identified which file.\n\nPlease use the Spreadsheet",
                       "button to select the CSV file you wish",
                       "to use as your testset before you Execute."))
      return()
    }

    # Load the testset from file, but only load it if it is not
    # already loaded.

    if (is.null(crs$testset)
        || is.null(crs$testname)
        || (basename(filename) != crs$testname))
    {
      # Fix filename for MS/Windows - otherwise eval/parse strips the \\.

      if (isWindows()) filename <- gsub("\\\\", "/", filename)

      nastring <- ', na.strings=c(".", "NA", "", "?")'
      sep = theWidget("data_separator_entry")$getText()
      hdr = theWidget("data_header_checkbutton")$getActive()
      read.cmd <- sprintf(paste('crs$testset <- read.csv("%s"%s, header=%s,',
                                'sep="%s", encoding="%s", strip.white=TRUE)'),
                          filename, nastring,
                          ifelse(hdr, "TRUE", "FALSE"),
                          sep, crv$csv.encoding)

      appendLog(Rtxt("Read a dataset from file for testing the model."), read.cmd)
      eval(parse(text=read.cmd))

      testname <- basename(filename)
      crs$testname <- testname
    }

    # TODO The following case for included assumes the same column
    # orders. Should really check this to make sure.  For scoring a
    # dataset we do not include the target or the risk in the
    # variables, since they may not be present in the csv file that is
    # being loaded (if that option is active). Thus, in this case it
    # is best to simply use the whole dataset for scoring. But, for
    # the case where there are lots of columns that are ignored in the
    # model building, if they have lots of NAs then the scoring is
    # going to give NAs for RF, etc. (Pointed out by Ed Cox 9 Feb
    # 2008.) In general, not sure how to handle this, except for now
    # say that the schema must be identical in the scoring dataset to
    # the training dataset (including the target, risk, and ignored
    # columns). In fact, if the target etc are the last columns then
    # we can get away with it.

    if (is.null(included)) # || theWidget("score_radiobutton")$getActive())
      testset0 <- "crs$testset"
    else
      testset0 <- sprintf("crs$testset[,%s,drop=FALSE]", included)
  }
  else if (theWidget("evaluate_rdataset_radiobutton")$getActive())
  {
    dataset <- theWidget("evaluate_rdataset_combobox")$
               getActiveText()

    if (is.null(dataset) || nchar(dataset) == 0)
    {
      errorDialog(Rtxt("The R Dataset is active but",
                       "no dataset name has been specified.",
                       "Please identify the name of the R dataset",
                       "on which you would like to evaluate the model.",
                       "This dataset will be one that has been defined",
                       "in the R Console."))
      return()
    }

    testset0 <- 'crs$testset'
    testname <- dataset
    crs$testname <- testname

    assign.cmd <- sprintf("crs$testset <- %s", dataset)
    appendLog(Rtxt("Assign the R dataset to be used as the test set."), assign.cmd)
    eval(parse(text=assign.cmd))
  }

  # Ensure the test dataset has the same levels for each variable of
  # the training dataset. This can arise when we externally split a
  # dataset into a training and testing dataset, and the smaller
  # testing dataset may not have representatives of all of the
  # variables. Be sure to add any new levels to the end, otherwise
  # you'll end up renaming some of the other levels! This won't help a
  # model that uses the variable and does not find the particular
  # level, although it is okay if it is missing levels. TODO this
  # might need to check for the former and error out if it is the
  # case. TODO 081204 I don't need to do this for every factor, just
  # those with different levels.

  if (not.null(crs$testname) && crs$testname != crs$dataname)
    # 130128 Why do we need to add the target variable back in?
    # Especially when adding it in and setting it to NA, na.omit for
    # RF has a particular issue!!!! So don't add target back in if it
    # is missing. TODO The code around here has evolved poorly and
    # could really do with quite a cleanup.
    for (cn in setdiff(colnames(crs$dataset), crs$target))
      if (is.factor(crs$dataset[[cn]]))
      {
        # 100701 If the categoric variable is missing (like it might
        # be for the target variable) then be sure to add it in. TODO
        # 100701 Does the order of the variables matter in scoring? I
        # think it does (since we use indicies to subset), so be sure
        # to insert the column in the right place.

        if (! cn %in% colnames(crs$testset))
        {
          place <- which(cn == colnames(crs$dataset))
          cmd <- sprintf(paste("crs$testset <- cbind(crs$testset[1:%d], ",
                               "%s=rep(NA, nrow(crs$testset))",
                               ifelse(place < ncol(crs$testset),
                                      ", crs$testset[%d:ncol(crs$testset)]",
                                      "%s"),
                               ")", sep=""),
                         place-1, cn,
                         ifelse(place < ncol(crs$testset), place, ""))
          appendLog(sprintf(Rtxt("Add missing column `%s'",
                                 "to the testing dataset"), cn),
                          cmd)
          eval(parse(text=cmd))
        }

        # 090808 Be sure to expose this trick to the log file since
        # the user will otherwise be unable to repeat the scoring for
        # the case where the levels are not the same as the training
        # dataset.

        cmd <- sprintf(paste('levels(crs$testset[["%s"]]) <-',
                             '\n  c(levels(crs$testset[["%s"]]),',
                             '\n    setdiff(levels(crs$dataset[["%s"]]),',
                             '\n               levels(crs$testset[["%s"]])))'),
                       cn, cn, cn, cn)
        appendLog(sprintf(Rtxt("Ensure the levels are the same as the training",
                               "data for variable `%s'."), cn),
                          cmd)
        eval(parse(text=cmd))
      }

  ## The default command for prediction from any model is
  ## predict(model, data). Here we tune the predict command to
  ## particular types of models where they have specific general
  ## requirements. We then modify the default predict command to
  ## generate either a prediction of the response or a probability of
  ## the class, as appropriate to the particular evaluator.
  ##
  ## PREDICT: crs$pr <- predict(crs$model, crs$testset[crs$sample, c(...)])

  ## PROBABILITY: this predicts a matrix, each column a probability
  ## for that class.

  ## We want to obtain the probablity of class 1 (i.e., the second of
  ## a two level class). Start with the default predict.cmd.

  # Now build model specific strings for each model

  testset <- list() # The string representing the test dataset
  predcmd <- list() # Command string for predictions
  respcmd <- list() # Command string for response - class of entities
  probcmd <- list() # Command string for probability

  # Why a Predict and Response command? Need better documentation.
  #
  # modeller    pred		resp		prob
  # ada				=pred
  # kmeans			=pred		=pred
  # hclust			=pred		=pred
  # survival			=pred

  if (crv$ADA %in%  mtypes)
  {
    testset[[crv$ADA]] <- testset0

    predcmd[[crv$ADA]] <- genPredictAda(testset[[crv$ADA]])
    respcmd[[crv$ADA]] <- genResponseAda(testset[[crv$ADA]])
    probcmd[[crv$ADA]] <- genProbabilityAda(testset[[crv$ADA]])
  }

  if (crv$KMEANS %in% mtypes)
  {
    testset[[crv$KMEANS]] <- testset0

    # These are all the same!

    predcmd[[crv$KMEANS]] <- genPredictKmeans(testset[[crv$KMEANS]])
    respcmd[[crv$KMEANS]] <- genResponseKmeans(testset[[crv$KMEANS]])
    probcmd[[crv$KMEANS]] <- genProbabilityKmeans(testset[[crv$KMEANS]])
  }

  if (crv$HCLUST %in% mtypes)
  {
    testset[[crv$HCLUST]] <- testset0

    # These are all the same!

    predcmd[[crv$HCLUST]] <- genPredictHclust(testset[[crv$HCLUST]])
    respcmd[[crv$HCLUST]] <- genResponseHclust(testset[[crv$HCLUST]])
    probcmd[[crv$HCLUST]] <- genProbabilityHclust(testset[[crv$HCLUST]])
  }

  if (crv$SURVIVAL %in%  mtypes)
  {
    testset[[crv$SURVIVAL]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[crv$SURVIVAL]] <- genPredictSurvival(testset[[crv$SURVIVAL]])
    respcmd[[crv$SURVIVAL]] <- genResponseSurvival(testset[[crv$SURVIVAL]])
    probcmd[[crv$SURVIVAL]] <- genProbabilitySurvival(testset[[crv$SURVIVAL]])
  }

  if (crv$NNET %in%  mtypes)
  {
    testset[[crv$NNET]] <- testset0

    # 090316 For a binomial target the output node is built with
    # linout=TRUE, thus the value is trying to be close to 1 or 0. Use
    # 0.5 threshold to predict it as 1 or 0. 090820 Move to doing a
    # logistic for nnet, so that we don't use linout in the model
    # building.

    predcmd[[crv$NNET]] <- sprintf("crs$pr <- predict(crs$nnet, newdata=%s)",
                                   testset[[crv$NNET]])

    if (binomialTarget())
      respcmd[[crv$NNET]] <- sub(")$", ', type="class")', predcmd[[crv$NNET]])

#090820 REMOVE the old commented code from here once nnet is stable.
#      predcmd[[crv$NNET]] <- sprintf("crs$pr <- as.integer(predict(crs$nnet, %s)>=0.5)",
#                                     testset[[crv$NNET]])

    else
      respcmd[[crv$NNET]] <- predcmd[[crv$NNET]]

    if (binomialTarget())
#      # 090809 Change from using the pred command to using the raw
#      # predicticed value, which is what it originally probably meant
#      # to be.  090820 TODO Combine these two that both set to
#      # basecmd. In fact, currently nnet is either only binomial or
#      # numeric?
      probcmd[[crv$NNET]] <- predcmd[[crv$NNET]]
#      ## gsub(")$", ', type="raw")', predcmd[[crv$NNET]])
    else if (numericTarget())
      probcmd[[crv$NNET]] <- predcmd[[crv$NNET]]
    else
      probcmd[[crv$NNET]] <- gsub(")$", ', type="prob")', predcmd[[crv$NNET]])
  }

  if (crv$RPART %in%  mtypes)
  {
    cond.tree <- attr(class(crs$rpart), "package") %in% "party"
    if (! length(cond.tree)) cond.tree <- FALSE
    
    testset[[crv$RPART]] <- testset0
    predcmd[[crv$RPART]] <- sprintf("crs$pr <- predict(crs$rpart, newdata=%s)",
                                testset[[crv$RPART]])

    # For crv$RPART, the default is to generate class probabilities for
    # each output class, so ensure we instead generate the response.

    respcmd[[crv$RPART]] <- gsub(")$",
                                 ifelse(cond.tree,
                                        ', type="response")',
                                        ', type="class")'),
                                 predcmd[[crv$RPART]])

    # For RPART the default predict command generates the probabilities
    # for each class and we assume we are interested in the final class
    # (i.e., for binary classification we are interested in the 1's).

    if (cond.tree)
      probcmd[[crv$RPART]] <- sub(')$', '), function(x) x[2])',
                                  sub("predict", "sapply(treeresponse",
                                      predcmd[[crv$RPART]]))
    else
      if (binomialTarget())
        probcmd[[crv$RPART]] <- sprintf("%s[,2]", predcmd[[crv$RPART]])
      else
        probcmd[[crv$RPART]] <- sprintf("%s", predcmd[[crv$RPART]])

    if (multinomialTarget())
    {
      # 081226 Add on the actual class also. This is useful for Score
      # but may be a problem for other types of evaluations (of which
      # there are currently none that use probcmd for multinom).

      probcmd[[crv$RPART]] <- sub("<- ", "<- data.frame(",
                               sub(")$",
                                   sprintf(paste("), rpart=predict(crs$rpart,",
                                                 "newdata=%s, type='class'))"),
                                           testset[[crv$RPART]]),
                                   probcmd[[crv$RPART]]))
    }
  }

  if (crv$RF %in%  mtypes)
  {
    # 090301 Having added support for random forest regression seems
    # like we need to take into acocunt missing for PvO and scoring
    # with numeric targets. In fact, we can probably add na.omit also
    # for categoric targets, since randomForest also does na.omit
    # internally. So it won't help, and will keep in line with other
    # algorithms that actually need the na.omit to be done here.

    cond.rf <- "RandomForest" %in% class(crs$rf) # party conditional rf

    # 090301 testset[[crv$RF]] <- testset0
    if (cond.rf)
      testset[[crv$RF]] <- testset0 # 130323 cforest handles NA in predict
    else
      testset[[crv$RF]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[crv$RF]] <- sprintf("crs$pr <- predict(crs$rf, newdata=%s)",
                             testset[[crv$RF]])

    # The default for crv$RF is to predict the class, so no
    # modification of the predict command is required.

    respcmd[[crv$RF]] <- predcmd[[crv$RF]]

    # For RF we request a probability with the type argument, and as
    # with RPART we extract the column of interest (the last column).

    if (numericTarget())
      probcmd[[crv$RF]] <- predcmd[[crv$RF]]
    else
      if (cond.rf) 
        probcmd[[crv$RF]] <- sub(')$', '), function(x) x[2])',
                                 sub("predict", "sapply(treeresponse",
                                     predcmd[[crv$RF]]))
      else
        probcmd[[crv$RF]] <- sprintf("%s[,2]",
                                     gsub(")$", ', type="prob")', predcmd[[crv$RF]]))

  }

  if (crv$KSVM %in%  mtypes)
  {

    ## For SVM and KSVM, we need to deal with NA's. The predict seems to
    ## need to have NA's removed from the testset, (unlike rpart and rf
    ## where perhaps the NAs don't appear in columns that are used in
    ## the model? An SVM will use all the columns. But in the way we
    ## construct the evaluate command we add extra columns in the third
    ## argument to make sure we get the risk variable in the dataset.
    ## So we need to ensure we get the same subset. It might be smaller
    ## otherwise since the extra columns may have NAs.
    ##
    ## 060527 Comment this out since I was ending up with different
    ## lengths in the 2nd and 3rd arguments in the call to evaluateRisk
    ## in the svm stuff? Using survery-2k the 2nd arg length was 600 and
    ## the 3rd 561, both using na.omit. Perhaps the 3rd should not be
    ## using na.omit, but I haven't investigated this.
    ##
    ## 060603 Put this back in!!! I was again getting the 600 in the
    ## testdata and 561 in the result from predict. Doing a
    ## na.omit(testset) resulted in 561, from the original 600., which
    ## matches the number output from the predict. Try out
    ## survey-training with 10% training to see that that also works! I
    ## suspect that if it does not work, then the issue is missing
    ## levels.
    ## 060622 Seems like the problem is that the na.omit is working on
    ## different subsets of the columns:
    ##   na.omit(crs$dataset[-crs$sample, c(2:22,25)]) versus
    ##   na.omit(crs$dataset[-crs$sample,])
    ## because in the second one we want to retrieve the Risk variable,
    ## which is
    ## not in the first! Instead, let's always extract the list of omitted
    ## rows, and use that here.
    ##
    ##  romit <- attr(na.omit(testset), "na.action")[]
    ## Then use testset[-romit,]
    ## Note that predict automatically removes NAs.
    ## I.e.
    ## crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[-crs$sample,
    ## c(2:22,25)])$Target1,crs$dataset[-crs$sample,][-romit,]$NETADJ_AS_LBLTY)
    ##
    ## 060623 For now in the risk chart function we add the risk
    ## variable back into the testset to ensure it can be accessed,
    ## whereas previously we added in all columns to ensure the risk
    ## variable was included, and this latter resulted in much more
    ## potential for a row to include an NA, and hence to be omitted,
    ## leading to different sized vetors being passed to evaluateRisk. I
    ## hope this now solves the problem and we don't need the top
    ## solution for now.

    testset[[crv$KSVM]] <- sprintf("na.omit(%s)", testset0)

    predcmd[[crv$KSVM]] <- sprintf("crs$pr <- kernlab::predict(crs$ksvm, newdata=%s)",
                               testset[[crv$KSVM]])

    ## The default for KSVM is to predict the class, so no
    ## modification of the predict command is required.

    respcmd[[crv$KSVM]] <- predcmd[[crv$KSVM]]

    ## For KSVM we request a probability with the type argument set to
    ## probability (but need prob.model=TRUE in model building). For SVM
    ## we request probabilities by setting probablity=TRUE and don't
    ## need the second column stuff (and in building the model we needed
    ## probability=TRUE).

    probcmd[[crv$KSVM]] <- sprintf("%s[,2]",
                               gsub(")$",
                                    ', type="probabilities")',
                                    predcmd[[crv$KSVM]]))
    ## For SVM:
    ## probability.cmd <- sprintf("%s",
    ##                             gsub(")$",
    ##                                  ', probability=TRUE)',
    ##                                  probability.cmd))
  }

  if (crv$GLM %in%  mtypes)
  {
    # 080716 The multinom model has been moved to GLM, even though it
    # is using the nnet library. So we need to do the nnet predict
    # here.

    if ("multinom" %in% class(crs$glm))
    {
      testset[[crv$GLM]] <- testset0
      predcmd[[crv$GLM]] <- sprintf("crs$pr <- predict(crs$glm, newdata=%s)",
                                     testset[[crv$GLM]])
      respcmd[[crv$GLM]] <- predcmd[[crv$GLM]]
      probcmd[[crv$GLM]] <- sub(")$", ', type="prob")', predcmd[[crv$GLM]])

      # Add on the actual class also. This is useful for Score but may
      # be a problem for other types of evaluations (of which there
      # are currently none that use probcmd for multinom).

      probcmd[[crv$GLM]] <- sub("<- ", "<- cbind(",
                                sub(")$",
                                    sprintf(paste("), crs$glm$lab[predict(crs$glm,",
                                                  "newdata=%s)])"),
                                            testset[[crv$GLM]]),
                                    probcmd[[crv$GLM]]))

    }
    else
    {

      # GLM's predict removes rows with missing values, so we also need
      # to ensure we remove rows with missing values here.

      # 081029 Try without na.omit since if the target has missing
      # values the record won't be scored, yet there is no reason not
      # to score it. Example is w_reg_logistic.

      ## testset[[crv$GLM]] <- sprintf("na.omit(%s)", testset0)

      testset[[crv$GLM]] <- testset0

      predcmd[[crv$GLM]] <- sprintf(paste("crs$pr <- predict(crs$glm,",
                                          'type="response", newdata=%s)'),
                                    testset[[crv$GLM]])

      # For GLM, a response is a figure close to the class, either close
      # to 1 or close to 0, so threshold it to be either 1 or 0. TODO
      # Simplify this like?
      #    response.cmd <- gsub("predict", "(predict",
      #                         gsub(")$", ")>0.5)*1", response.cmd))

      # 081025 Why do the as.factor? Try just the 0/1 instead. In fact
      # we have now modified this to use the actual levels.

##      respcmd[[crv$GLM]] <- gsub("predict", "as.factor(as.vector(ifelse(predict",
##                                  gsub(")$", ', type="response") > 0.5, 1, 0)))',
##                                       predcmd[[crv$GLM]]))

      # 081029 No longer need response - already there?
      # lvls <- sprintf(', type="response") > 0.5, "%s", "%s"))',
      lvls <- sprintf(') > 0.5, "%s", "%s"))',
                      levels(as.factor(crs$dataset[[crs$target]]))[2],
                      levels(as.factor(crs$dataset[[crs$target]]))[1])
      respcmd[[crv$GLM]] <- gsub("predict", "as.vector(ifelse(predict",
                                 gsub(")$", lvls, predcmd[[crv$GLM]]))

      # For GLM, the response is a probability of the class.

      # 081029 No longer need response - already there?
      # probcmd[[crv$GLM]] <- gsub(")$", ', type="response")', predcmd[[crv$GLM]])
      probcmd[[crv$GLM]] <- predcmd[[crv$GLM]]
    }
  }

##   if (GBM %in%  mtypes)
##   {
##     testset[[GBM]] <- testset0

##     ## For GBM the default needs to know the number of trees to include.

##     predcmd[[GBM]] <- sprintf(paste("crs$pr <- predict(crs$gbm, %s,",
##                                     "n.trees=length(crs$gbm$trees))"),
##                               testset[[GBM]])
##     respcmd[[GBM]] <- predcmd[[GBM]]
##     probcmd[[GBM]] <- predcmd[[GBM]]
##   }

  # Currently (and perhaps permanently) the ROCR package deals only
  # with binary classification, as does my own Risk Chart.

  if (!(theWidget("evaluate_confusion_radiobutton")$getActive()

        # 090506 I had a note here that pvo was not working for
        # multiclass targets for the PrvOb plot, but uncommenting the
        # following and having a numeric target from a categoric
        # variable we get a decent looking plot, but we get a warning
        # about only fitting to first two points and the plotted lines
        # may be relating only to the first two etc. So until I review
        # what happens leave this out. Make sure pvo is not available
        # under such circumstances. Add the check of the Categoric
        # target choice to cover the case of a Numeric override of a
        # Categoric variable.

        || theWidget("evaluate_pvo_radiobutton")$getActive()
        || theWidget("evaluate_score_radiobutton")$getActive())
      && !theWidget("data_target_categoric_radiobutton")$getActive() # See Note Above
      && is.factor(crs$dataset[[crs$target]])
      && length(levels(crs$dataset[[crs$target]])) > 2)
  {
    errorDialog(Rtxt("The number of levels in the target variable is greater",
                     "than 2. Currently, Risk charts and the ROCR package",
                     "(which implements the Lift, ROC, Precision, and Specificity",
                     "charts) apply only to binary classification."))
    return()
  }

  # Dispatch to the appropriate function.

  if (theWidget("evaluate_confusion_radiobutton")$getActive())
    msg <- executeEvaluateConfusion(respcmd, testset, testname)
  else if (theWidget("evaluate_risk_radiobutton")$getActive())
    msg <- executeEvaluateRisk(probcmd, testset, testname)
  else if (theWidget("evaluate_costcurve_radiobutton")$getActive())
    msg <- executeEvaluateCostCurve(probcmd, testset, testname)
  else if (theWidget("evaluate_roc_radiobutton")$getActive())
    msg <- executeEvaluateROC(probcmd, testset, testname)
  else if (theWidget("evaluate_lift_radiobutton")$getActive())
    msg <- executeEvaluateLift(probcmd, testset, testname)
  else if (theWidget("evaluate_precision_radiobutton")$getActive())
    msg <- executeEvaluatePrecision(probcmd, testset, testname)
  else if (theWidget("evaluate_sensitivity_radiobutton")$getActive())
    msg <- executeEvaluateSensitivity(probcmd, testset, testname)
  else if (theWidget("evaluate_hand_radiobutton")$getActive())
    msg <- executeEvaluateHand(probcmd, testset, testname)
  else if (theWidget("evaluate_pvo_radiobutton")$getActive())
  {
    if (categoricTarget())
      msg <- executeEvaluatePvOplot(probcmd, testset, testname)
    else if (numericTarget())
      msg <- executeEvaluatePvOplot(predcmd, testset, testname)
  }

  else if (theWidget("evaluate_score_radiobutton")$getActive())
  {
    if (categoricTarget() || crv$SURVIVAL %in%  mtypes)

      # 081025 Which is best? For trees, traditionally we return the
      # class, but for logistic regression we might return the
      # probability. 081204 So we pass both to the function and decide in
      # there based on a radiobutton setting.

      # 091115 Add the survival option. For a survival model we want
      # access to both the prob and resp (i.e., pred) commands, yet we
      # do not have a categoric target. The prob will be the risk
      # prediction and the resp will be the time to event
      # prediction. Unfortunately, if other models are also selected,
      # this will fail since normally they would go down the other
      # branch here - should be testing that case also I think and put
      # up a warning.

      msg <- executeEvaluateScore(probcmd, respcmd, testset, testname)

    else

      msg <- executeEvaluateScore(predcmd, predcmd, testset, testname)
  }
  else
    msg <- Rtxt("No appropriate evaluator found.")

  if (not.null(msg)) setStatusBar(msg)
}
