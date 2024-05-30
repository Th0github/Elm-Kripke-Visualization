{-# LANGUAGE OverloadedStrings #-}

module EvaluationRequest where

import Data.Aeson
import Form
import Model

newtype EvaluationRequest = Req (Form, Model)

getFormFromRequest :: EvaluationRequest -> Form
getFormFromRequest (Req (f, _)) = f

getModelFromRequest :: EvaluationRequest -> Model
getModelFromRequest (Req (_, m)) = m

instance FromJSON EvaluationRequest where
  parseJSON =
    withObject
      "Form, Model"
      ( \o -> do
          _form <- o .: "form"
          _model <- o .: "model"
          return (Req (_form, _model))
      )
