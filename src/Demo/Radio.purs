module Demo.Radio
where

import Prelude

import Bonsai.Forms (FormMsg, form, radioInput, textInput, withLegend, withMessage, (!))
import Bonsai.Forms.Model (emptyFormModel, insert)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Demo.Common as Common

emptyModel :: Common.Model
emptyModel =
  { button: Nothing
  , source: Nothing
  , formModel: insert "radio_sex" "m" emptyFormModel
  }

view :: Common.Model -> H.MarkupT FormMsg
view model =
  alignedForm Nothing model.formModel $
    form "radio" `withLegend` "Radio ..." $ do
      textInput "name" "Name" `withMessage` "Required" ! A.required true
      radioInput "sex"
        [ Tuple "m" "Male"
        , Tuple "f" "Female"
        , Tuple "x" "Yes please"
        ]
