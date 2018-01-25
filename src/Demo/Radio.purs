module Demo.Radio
where

import Prelude

import Bonsai.Forms (FormMsg, Prop(..), emptyFormModel, form, radioInput, set, textInput, withLegend, withMessage)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Demo.Common as Common

emptyModel :: Common.Model
emptyModel =
  { button: Nothing
  , formModel: set "radio_sex" "m" $ emptyFormModel
  }

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "radio" `withLegend` "Radio ..." $ do
        textInput "name" "Name" [ Required true ] `withMessage` "Required"
        radioInput "sex"
          [ Tuple "m" "Male"
          , Tuple "f" "Female"
          , Tuple "x" "Yes please"
          ]
    H.vnode $ Common.view model
