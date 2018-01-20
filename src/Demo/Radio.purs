module Demo.Radio
where

import Prelude

import Bonsai.Forms (FormMsg, radioInput, form, textInput, withLegend, withMessage)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "radio" `withLegend` "Radio ..." $ do
        textInput "name" "Name" [ A.required true ] `withMessage` "Required"
        radioInput "sex" "m"
          [ Tuple "m" "Male"
          , Tuple "f" "Female"
          , Tuple "x" "Yes please"
          ]
    H.vnode $ Common.view model
