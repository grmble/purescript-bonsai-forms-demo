module Demo.Checkbox
where

import Prelude

import Bonsai.Forms (FormMsg, Prop(..), checkboxInput, form, textInput, withLegend, withMessage)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "checkbox" `withLegend` "Checkboxes ..." $ do
        textInput "name" "Name" [ Required true ] `withMessage` "Required"
        checkboxInput "can"
          [ Tuple "sing" "Can Sing"
          , Tuple "dance" "Can Dance"
          , Tuple "program" "Can Program"
          ]
    H.vnode $ Common.view model
