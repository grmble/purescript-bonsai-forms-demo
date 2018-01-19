module Demo.Checkbox
where

import Prelude

import Bonsai.Forms (FormMsg, checkboxInput, form, textInput, withLegend, withMessage)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "checkbox" `withLegend` "Checkboxes ..." $ do
        textInput "name" "Name" [ A.required true ] `withMessage` "Required"
        checkboxInput "canSing" "Can Sing" [ ]
    H.vnode $ Common.view model
