module Demo.TextInput
where

import Prelude

import Bonsai.Forms (FormMsg, form, textInput, withLegend, withMessage)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "text" `withLegend` "Text fields ..." $ do
        textInput "name" "Name" [ A.required true ] `withMessage` "Required"
        textInput "code" "Code" [ A.pattern "[A-Z]*"] `withMessage` "All uppercase."
        textInput "comment" "Comment" [ ]
    H.vnode $ Common.view model
