module Demo.TextInput
where

import Prelude

import Bonsai.Forms (FormMsg, form, textInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "text" `withLegend` "Text fields ..." ! A.autocomplete false $ do
        textInput "name" "Name" `withMessage` "Required" ! A.required true
        textInput "code" "Code" `withMessage` "All uppercase." ! A.pattern "[A-Z]*"
        textInput "longInput" "Long input" ! A.cls "pure-u-1-2"
        textInput "auto" "Autocomplete" ! A.autocomplete true
        textInput "comment" "Comment"
    H.vnode $ Common.view model
