module Demo.TextInput
where

import Prelude

import Bonsai.Forms (FormMsg, form, textInput, textareaInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.Markup FormMsg
view model =
  alignedForm Nothing model.formModel $
    form "text" `withLegend` "Text fields ..." $ do
      textInput "name" "Name" `withMessage` "Required" ! A.required true
      textInput "code" "Code" `withMessage` "All uppercase." ! A.pattern "[A-Z]*"
      textInput "noauto" "No Autocomplete" ! A.autocomplete false
      textInput "longInput" "Long input" ! A.cls "pure-u-1-2"
      textareaInput "comment" "Comment" ! A.cls "pure-u-1-2"
