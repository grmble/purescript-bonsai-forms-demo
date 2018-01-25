module Demo.TextInput
where

import Prelude

import Bonsai.Forms (FormMsg, Prop(..), form, textInput, withLegend, withMessage)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "text" `withLegend` "Text fields ..." $ do
        textInput "name" "Name" [ Required true ] `withMessage` "Required"
        textInput "code" "Code" [ Pattern "[A-Z]*"] `withMessage` "All uppercase."
        textInput "longInput" "Long input" [ ClassList ["pure-u-1-2"] ]
        textInput "auto" "Autocomplete" [ Autocomplete "on" ]
        textInput "comment" "Comment" [ ]
    H.vnode $ Common.view model
