module Demo.RequiredText
where

import Bonsai.Forms
import Bonsai.Forms.PureCss
import Prelude

import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Foldable (traverse_)
import Data.Map (empty, toAscUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

view :: FormModel -> H.VNode FormMsg
view fm@(FormModel model) =
  H.render $ do
    alignedForm fm $ form $ do
      fieldset (Just "required_text") (Just "A required text field and an optional one") $ do
        textInput (mkTextInput "name" "Name") { required = true }
        textInput (mkTextInput "comment" "Comment")
    H.hr
    H.ul $ do
      traverse_ (\(Tuple k v) ->
          H.li $ do
            H.text k
            H.text ": "
            H.text v)
        (toAscUnfoldable model :: Array (Tuple String String))
