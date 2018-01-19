module Demo.RequiredText
where

import Prelude

import Bonsai.Forms (FormModel(..), FormMsg, form, textInput)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Foldable (traverse_)
import Data.Map (toAscUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

view :: FormModel -> H.VNode FormMsg
view fm@(FormModel model) =
  H.render $ do
    alignedForm Nothing fm $
      form "required" (Just "A required text field and an optional one") $ do
        textInput "name" "Name" [ A.required true ]
        textInput "comment" "Comment" [ ]
    H.hr
    H.ul $ do
      traverse_ (\(Tuple k v) ->
          H.li $ do
            H.text k
            H.text ": "
            H.text v)
        (toAscUnfoldable model :: Array (Tuple String String))
