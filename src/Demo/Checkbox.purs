module Demo.Checkbox
where

import Prelude

import Bonsai.Forms (FormModel(..), FormMsg, form, checkboxInput, textInput)
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
      form "checkbox" (Just "Checkboxes, oh my!") $ do
        textInput "name" "Name" [ A.required true ]
        checkboxInput "canSing" "Can Sing" [ ]
    H.hr
    H.ul $ do
      traverse_ (\(Tuple k v) ->
          H.li $ do
            H.text k
            H.text ": "
            H.text v)
        (toAscUnfoldable model :: Array (Tuple String String))
