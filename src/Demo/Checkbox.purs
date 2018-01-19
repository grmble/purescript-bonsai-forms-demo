module Demo.Checkbox
where

import Prelude

import Bonsai.Forms (FormModel(..), FormMsg, form, checkboxInput, mkTextInput, textInput)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Data.Foldable (traverse_)
import Data.Map (toAscUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

view :: FormModel -> H.VNode FormMsg
view fm@(FormModel model) =
  H.render $ do
    alignedForm "checkbox" fm $
      form Nothing (Just "Checkboxes, oh my!") $ do
        textInput (mkTextInput "name" "Name") { required = true }
        checkboxInput "canSing" "Can Sing"
    H.hr
    H.ul $ do
      traverse_ (\(Tuple k v) ->
          H.li $ do
            H.text k
            H.text ": "
            H.text v)
        (toAscUnfoldable model :: Array (Tuple String String))
