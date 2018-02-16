module Demo.Checkbox
where

import Prelude

import Bonsai.Forms (FormMsg, checkboxInput, form, textInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html (Markup)
import Bonsai.Html.Attributes (required)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Demo.Common as Common

view :: Common.Model -> Markup FormMsg
view model = do
  alignedForm Nothing model.formModel $
    form "checkbox" `withLegend` "Checkboxes ..." $ do
      textInput "name" "Name" `withMessage` "Required" ! required true
      checkboxInput "can"
        [ Tuple "sing" "Can Sing"
        , Tuple "dance" "Can Dance"
        , Tuple "program" "Can Program"
        ]
