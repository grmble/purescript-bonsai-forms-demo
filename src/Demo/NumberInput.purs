module Demo.NumberInput
where

import Prelude

import Bonsai.Forms (FormMsg, form, numberInput, rangeInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm (Just "ShouldNotBeInModel") model.formModel $
      form "number" `withLegend` "Number fields ..." $ do
        numberInput "required" "Required" `withMessage` "Required" ! A.required true
        numberInput "minmax" "Min/max" ! A.min "1" ! A.max "10"
        numberInput "step" "Step" ! A.min "1" ! A.max "10" ! A.step "0.1"
        rangeInput  "range" "Range" ! A.min "1" ! A.max "10" ! A.step "0.1"
    H.vnode $ Common.view model
