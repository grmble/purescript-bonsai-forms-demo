module Demo.DateInput
where

import Prelude

import Bonsai.Forms (FormMsg, form, dateInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "date" `withLegend` "Date fields ..." $ do
        dateInput "required" "Required" `withMessage` "Required" ! A.required true
        dateInput "foo" "Some date"
    H.vnode $ Common.view model
