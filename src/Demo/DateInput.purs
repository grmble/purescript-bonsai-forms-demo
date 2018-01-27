module Demo.DateInput
where

import Prelude

import Bonsai.Forms (FormMsg, colorInput, dateInput, datetimeLocalInput, form, telInput, urlInput, withLegend, withMessage, (!))
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Demo.Common as Common

view :: Common.Model -> H.VNode FormMsg
view model =
  H.render $ do
    alignedForm Nothing model.formModel $
      form "date" `withLegend` "Date and others that degrade to text  ..." $ do
        dateInput "required" "Required" `withMessage` "Required" ! A.required true
        datetimeLocalInput "datetime" "Date/Time"
        colorInput "color" "Color"
        telInput "tel" "Tel"
        urlInput "url" "URL"
    H.vnode $ Common.view model
