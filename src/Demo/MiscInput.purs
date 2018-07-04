module Demo.MiscInput
where

import Prelude

import Bonsai.Forms (FormModel, colorInput, dateInput, datetimeLocalInput, form, simpleSelect, telInput, urlInput, withLegend, withMessage, (!))
import Bonsai.Forms.Internal (FormDef)
import Bonsai.Forms.Model (FormMsg, emptyFormModel, insert)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Demo.Common as Common

emptyModel :: Common.Model
emptyModel =
  { button: Nothing
  , source: Nothing
  , formModel: insert "date_select" "opt1" emptyFormModel
  }

view :: Common.Model -> H.Markup FormMsg
view model =
  alignedForm Nothing model.formModel $
    form "date" `withLegend` "Date and others that degrade to text  ..." $ do
      dateInput "required" "Required" `withMessage` "Required" ! A.required true
      datetimeLocalInput "datetime" "Date/Time"
      colorInput "color" "Color"
      telInput "tel" "Tel"
      urlInput "url" "URL"
      demoSelect model.formModel


-- demo of html select using customControl
demoSelect :: FormModel -> FormDef
demoSelect model =
  simpleSelect "select" "Select" model
    [ Tuple "opt1" "Option 1", Tuple "opt2" "Option 2" ]
