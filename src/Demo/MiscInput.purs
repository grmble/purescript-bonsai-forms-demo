module Demo.MiscInput
where

import Prelude

import Bonsai.Forms (FormModel, colorInput, customControl, dateInput, datetimeLocalInput, form, telInput, urlInput, withLegend, withMessage, (!))
import Bonsai.Forms.Internal (FormDefT)
import Bonsai.Forms.Model (FormMsg, emptyFormModel, insert, lookupChecked, targetSelectedOptions)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.Types (Cmd(..))
import Data.Maybe (Maybe(..))
import Demo.Common as Common

emptyModel :: Common.Model
emptyModel =
  { button: Nothing
  , source: Nothing
  , formModel: insert "date_select" "opt1" emptyFormModel
  }

view :: Common.Model -> H.MarkupT FormMsg
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
demoSelect :: FormModel -> FormDefT
demoSelect model =
  customControl "select" "Select" $
    H.select
      -- H.! A.required true
      H.! E.on "change" (map Cmd <<< targetSelectedOptions "date_select") $ do
      -- option with empty value is so called "placeholder label option"
      -- H.option H.! A.value "" $ H.text "Select one ..."
      -- but works best WITHOUT "placeholder label option"
      -- model has to default one value from the options though, like radio
      -- H.! A.multiple true
      H.option H.! A.value "opt1"
        H.! A.selected (lookupChecked "date_select" "opt1" model)
        $ H.text "Option 1"
      H.option H.! A.value "opt2"
        H.! A.selected (lookupChecked "date_select" "opt2" model)
        $ H.text "Option 2"
