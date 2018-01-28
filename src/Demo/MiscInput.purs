module Demo.MiscInput
where

import Prelude

import Bonsai.Forms (FormModel, colorInput, customControl, dateInput, datetimeLocalInput, form, telInput, urlInput, withLegend, withMessage, (!))
import Bonsai.Forms.Internal (FormDefT)
import Bonsai.Forms.Model (FormMsg, lookupChecked, targetSelectedOptions)
import Bonsai.Forms.PureCss (alignedForm)
import Bonsai.Html as H
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events as E
import Bonsai.Types (Cmd(..), f2cmd)
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
        demoSelect model.formModel

    H.vnode $ Common.view model


-- demo of html select using customControl
demoSelect :: FormModel -> FormDefT
demoSelect model =
  customControl "select" "Select" $
    H.select
      H.! E.on "change" (f2cmd Cmd <<< targetSelectedOptions "date_select") $ do
      -- non-multiple options really need a first option with empty value
      -- the so called "placeholder label option"
      -- H.! A.multiple true
      H.option H.! A.value "" $ H.text "Select one ..."
      H.option H.! A.value "opt1"
        H.! A.selected (lookupChecked "date_select" "opt1" model)
        $ H.text "Option 1"
      H.option H.! A.value "opt2"
        H.! A.selected (lookupChecked "date_select" "opt2" model)
        $ H.text "Option 2"
