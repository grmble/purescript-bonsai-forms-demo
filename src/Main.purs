module Main where

import Prelude

import Bonsai (BONSAI, ElementId(ElementId), UpdateResult, debugProgram, mapResult, plainResult, pureCommand, window)
import Bonsai.Forms (FormMsg)
import Bonsai.Html (Property, VNode, MarkupT, a, button, div_, hr, li, nav, onWithOptions, render, text, ul, vnode, (!), (#!))
import Bonsai.Html.Attributes (classList, cls, href, style)
import Bonsai.Html.Events (onClick, preventDefaultStopPropagation)
import Bonsai.VirtualDom as VD
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Tuple (Tuple(..))
import Demo.Checkbox as Checkbox
import Demo.Common as Common
import Demo.MiscInput as MiscInput
import Demo.ManualForm as ManualForm
import Demo.NumberInput as NumberInput
import Demo.Radio as Radio
import Demo.TextInput as TextInput

data Demo
  = TextInputDemo
  | NumberInputDemo
  | MiscInputDemo
  | CheckboxDemo
  | RadioDemo
  -- the following ones are manually coded for comparison
  | ManualFormDemo


derive instance eqExample :: Eq Demo

type MasterModel =
  { active :: Demo
  , simpleFormModel :: ManualForm.Model
  , textInputModel :: Common.Model
  , numberInputModel :: Common.Model
  , miscInputModel :: Common.Model
  , checkboxModel :: Common.Model
  , radioModel :: Common.Model
  }

data MasterMsg
  = SetCurrent Demo
  | EmptyModel
  | ManualFormMsg ManualForm.Msg
  | TextInputMsg FormMsg
  | NumberInputMsg FormMsg
  | MiscInputMsg FormMsg
  | CheckboxMsg FormMsg
  | RadioMsg FormMsg

update :: forall eff. MasterModel -> MasterMsg -> UpdateResult eff MasterModel MasterMsg
update model (SetCurrent demo) =
  plainResult $ model { active = demo }
update model EmptyModel =
  plainResult emptyModel
update model (ManualFormMsg msg) =
  mapResult ( model { simpleFormModel = _ } ) ManualFormMsg
    (ManualForm.update model.simpleFormModel msg)
update model (TextInputMsg msg) =
  mapResult ( model { textInputModel = _ } ) TextInputMsg
    (Common.update model.textInputModel msg)
update model (NumberInputMsg msg) =
  mapResult ( model { numberInputModel = _ } ) NumberInputMsg
    (Common.update model.numberInputModel msg)
update model (MiscInputMsg msg) =
  mapResult ( model { miscInputModel = _ } ) MiscInputMsg
    (Common.update model.miscInputModel msg)
update model (CheckboxMsg msg) =
  mapResult ( model { checkboxModel = _ } ) CheckboxMsg
    (Common.update model.checkboxModel msg)
update model (RadioMsg msg) =
  mapResult ( model { radioModel = _ } ) RadioMsg
    (Common.update model.radioModel msg)

view :: MasterModel -> VNode MasterMsg
view model =
  render $
    div_ ! cls "pure-grid" $ do
      vnode (VD.lazy (viewMenu "pure-u-1-6") model.active)
      viewContent "pure-u-5-6" model

viewMenu :: String -> Demo -> VNode MasterMsg
viewMenu gridKlass active =
  render $ do
    div_ ! cls gridKlass $ do
      nav ! cls "pure-menu" $
        ul ! cls "pure-menu-list" $ do
          item TextInputDemo "Text Input"
          item NumberInputDemo "Number Input"
          item CheckboxDemo "Checkbox"
          item RadioDemo "Radio"
          item MiscInputDemo "Misc Input"
          item ManualFormDemo "Manual Form"
      hr
      button
        ! cls "pure-button"
        ! onClick EmptyModel
        $ text "Empty Models"


  where
    item demo str =
      li ! menuItemClasses demo $
        a ! cls "pure-menu-link" ! href "#"
          ! onClickPreventDefault (SetCurrent demo)
          $ text str

    menuItemClasses ex =
      classList
        [ Tuple "pure-menu-item" true
        , Tuple "pure-menu-selected" (ex == active) ]


viewContent :: String -> MasterModel -> MarkupT MasterMsg
viewContent gridKlass model =
  div_ ! cls gridKlass $
    div_ #! style "margin-left" "2em" $
      case model.active of
        ManualFormDemo ->
          vnode (map ManualFormMsg $ ManualForm.view model.simpleFormModel)
        TextInputDemo ->
          vnode (map TextInputMsg $ TextInput.view model.textInputModel)
        NumberInputDemo ->
          vnode (map NumberInputMsg $ NumberInput.view model.numberInputModel)
        MiscInputDemo ->
          vnode (map MiscInputMsg $ MiscInput.view model.miscInputModel)
        CheckboxDemo ->
          vnode (map CheckboxMsg $ Checkbox.view model.checkboxModel)
        RadioDemo ->
          vnode (map RadioMsg $ Radio.view model.radioModel)


onClickPreventDefault :: forall msg. msg -> Property msg
onClickPreventDefault msg =
  onWithOptions "click" preventDefaultStopPropagation (const $ pure $ pureCommand msg)

emptyModel :: MasterModel
emptyModel =
  { active: TextInputDemo
  , simpleFormModel: ManualForm.emptyModel
  , textInputModel: Common.emptyModel
  , numberInputModel: Common.emptyModel
  , miscInputModel: Common.emptyModel
  , checkboxModel: Common.emptyModel
  , radioModel: Radio.emptyModel
  }

main :: Eff (bonsai::BONSAI, exception::EXCEPTION) Unit
main =
  ( window >>=
    debugProgram (ElementId "main") update view emptyModel true true) *>
  pure unit
