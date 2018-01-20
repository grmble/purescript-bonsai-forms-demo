module Main where

import Prelude

import Bonsai (BONSAI, ElementId(ElementId), UpdateResult, debugProgram, mapResult, plainResult, pureCommand, window)
import Bonsai.Forms (FormMsg)
import Bonsai.Html (Property, VNode, a, button, div_, hr, li, nav, onWithOptions, render, text, ul, vnode, (!), (#!))
import Bonsai.Html.Attributes (classList, cls, href, style)
import Bonsai.Html.Events (onClick, preventDefaultStopPropagation)
import Bonsai.VirtualDom as VD
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Tuple (Tuple(..))
import Demo.Common as Common
import Demo.ManualForm as ManualForm
import Demo.TextInput as TextInput
import Demo.Checkbox as Checkbox
import Demo.Radio as Radio

data Demo
  = TextInputDemo
  | CheckboxDemo
  | RadioDemo
  -- the following ones are manually coded for comparison
  | ManualFormDemo


derive instance eqExample :: Eq Demo

type MasterModel =
  { active :: Demo
  , simpleFormModel :: ManualForm.Model
  , textInputModel :: Common.Model
  , checkboxModel :: Common.Model
  , radioModel :: Common.Model
  }

data MasterMsg
  = SetCurrent Demo
  | EmptyModel
  | ManualFormMsg ManualForm.Msg
  | TextInputMsg FormMsg
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
      vnode (VD.lazy viewMenu model.active)
      div_ ! cls "pure-u-11-12" $
        div_ #! style "margin-left" "2em" $
          case model.active of
            ManualFormDemo ->
              vnode (map ManualFormMsg $ ManualForm.view model.simpleFormModel)
            TextInputDemo ->
              vnode (map TextInputMsg $ TextInput.view model.textInputModel)
            CheckboxDemo ->
              vnode (map CheckboxMsg $ Checkbox.view model.checkboxModel)
            RadioDemo ->
              vnode (map RadioMsg $ Radio.view model.radioModel)

viewMenu :: Demo -> VNode MasterMsg
viewMenu active =
  render $ do
    div_ ! cls "pure-u-1-12" $ do
      nav ! cls "pure-menu" $
        ul ! cls "pure-menu-list" $ do
          li ! menuItemClasses TextInputDemo $
            a ! cls "pure-menu-link" ! href "#"
              ! onClickPreventDefault (SetCurrent TextInputDemo)
              $ text "Text Input"
          li ! menuItemClasses CheckboxDemo $
            a ! cls "pure-menu-link" ! href "#"
              ! onClickPreventDefault (SetCurrent CheckboxDemo)
              $ text "Checkbox"
          li ! menuItemClasses RadioDemo $
            a ! cls "pure-menu-link" ! href "#"
              ! onClickPreventDefault (SetCurrent RadioDemo)
              $ text "Radio"
          li ! menuItemClasses ManualFormDemo $
            a ! cls "pure-menu-link" ! href "#"
              ! onClickPreventDefault (SetCurrent ManualFormDemo)
              $ text "Simple Form"
      hr
      button
        ! cls "pure-button"
        ! onClick EmptyModel
        $ text "Empty Models"


  where
    menuItemClasses ex =
      classList
        [ Tuple "pure-menu-item" true
        , Tuple "pure-menu-selected" (ex == active) ]

onClickPreventDefault :: forall msg. msg -> Property msg
onClickPreventDefault msg =
  onWithOptions "click" preventDefaultStopPropagation (const $ pure $ pureCommand msg)

emptyModel :: MasterModel
emptyModel =
  { active: TextInputDemo
  , simpleFormModel: ManualForm.emptyModel
  , textInputModel : Common.emptyModel
  , checkboxModel: Common.emptyModel
  , radioModel: Common.emptyModel
  }

main :: Eff (bonsai::BONSAI, exception::EXCEPTION) Unit
main =
  ( window >>=
    debugProgram (ElementId "main") update view emptyModel true true) *>
  pure unit
