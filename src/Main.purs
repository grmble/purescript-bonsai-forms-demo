module Main where

import Prelude

import Bonsai (BONSAI, Cmd, ElementId(ElementId), debugProgram, noDebug, plainResult, window)
import Bonsai.Forms (FormMsg)
import Bonsai.Html (MarkupT, VNode, a, button, div_, hr, li, nav, render, text, ul, vnode, (!), (#!))
import Bonsai.Html.Attributes (classList, cls, href, style)
import Bonsai.Html.Events (onClick, onClickPreventDefault)
import Bonsai.VirtualDom as VD
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Bifunctor (bimap)
import Data.Tuple (Tuple(..))
import Demo.Checkbox as Checkbox
import Demo.Common as Common
import Demo.ManualForm as ManualForm
import Demo.MiscInput as MiscInput
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

update :: forall eff. MasterMsg -> MasterModel -> Tuple (Cmd eff MasterMsg) MasterModel
update (SetCurrent demo) model =
  plainResult $ model { active = demo }
update EmptyModel model =
  plainResult emptyModel
update (ManualFormMsg msg) model =
  bimap (map ManualFormMsg) ( model { simpleFormModel = _ } )
    (ManualForm.update msg model.simpleFormModel)
update (TextInputMsg msg) model =
  bimap (map TextInputMsg) ( model { textInputModel = _ } )
    (Common.update msg model.textInputModel)
update (NumberInputMsg msg) model =
  bimap (map NumberInputMsg) ( model { numberInputModel = _ } )
    (Common.update msg model.numberInputModel)
update (MiscInputMsg msg) model =
  bimap (map MiscInputMsg) ( model { miscInputModel = _ } )
    (Common.update msg model.miscInputModel)
update (CheckboxMsg msg) model =
  bimap (map CheckboxMsg) ( model { checkboxModel = _ } )
    (Common.update msg model.checkboxModel)
update (RadioMsg msg) model =
  bimap (map RadioMsg) ( model { radioModel = _ } )
    (Common.update msg model.radioModel)

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


emptyModel :: MasterModel
emptyModel =
  { active: TextInputDemo
  , simpleFormModel: ManualForm.emptyModel
  , textInputModel: Common.emptyModel
  , numberInputModel: Common.emptyModel
  , miscInputModel: MiscInput.emptyModel
  , checkboxModel: Common.emptyModel
  , radioModel: Radio.emptyModel
  }

main :: Eff (bonsai::BONSAI, exception::EXCEPTION) Unit
main =
  ( window >>=
    debugProgram (ElementId "main") update view emptyModel (noDebug { timing = true })) *>
  pure unit
