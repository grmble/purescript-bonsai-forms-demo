module Main where

import Prelude

import Bonsai (BONSAI, ElementId(ElementId), UpdateResult, debugProgram, mapResult, plainResult, pureCommand, window)
import Bonsai.Forms (FormModel, FormMsg, emptyFormModel, updateForm)
import Bonsai.Html (Property, VNode, a, button, div_, hr, li, nav, onWithOptions, render, text, ul, vnode, (!), (#!))
import Bonsai.Html.Attributes (classList, cls, href, style)
import Bonsai.Html.Events (onClick, preventDefaultStopPropagation)
import Bonsai.VirtualDom as VD
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Tuple (Tuple(..))
import Demo.ManualForm as ManualForm
import Demo.RequiredText as RequiredText

data Demo
  = RequiredTextDemo
  -- the following ones are manually coded for comparison
  | ManualFormDemo


derive instance eqExample :: Eq Demo

type MasterModel =
  { active :: Demo
  , simpleFormModel :: ManualForm.Model
  , requiredTextModel :: FormModel
  }

data MasterMsg
  = SetCurrent Demo
  | EmptyModel
  | ManualFormMsg ManualForm.Msg
  | RequiredTextMsg FormMsg

update :: forall eff. MasterModel -> MasterMsg -> UpdateResult eff MasterModel MasterMsg
update model msg =
  case msg of
    SetCurrent demo ->
      plainResult $ model { active = demo }
    EmptyModel ->
      plainResult emptyModel
    ManualFormMsg simpleMsg ->
      mapResult ( model { simpleFormModel = _ } ) ManualFormMsg
        (ManualForm.update model.simpleFormModel simpleMsg)
    RequiredTextMsg msg ->
      mapResult ( model { requiredTextModel = _ } ) RequiredTextMsg
        (updateForm model.requiredTextModel msg)

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
            RequiredTextDemo ->
              vnode (map RequiredTextMsg $ RequiredText.view model.requiredTextModel)

viewMenu :: Demo -> VNode MasterMsg
viewMenu active =
  render $ do
    div_ ! cls "pure-u-1-12" $ do
      nav ! cls "pure-menu" $
        ul ! cls "pure-menu-list" $ do
          li ! menuItemClasses RequiredTextDemo $
            a ! cls "pure-menu-link" ! href "#"
              ! onClickPreventDefault (SetCurrent RequiredTextDemo)
              $ text "Required Text"
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
  { active: RequiredTextDemo
  , simpleFormModel: ManualForm.emptyModel
  , requiredTextModel : emptyFormModel
  }

main :: Eff (bonsai::BONSAI, exception::EXCEPTION) Unit
main =
  ( window >>=
    debugProgram (ElementId "main") update view emptyModel true true) *>
  pure unit
