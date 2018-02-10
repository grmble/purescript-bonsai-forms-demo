module Main where

import Prelude

import Bonsai (BONSAI, Cmd, ElementId(ElementId), debugProgram, noDebug, simpleTask, window)
import Bonsai.Core.DOM (locationHashCmd)
import Bonsai.DOM (DOM, document, effF, locationHash)
import Bonsai.Forms (FormMsg)
import Bonsai.Html (Markup, MarkupT, VNode, a, div_, input, label, li, nav, pre_, render, text, ul, vnode, (!), (#!))
import Bonsai.Html.Attributes (checked, cls, defaultValue, href, id_, style, target, typ)
import Bonsai.Html.Events (onCheckedChange, onClickPreventDefault)
import Bonsai.VirtualDom as VD
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Plus (empty)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Tuple (Tuple(..))
import Demo.Checkbox as Checkbox
import Demo.Common as Common
import Demo.MiscInput as MiscInput
import Demo.NumberInput as NumberInput
import Demo.Radio as Radio
import Demo.TextInput as TextInput
import Network.HTTP.Affjax (AJAX, get)

data Demo
  = TextInputDemo
  | NumberInputDemo
  | MiscInputDemo
  | CheckboxDemo
  | RadioDemo


derive instance eqExample :: Eq Demo

type MasterModel =
  { active :: Demo
  , showSource :: Boolean
  , textInputModel :: Common.Model
  , numberInputModel :: Common.Model
  , miscInputModel :: Common.Model
  , checkboxModel :: Common.Model
  , radioModel :: Common.Model
  }


data MasterMsg
  = SetCurrent Demo
  | SetSource Demo String
  | EmptyModel
  | ShowSource Boolean
  | TextInputMsg FormMsg
  | NumberInputMsg FormMsg
  | MiscInputMsg FormMsg
  | CheckboxMsg FormMsg
  | RadioMsg FormMsg

update
  :: forall eff
  .  MasterMsg
  -> MasterModel
  -> Tuple (Cmd (ajax::AJAX,dom::DOM|eff) MasterMsg) MasterModel
update (SetCurrent demo) model =
  let model' = model { active = demo }
  in  Tuple
        (loadCurrentSourceTask model' <> (locationHashCmd $ demoHash demo))
        model'

update (SetSource demo str) model =
  let
    go :: Common.Model -> (Common.Model -> MasterModel) -> MasterModel
    go m setM = setM (m { source = Just str })
  in
    Tuple
      empty
      case demo of
        TextInputDemo ->
          go model.textInputModel (model { textInputModel = _ })
        NumberInputDemo ->
          go model.numberInputModel (model { numberInputModel = _ })
        MiscInputDemo ->
          go model.miscInputModel (model { miscInputModel = _ })
        CheckboxDemo ->
          go model.checkboxModel (model { checkboxModel = _ })
        RadioDemo ->
          go model.radioModel (model { radioModel = _ })

update (ShowSource b) model =
  let model' = model { showSource = b }
      cmd = loadCurrentSourceTask model'
  in  Tuple cmd model'

update EmptyModel model =
  let model' = emptyModel { showSource = model.showSource, active = model.active }
  in  Tuple (loadCurrentSourceTask model') model'

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

loadCurrentSourceTask :: forall eff. MasterModel -> Cmd (ajax::AJAX|eff) MasterMsg
loadCurrentSourceTask model =
  if model.showSource && isNothing formModel.source
    then go formModel
    else empty
  where
    formModel = unwrapModel model
    go m =
      case m.source of
        Just _ ->
          empty
        Nothing ->
          simpleTask $ do
            res <- get (sourceFilename model.active)
            pure $ SetSource model.active res.response

view :: MasterModel -> VNode MasterMsg
view model =
  render $
    div_ ! id_ "bonsai-main" ! cls "pure-g" $
      if model.showSource
        then do
          menuAndContent "pure-u-1 pure-u-lg-1-2"
          div_ ! id_ "source"
            ! cls "pure-u-1 pure-u-lg-1-2" $
            pre_ $ text $ fromMaybe "Loading ..." (unwrapModel model).source
        else menuAndContent "pure-u-1"
  where
    menuAndContent klass = do
      div_ ! cls klass $ do
        vnode (VD.lazy (render <<< viewMenu) model)
        viewContent model



viewMenu :: MasterModel -> MarkupT MasterMsg
viewMenu model = do
    nav ! cls "l-box pure-menu pure-menu-horizontal pure-menu-scrollable" $ do
      a ! cls "pure-menu-heading pure-menu-link"
        ! target "_blank"
        ! href "https://github.com/grmble/purescript-bonsai-forms-demo/"
        $ text "Forms Demo"
      ul ! cls "pure-menu-list" $ do
        item TextInputDemo "Text Input"
        item NumberInputDemo "Number Input"
        item CheckboxDemo "Checkbox"
        item RadioDemo "Radio"
        item MiscInputDemo "Misc Input"
        li ! cls "pure-menu-item pure-menu-link pure-form" $ do
          label $ do
            input
              ! typ "checkbox"
              ! defaultValue "y"
              ! checked model.showSource
              ! onCheckedChange ShowSource
            text " Show Source"
        li ! cls "pure-menu-item" $ do
          a ! cls "pure-menu-link" ! href "#"
            ! onClickPreventDefault EmptyModel
            $ text "Empty Model"

  where
    item demo str =
      li ! menuItemClasses demo $
        a ! cls "pure-menu-link" ! href "#"
          ! onClickPreventDefault (SetCurrent demo)
          $ text str

    menuItemClasses ex =
      cls
        if ex == model.active
          then "pure-menu-item pure-menu-selected"
          else "pure-menu-item"

viewContent :: MasterModel -> Markup MasterMsg Unit
viewContent model =
  div_ ! cls "l-box" #! style "margin-left" "2em" $
    case model.active of
      TextInputDemo ->
        mapMarkup TextInputMsg $ Common.viewDemo TextInput.view model.textInputModel
      NumberInputDemo ->
        mapMarkup NumberInputMsg $ Common.viewDemo NumberInput.view model.numberInputModel
      MiscInputDemo ->
        mapMarkup MiscInputMsg $ Common.viewDemo MiscInput.view model.miscInputModel
      CheckboxDemo ->
        mapMarkup CheckboxMsg $ Common.viewDemo Checkbox.view model.checkboxModel
      RadioDemo ->
        mapMarkup RadioMsg $ Common.viewDemo Radio.view model.radioModel

  where
    mapMarkup fn =
      vnode <<< map fn <<< render

emptyModel :: MasterModel
emptyModel =
  { active: TextInputDemo
  , showSource: false
  , textInputModel: Common.emptyModel
  , numberInputModel: Common.emptyModel
  , miscInputModel: MiscInput.emptyModel
  , checkboxModel: Common.emptyModel
  , radioModel: Radio.emptyModel
  }

sourceFilename :: Demo -> String
sourceFilename demo =
  "src/Demo/" <> n <> ".purs"
  where
    n = case demo of
      TextInputDemo -> "TextInput"
      NumberInputDemo -> "NumberInput"
      CheckboxDemo -> "Checkbox"
      RadioDemo -> "Radio"
      MiscInputDemo -> "MiscInput"

demoHash :: Demo -> String
demoHash demo =
  case demo of
    TextInputDemo -> "#text"
    NumberInputDemo -> "#number"
    MiscInputDemo -> "#misc"
    CheckboxDemo -> "#checkbox"
    RadioDemo -> "#radio"

hashDemo :: String -> Demo
hashDemo hash =
  case hash of
    "#text" -> TextInputDemo
    "#number" -> NumberInputDemo
    "#misc" -> MiscInputDemo
    "#checkbox" -> CheckboxDemo
    "#radio" -> RadioDemo

    -- default to textInputDemo
    _ -> TextInputDemo

unwrapModel :: MasterModel -> Common.Model
unwrapModel model =
  case model.active of
    TextInputDemo -> model.textInputModel
    NumberInputDemo -> model.numberInputModel
    MiscInputDemo -> model.miscInputModel
    CheckboxDemo -> model.checkboxModel
    RadioDemo -> model.radioModel

wrapModel :: MasterModel -> Common.Model -> MasterModel
wrapModel model demoModel =
  case model.active of
    TextInputDemo -> model { textInputModel = demoModel }
    NumberInputDemo -> model { numberInputModel = demoModel }
    MiscInputDemo -> model { miscInputModel = demoModel }
    CheckboxDemo -> model { checkboxModel = demoModel }
    RadioDemo -> model { radioModel = demoModel }


main :: Eff (bonsai::BONSAI, dom::DOM, exception::EXCEPTION) Unit
main = do
  hash <- effF $ window >>= document >>= locationHash
  let demo = hashDemo hash
  let model = emptyModel { active = demo }
  _ <- dbgProgram (ElementId "main") update view model window
  pure unit

  where
    dbgProgram =
      debugProgram (noDebug
        { timing = true
        -- , events = true
        })
