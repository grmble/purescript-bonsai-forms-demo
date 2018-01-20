module Demo.ManualForm
where

import Prelude

import Bonsai (UpdateResult, plainResult, pureCommand)
import Bonsai.EventDecoder (targetValuesEvent)
import Bonsai.Html (Property, button, div_, fieldset, form, hr, input, label, legend, onWithOptions, render, span, text, (!))
import Bonsai.Html.Attributes (checked, cls, for, id_, name, pattern, placeholder, required, typ, value)
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events (preventDefaultStopPropagation)
import Bonsai.Types (f2cmd)
import Bonsai.VirtualDom (VNode)
import Control.Alt ((<|>))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.List.NonEmpty as NEL
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Debug.Trace (traceAny)

newtype Data =
  Data
    { name :: String
    , age :: Int
    , canDance :: Boolean
    , canSing :: Boolean
    , canProgram :: Boolean
    }
derive instance genericData :: Generic Data _
instance showData :: Show Data where show = genericShow
instance genericEncode :: Encode Data where encode = genericEncode codecOpts
instance genericDecode :: Decode Data where decode = genericDecode codecOpts

codecOpts :: Options
codecOpts = defaultOptions { unwrapSingleConstructors = true }

type Model = Maybe Data

data Msg
  = OK Model
  | Cancel

emptyModel :: Model
emptyModel =
  Nothing

update :: forall eff. Model -> Msg -> UpdateResult eff Model Msg
update model msg =
  plainResult
    case msg of
      Cancel ->
        emptyModel
      OK model2 ->
        model2

{--
submitDecoder :: EventDecoder Msg
submitDecoder = ?blubb

xxx :: forall eff. CmdDecoder eff Msg
xxx = f2cmd

asdf = onWithOptions "blubb"
--}

extractOK :: Map String (NEL.NonEmptyList String) -> Msg
extractOK m = traceAny (show m) \_ ->
  OK $ mkData
    <$> map NEL.head (lookup "name" m)
    <*> (map NEL.head (lookup "age" m) >>= fromString)
    <*> (map (NEL.any (_ == "dance")) (lookup "can" m) <|> Just false)
    <*> (map (NEL.any (_ == "sing")) (lookup "can" m) <|> Just false)
    <*> (map (NEL.any (_ == "program")) (lookup "can" m) <|> Just false)
  where
    mkData name age canDance canSing canProgram =
      Data { name, age, canDance, canSing, canProgram }

onSubmit :: forall msg. (Map String (NEL.NonEmptyList String) -> msg) -> Property msg
onSubmit cmdFn =
   onWithOptions "submit" preventDefaultStopPropagation
    (f2cmd (pureCommand <<< cmdFn) <<< targetValuesEvent)

view :: Model -> VNode Msg
view model =
  render $
    div_ $ do
      form ! cls "pure-form pure-form-aligned"
        ! onSubmit extractOK $ do
        fieldset $ do
          legend $ text "Simple Form, manual"
          div_ ! cls "pure-control-group" $ do
            label ! for "name" $ text "Name"
            input ! id_ "name"
              ! name "name"
              ! typ "text"
              ! required true
              ! pattern "[a-zA-Z]*"
              -- ! title "Blubb!"
              ! placeholder "Name"
            span ! cls "pure-form-message-inline" $ text $ "This is a required field"
          div_ ! cls "pure-control-group" $ do
            label ! for "age" $ text "Age"
            input
              ! id_ "age"
              ! name "age"
              ! typ "number"
              ! required true
              ! A.min "0"
              ! A.max "99"
              ! placeholder "Age"

          div_ ! cls "pure-controls" $ do
            label ! cls "pure-checkbox" $ do
              input ! name "can" ! typ "checkbox" ! value "dance"
              text " Dance"
            label ! cls "pure-checkbox" $ do
              input ! name "can" ! typ "checkbox" ! value "sing"
              text " Sing"
            label ! cls "pure-checkbox" $ do
              input ! name "can" ! typ "checkbox" ! value "program"
              text " Program"

          div_ ! cls "pure-controls" $ do
            label ! cls "pure-radio" $ do
              input ! name "yesno" ! typ "radio" ! value "yes" ! checked true
              text " Yes"
            label ! cls "pure-radio" $ do
              input ! name "yesno" ! typ "radio" ! value "no"
              text " No"

{--
<label for="option-two" class="pure-radio">
        <input id="option-two" type="radio" name="optionsRadios" value="option1" checked>
        Here's a radio button. You can choose this one..
    </label>

    <label for="option-three" class="pure-radio">
        <input id="option-three" type="radio" name="optionsRadios" value="option2">
        ..Or this one!
    </label>
--}

          div_ ! cls "pure-controls" $ do
            button
              ! typ "submit"
              ! cls "pure-button pure-button-primary"
              $ text "Submit"
            button ! typ "reset" ! cls "pure-button" $ text "Reset"
      hr
      div_ $ do
        case model of
          Nothing ->
            text "Nothing"
          Just d ->
            text $ encodeJSON d
