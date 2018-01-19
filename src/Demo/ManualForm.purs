module Demo.ManualForm
where

import Prelude

import Bonsai (UpdateResult, plainResult)
import Bonsai.Html (button, div_, fieldset, form, hr, input, label, legend, render, span, text, (!))
import Bonsai.Html.Attributes (cls, for, id_, name, pattern, placeholder, required, typ)
import Bonsai.Html.Attributes as A
import Bonsai.Html.Events (onSubmit)
import Bonsai.VirtualDom (VNode)
import Control.Alt ((<|>))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, encodeJSON, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
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

extractOK :: Map String String -> Msg
extractOK m = traceAny (show m) \_ ->
  OK $ mkData
    <$> lookup "name" m
    <*> (lookup "age" m >>= fromString)
    <*> (map toBoolean (lookup "dance" m) <|> Just false)
    <*> (map toBoolean (lookup "sing" m) <|> Just false)
    <*> (map toBoolean (lookup "program" m) <|> Just false)
  where
    toBoolean s =
      s == "on"
    mkData name age canDance canSing canProgram =
      Data { name, age, canDance, canSing, canProgram }


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
            label ! for "dance" $ do
              input ! id_ "dance" ! name "dance" ! typ "checkbox"
              text "Dance"
          div_ ! cls "pure-controls" $ do
            label ! for "sing" $ do
              input ! id_ "sing" ! name "sing" ! typ "checkbox"
              text "Sing"
          div_ ! cls "pure-controls" $ do
            label ! for "program" $ do
              input ! id_"program" ! name "program" ! typ "checkbox"
              text "Program"

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
