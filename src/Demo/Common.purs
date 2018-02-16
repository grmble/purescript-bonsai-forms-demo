module Demo.Common
where

import Prelude

import Bonsai (Cmd)
import Bonsai.Forms.Model (FormModel, FormMsg(..), emptyFormModel, updateForm)
import Bonsai.Html (Markup, h3, hr, li, p, text, ul)
import Control.Plus (empty)
import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.List (List)
import Data.List.NonEmpty as NEL
import Data.Map (toAscUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Model =
  { button :: Maybe String
  , formModel :: FormModel
  , source :: Maybe String
  }

emptyModel :: Model
emptyModel =
  { button: Nothing
  , source: Nothing
  , formModel: emptyFormModel
  }

update :: forall eff. FormMsg -> Model -> Tuple (Cmd eff FormMsg) Model
update (FormOK) model =
  Tuple empty $ model { button = Just "OK" }
update (FormCancel) model =
  Tuple empty $ model { button = Just "Cancel" }
update msg model =
  bimap
    id
    (\x -> model { formModel = x })
    (updateForm msg model.formModel)


viewModel :: Model -> Markup FormMsg
viewModel model = do
  hr
  h3 $ text "Last Button"
  p $ text (show model.button)

  let fmodel = model.formModel
  h3 $ text "Model"
  ul $ do
    traverse_ (\(Tuple k v) ->
        li $ do
          text k
          text ": "
          text $ NEL.intercalate ", " v)
      (toAscUnfoldable fmodel :: List (Tuple String (NEL.NonEmptyList String)))


viewDemo :: (Model -> Markup FormMsg) -> Model -> Markup FormMsg
viewDemo demoFn demoModel = do
  demoFn demoModel
  viewModel demoModel
