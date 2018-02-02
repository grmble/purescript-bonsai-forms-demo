module Demo.Common
where

import Prelude

import Bonsai (Cmd, emptyCommand)
import Bonsai.Forms.Model (FormModel, FormMsg(..), emptyFormModel, updateForm)
import Bonsai.Html as H
import Bonsai.VirtualDom (VNode)
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
  }

emptyModel :: Model
emptyModel =
  { button: Nothing
  , formModel: emptyFormModel
  }

update :: forall eff. FormMsg -> Model -> Tuple (Cmd eff FormMsg) Model
update (FormOK) model =
  Tuple emptyCommand $ model { button = Just "OK" }
update (FormCancel) model =
  Tuple emptyCommand $ model { button = Just "Cancel" }
update msg model =
  bimap
    id
    (\x -> model { formModel = x })
    (updateForm msg model.formModel)

view :: Model -> VNode FormMsg
view model =
  H.render $ do
    H.hr
    H.h3 $ H.text "Last Button"
    H.p $ H.text (show model.button)

    let fmodel = model.formModel
    H.h3 $ H.text "Model"
    H.ul $ do
      traverse_ (\(Tuple k v) ->
          H.li $ do
            H.text k
            H.text ": "
            H.text $ NEL.intercalate ", " v)
        (toAscUnfoldable fmodel :: List (Tuple String (NEL.NonEmptyList String)))
