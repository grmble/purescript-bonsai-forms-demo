module Demo.Common
where

import Prelude

import Bonsai (UpdateResult, mapResult, plainResult)
import Bonsai.Forms (FormModel(..), FormMsg(..), emptyFormModel, updateForm)
import Bonsai.Html as H
import Bonsai.VirtualDom (VNode)
import Data.Foldable (traverse_)
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

update :: forall eff. Model -> FormMsg -> UpdateResult eff Model FormMsg
update model (FormOK) =
  plainResult $ model { button = Just "OK" }
update model (FormCancel) =
  plainResult $ model { button = Just "Cancel" }
update model msg =
  mapResult
    (\x -> model { formModel = x })
    id
    (updateForm model.formModel msg)

view :: Model -> VNode FormMsg
view model =
  H.render $ do
    H.hr
    H.h3 $ H.text "Last Button"
    H.p $ H.text (show model.button)

    let (FormModel fmodel) = model.formModel
    H.h3 $ H.text "Model"
    H.ul $ do
      traverse_ (\(Tuple k v) ->
          H.li $ do
            H.text k
            H.text ": "
            H.text v)
        (toAscUnfoldable fmodel :: Array (Tuple String String))
