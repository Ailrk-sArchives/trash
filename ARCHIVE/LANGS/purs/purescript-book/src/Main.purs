module Main where

import Prelude

import Data.AddressBook (Person, examplePerson)
import Data.Maybe (Maybe(..))
import Data.ValidationApplicative (Errors)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic (ReactComponent, element)
import React.Basic.DOM as D
import React.Basic.Hooks (reactComponent)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

newtype AppState =
  AppState
  { person :: Person
  , errors :: Errors
  }

initState :: AppState
initState = AppState
  { person: examplePerson
  , errors: []
  }

main :: Effect Unit
main = do
  log "Rendering addres book component"
  doc <- document =<< window
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
       Nothing -> throw "Container element not found"
       Just c -> do
          addressBookApp <- mkAddressBookApp
          let
              app = element addressBookApp {}
          D.render app c

mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  reactComponent
    "AddressBookApp"
    (\props -> pure $ D.text "Hi I'm an address book")


