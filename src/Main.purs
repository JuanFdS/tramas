module Main where

import Prelude


import React.Basic (JSX, createComponent, makeStateless)
import React.Basic.DOM as R
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

app :: JSX
app = unit # makeStateless (createComponent "app") \_ -> R.text "Hello world!"

main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Root element not found."
    Just r  -> render app r