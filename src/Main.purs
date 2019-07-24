module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Tramas (ejercicio, Ejercicio)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

divWithClass :: String -> Array JSX -> JSX
divWithClass className children = R.div { className, children }

componenteEjercicio :: Ejercicio -> JSX
componenteEjercicio = make (createComponent "Ejercicio") { initialState, render }

  where render self = divWithClass "ejercicio" [
            divWithClass "enunciado" [R.text self.props.enunciado],
            divWithClass "boton-mostrarSolucion" [bottonDeMostrarSolucion self],
            divWithClass "solucion" [solucion self]
          ]

        initialState = { mostrandoSolucion: false }

        bottonDeMostrarSolucion self = R.button { onClick: toggleSolucion self, children: [R.text textoDelBoton] }
            where textoDelBoton = case self.state.mostrandoSolucion of
                    true -> "Esconder solucion"
                    false -> "Mostrar solucion"

        toggleSolucion self = capture_ $ self.setState \s -> s { mostrandoSolucion = not s.mostrandoSolucion }

        solucion self = R.ul {
          children: (\punto -> R.li_ [punto]) <$> R.text <$> self.props.solucion,
          hidden: not self.state.mostrandoSolucion
        }

main :: Effect Unit
main = do
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  miEjercicio <- ejercicio
  case root of
    Nothing -> throw "Root element not found."
    Just r  -> render (divWithClass "app" [componenteEjercicio miEjercicio]) r