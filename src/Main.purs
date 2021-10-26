module Main (main) where

import Prelude

import Data.Array (filter)
import Data.Interpolate (i)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception as Exception
import Foreign.Object as Obj
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler_, handler)
import React.Basic.Hooks as Hooks
import Site (Isolate, isolates, baseURL)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

table :: Array Isolate -> JSX
table isos =
  DOM.tbody_ $ row <$> isos
  where
    row :: Isolate -> JSX
    row iso =
      DOM.tr_
        [ DOM.th { scope: "row"
                 , children: [ DOM.a { href: i baseURL "/isolates/" iso.name ".html"
                                     , children: [ DOM.text iso.name ]
                                     }
                             ]
                 }
        , DOM.td_ [ DOM.text iso.kl ]
        , DOM.td_ [ DOM.text iso.st ]
        , DOM.td_ [ DOM.text iso.ocl ]
        ]

filterByField :: String -> String -> Isolate -> Boolean
filterByField field needle iso =
  fromMaybe false $
    contains (Pattern $ toLower needle) <<< toLower
    <$> (M.lookup field $ M.fromFoldable [ ("name" /\ iso.name)
                                         , ("kl" /\ iso.kl)
                                         , ("st" /\ iso.st)
                                         , ("ocl" /\ iso.ocl)
                                         ]
        )

mkApp :: Hooks.Component (Array Isolate)
mkApp = Hooks.component "App" \isos -> Hooks.do
  {feature, query} /\ setFeature <- Hooks.useState' { feature: "name", query: "" }

  let filtered = filter (filterByField feature query) isos

  pure do
    fragment
      [ DOM.div { className: "input-group mb-3"
                , children: [ DOM.div { className: "form-floating flex-grow-1 me-sm-4 my-1 me-0"
                                      , children: [ DOM.select { className: "form-select"
                                                               , id: "filterField"
                                                               , _aria: Obj.fromFoldable [("label" /\ "Filter criteria")]
                                                               , children: [ DOM.option { onClick: handler_ $ setFeature {feature: "name", query}
                                                                                        , selected: feature == "name"
                                                                                        , children: [ DOM.text "Name" ]
                                                                                        }
                                                                           , DOM.option { onClick: handler_ $ setFeature {feature: "st", query}
                                                                                        , selected: feature == "st"
                                                                                        , children: [ DOM.text "ST" ]
                                                                                        }
                                                                           , DOM.option { onClick: handler_ $ setFeature {feature:"kl", query}
                                                                                        , selected: feature == "kl"
                                                                                        , children: [ DOM.text "KL" ]
                                                                                        }
                                                                           , DOM.option { onClick: handler_ $ setFeature {feature:"ocl", query}
                                                                                        , selected: feature == "ocl"
                                                                                        , children: [ DOM.text "OCL" ]
                                                                                        }
                                                                           , DOM.option { onClick: handler_ $ setFeature {feature:"mt", query}
                                                                                        , selected: feature == "mt"
                                                                                        , children: [ DOM.text "MT" ]
                                                                                        }
                                                                           ]
                                                               }
                                                    , DOM.label { htmlFor: "filterField"
                                                                , children: [ DOM.text "filter field" ]
                                                                }
                                                    ]
                                      }
                            , DOM.div { className: "form-floating flex-grow-1 ms-sm-4 my-1 ms-0"
                                      , children: [ DOM.input { type: "text"
                                                              , className: "form-control"
                                                              , id: "filterInput"
                                                              , placeholder: "filter term"
                                                              , onChange: handler targetValue (\value -> setFeature {feature, query: fromMaybe "" value})
                                                              , value: query
                                                              }
                                                  , DOM.label { htmlFor: "filterInput"
                                                              , children: [ DOM.text "filter term" ]
                                                              }
                                                  ]
                                      }
                            ]
                  }
      , DOM.table { className: "table table-striped table-hover"
                  , children: [ DOM.thead_
                                [ DOM.tr_
                                  [ DOM.th { scope: "col", children: [DOM.text "Strain"] }
                                  , DOM.th { scope: "col", children: [DOM.text "Capsule locus type"] }
                                  , DOM.th { scope: "col", children: [DOM.text "Sequence type"] }
                                  , DOM.th { scope: "col", children: [DOM.text "OCL"] }
                                  ]
                                ]
                              , table filtered
                              ]
                  }
      ]
  

main :: Effect Unit
main = do
  log "🍝"
  container <- (querySelector $ QuerySelector "main") =<< (toParentNode <$> (window >>= document))

  app <- mkApp
  
  case container of
    Nothing -> Exception.throw "Cannot find container element: main"
    Just c -> do
        DOM.hydrate (app isolates) c
