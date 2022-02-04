module Main (main) where

import Prelude
import Data.Array (filter)
import Data.Interpolate (i)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception as Exception
import Foreign.Object as Obj
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks as Hooks
import Site (Isolate, isolates, baseURL)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toParentNode)
import Web.HTML.Window (document)

table :: Array Isolate -> JSX
table isos = DOM.tbody_ $ row <$> isos
  where
  row :: Isolate -> JSX
  row iso =
    DOM.tr_
      [ DOM.th
          { scope: "row"
          , children:
              [ DOM.a
                  { href: i baseURL "/isolates/" iso.name ".html"
                  , children: [ DOM.text iso.name ]
                  }
              ]
          }
      , DOM.td { className: "text-center", children: [ DOM.text iso.stPas ] }
      , DOM.td { className: "text-center", children: [ DOM.text iso.stOx ] }
      , DOM.td { className: "text-center", children: [ DOM.text iso.kl ] }
      , DOM.td { className: "text-center", children: [ DOM.text iso.ocl ] }
      , DOM.td { className: "text-center", children: [ DOM.text iso.mt ] }
      ]

filterByField :: String -> String -> Isolate -> Boolean
filterByField field needle iso =
  fromMaybe false
    $
      contains (Pattern $ toLower needle)
        <<< toLower
        <$>
          ( M.lookup field
              $ M.fromFoldable
                  [ ("name" /\ iso.name)
                  , ("kl" /\ iso.kl)
                  , ("st_pas" /\ iso.stPas)
                  , ("st_ox" /\ iso.stOx)
                  , ("ocl" /\ iso.ocl)
                  , ("mt" /\ iso.mt)
                  ]
          )

mkApp :: Hooks.Component (Array Isolate)
mkApp =
  Hooks.component "App" \isos -> Hooks.do
    { feature, query } /\ setFeature <- Hooks.useState' { feature: "name", query: "" }
    let
      filtered = filter (filterByField feature query) isos
    pure do
      fragment
        [ DOM.div
            { className: "input-group mb-3"
            , children:
                [ DOM.div
                    { className: "form-floating flex-grow-1 me-md-4 my-1 me-0"
                    , children:
                        [ DOM.select
                            { className: "form-select"
                            , id: "filterField"
                            , value: feature
                            , _aria: Obj.fromFoldable [ ("label" /\ "Filter criteria") ]
                            , onChange: handler targetValue (\value -> setFeature { feature: fromMaybe "name" value, query })
                            , children:
                                [ DOM.option
                                    { value: "name"
                                    , children: [ DOM.text "Name" ]
                                    }
                                , DOM.option
                                    { value: "st_pas"
                                    , children: [ DOM.text "Pasteur Sequence type" ]
                                    }
                                , DOM.option
                                    { value: "st_ox"
                                    , children: [ DOM.text "Oxford Sequence type" ]
                                    }
                                , DOM.option
                                    { value: "kl"
                                    , children: [ DOM.text "Capsule locus type" ]
                                    }
                                , DOM.option
                                    { value: "ocl"
                                    , children: [ DOM.text "Outer core lipooligosaccharide type" ]
                                    }
                                , DOM.option
                                    { value: "mt"
                                    , children: [ DOM.text "Macrocolony type" ]
                                    }
                                ]
                            }
                        , DOM.label
                            { htmlFor: "filterField"
                            , children: [ DOM.text "Filter field" ]
                            }
                        ]
                    }
                , DOM.div
                    { className: "form-floating flex-grow-1 ms-md-4 my-1 ms-0"
                    , children:
                        [ DOM.input
                            { type: "text"
                            , className: "form-control"
                            , id: "filterInput"
                            , placeholder: "filter term"
                            , onChange: handler targetValue (\value -> setFeature { feature, query: fromMaybe "" value })
                            , value: query
                            }
                        , DOM.label
                            { htmlFor: "filterInput"
                            , className: "text-muted"
                            , children: [ DOM.text "Filter term" ]
                            }
                        ]
                    }
                ]
            }
        , DOM.div
            { className: "table-responsive"
            , children:
                [ DOM.table
                    { className: "table table-striped table-hover"
                    , children:
                        [ DOM.thead_
                            [ DOM.tr_
                                [ DOM.th { scope: "col", children: [ DOM.text "Strain" ] }
                                , DOM.th { scope: "col", className: "text-center", children: [ DOM.text "Pasteur Sequence type" ] }
                                , DOM.th { scope: "col", className: "text-center", children: [ DOM.text "Oxford Sequence type" ] }
                                , DOM.th { scope: "col", className: "text-center", children: [ DOM.text "Capsule locus type" ] }
                                , DOM.th { scope: "col", className: "text-center", children: [ DOM.text "Outer core lipooligosaccharide type" ] }
                                , DOM.th { scope: "col", className: "text-center", children: [ DOM.text "Macrocolony Type" ] }
                                ]
                            ]
                        , table filtered
                        ]
                    }
                ]
            }
        ]

main :: Effect Unit
main = do
  container <- (querySelector $ QuerySelector "main") =<< (toParentNode <$> (window >>= document))
  app <- mkApp
  case container of
    Nothing -> Exception.throw "Cannot find container element: main"
    Just c -> do
      DOM.hydrate (app isolates) c
