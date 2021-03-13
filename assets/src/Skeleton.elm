module Skeleton exposing
    ( Details
    , Warning(..)
    , view
    )

import Backend exposing (Backend)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Lazy exposing (..)
import Prelude exposing (Segment(..))
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr



-- NODE


type alias Details msg =
    { title : String
    , header : List Segment
    , warning : Warning
    , attrs : List (Html.Attribute msg)
    , children : List (Html msg)
    }


type Warning
    = NoProblems



-- VIEW


view : Backend -> (a -> msg) -> Details a -> Browser.Document msg
view backend toMsg details =
    { title =
        details.title
    , body =
        [ viewAll backend toMsg details
        ]
    }


viewAll : Backend -> (a -> msg) -> Details a -> Html msg
viewAll backend toMsg details =
    div [ class "flex flex-col w-screen min-h-screen" ]
        [ viewShell <| viewBody toMsg details

        --viewHeader <| [ Link (backend.endpoint ++ "/app") "7I" ] ++ details.header
        , viewFooter
        ]


viewBody : (a -> msg) -> Details a -> Html msg
viewBody toMsg details =
    div [ class "container mx-auto flex-1 flex flex-col mt-10 section font-mono" ]
        [ lazy viewWarning details.warning
        , Html.map toMsg <|
            div details.attrs details.children
        ]



-- VIEW HEADER


viewHeader : List Segment -> Html msg
viewHeader segments =
    div [ class "header container mx-auto w-full h-10 bg-indigo-400 " ]
        [ div [ class "nav" ]
            [ case segments of
                [] ->
                    text ""

                _ ->
                    h1 [ class "text-black text-xl p-2" ] (List.intersperse slash (List.map viewSegment segments))
            ]
        ]


slash : Html msg
slash =
    span [ class "spacey-char" ] [ text "/" ]


viewSegment : Segment -> Html msg
viewSegment segment =
    case segment of
        Text string ->
            text string

        Link address string ->
            a [ href address ] [ text string ]



-- VIEW WARNING


viewWarning : Warning -> Html msg
viewWarning warning =
    div [ class "header-underbar" ] <|
        case warning of
            NoProblems ->
                []



-- VIEW FOOTER


viewFooter : Html msg
viewFooter =
    footer [ class "container mx-auto p-8 bg-white dark:bg-gray-800" ]
        [ div [ class "text-center dark:text-white" ]
            [ a [ class "grey-link", href "https://github.com/7hoenix/chess-club" ] [ text "Check out the code" ]
            , text " - © 2021 7hoenix Industries"
            ]
        ]



-- GENERATD


viewShell content =
    {- This example requires Tailwind CSS v2.0+ -}
    div
        [ Attr.class "h-screen flex overflow-hidden bg-gray-100"
        ]
        [ {- Off-canvas menu for mobile, show/hide based on off-canvas menu state. -}
          div
            [ Attr.class "md:hidden"
            ]
            [ div
                [ Attr.class "fixed inset-0 flex z-40"
                ]
                [ {-
                     Off-canvas menu overlay, show/hide based on off-canvas menu state.

                     Entering: "transition-opacity ease-linear duration-300"
                       From: "opacity-0"
                       To: "opacity-100"
                     Leaving: "transition-opacity ease-linear duration-300"
                       From: "opacity-100"
                       To: "opacity-0"
                  -}
                  div
                    [ Attr.class "fixed inset-0"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ div
                        [ Attr.class "absolute inset-0 bg-gray-600 opacity-75"
                        ]
                        []
                    ]
                , {-
                     Off-canvas menu, show/hide based on off-canvas menu state.

                     Entering: "transition ease-in-out duration-300 transform"
                       From: "-translate-x-full"
                       To: "translate-x-0"
                     Leaving: "transition ease-in-out duration-300 transform"
                       From: "translate-x-0"
                       To: "-translate-x-full"
                  -}
                  div
                    [ Attr.class "relative flex-1 flex flex-col max-w-xs w-full bg-indigo-700"
                    ]
                    [ div
                        [ Attr.class "absolute top-0 right-0 -mr-12 pt-2"
                        ]
                        [ button
                            [ Attr.class "ml-1 flex items-center justify-center h-10 w-10 rounded-full focus:outline-none focus:ring-2 focus:ring-inset focus:ring-white"
                            ]
                            [ span
                                [ Attr.class "sr-only"
                                ]
                                [ text "Close sidebar" ]
                            , {- Heroicon name: outline/x -}
                              svg
                                [ SvgAttr.class "h-6 w-6 text-white"
                                , SvgAttr.fill "none"
                                , SvgAttr.viewBox "0 0 24 24"
                                , SvgAttr.stroke "currentColor"
                                , Attr.attribute "aria-hidden" "true"
                                ]
                                [ path
                                    [ SvgAttr.strokeLinecap "round"
                                    , SvgAttr.strokeLinejoin "round"
                                    , SvgAttr.strokeWidth "2"
                                    , SvgAttr.d "M6 18L18 6M6 6l12 12"
                                    ]
                                    []
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "flex-1 h-0 pt-5 pb-4 overflow-y-auto"
                        ]
                        [ div
                            [ Attr.class "flex-shrink-0 flex items-center px-4"
                            ]
                            [ img
                                [ Attr.class "h-8 w-auto"
                                , Attr.src "images/logo-basic-matching.svg"

                                --, Attr.alt "Workflow"
                                ]
                                []
                            ]
                        , nav
                            [ Attr.class "mt-5 px-2 space-y-1"
                            ]
                            [ {- Current: "bg-indigo-800 text-white", Default: "text-white hover:bg-indigo-600 hover:bg-opacity-75" -}
                              a
                                [ Attr.href "#"
                                , Attr.class "bg-indigo-800 text-white group flex items-center px-2 py-2 text-base font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/home -}
                                  svg
                                    [ SvgAttr.class "mr-4 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
                                        ]
                                        []
                                    ]
                                , text "Dashboard"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-base font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/users -}
                                  svg
                                    [ SvgAttr.class "mr-4 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"
                                        ]
                                        []
                                    ]
                                , text "Team"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-base font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/folder -}
                                  svg
                                    [ SvgAttr.class "mr-4 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"
                                        ]
                                        []
                                    ]
                                , text "Projects"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-base font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/calendar -}
                                  svg
                                    [ SvgAttr.class "mr-4 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"
                                        ]
                                        []
                                    ]
                                , text "Calendar"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-base font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/inbox -}
                                  svg
                                    [ SvgAttr.class "mr-4 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M20 13V6a2 2 0 00-2-2H6a2 2 0 00-2 2v7m16 0v5a2 2 0 01-2 2H6a2 2 0 01-2-2v-5m16 0h-2.586a1 1 0 00-.707.293l-2.414 2.414a1 1 0 01-.707.293h-3.172a1 1 0 01-.707-.293l-2.414-2.414A1 1 0 006.586 13H4"
                                        ]
                                        []
                                    ]
                                , text "Documents"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-base font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/chart-bar -}
                                  svg
                                    [ SvgAttr.class "mr-4 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
                                        ]
                                        []
                                    ]
                                , text "Reports"
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "flex-shrink-0 flex border-t border-indigo-800 p-4"
                        ]
                        [ a
                            [ Attr.href "#"
                            , Attr.class "flex-shrink-0 group block"
                            ]
                            [ div
                                [ Attr.class "flex items-center"
                                ]
                                [ div []
                                    [ img
                                        [ Attr.class "inline-block h-10 w-10 rounded-full"
                                        , Attr.alt ""
                                        ]
                                        []
                                    ]
                                , div
                                    [ Attr.class "ml-3"
                                    ]
                                    [ p
                                        [ Attr.class "text-base font-medium text-white"
                                        ]
                                        [ text "Random Panda 1" ]
                                    , p
                                        [ Attr.class "text-sm font-medium text-indigo-200 group-hover:text-white"
                                        ]
                                        [ text "View profile" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , div
                    [ Attr.class "flex-shrink-0 w-14"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [{- Force sidebar to shrink to fit close icon -}]
                ]
            ]
        , {- Static sidebar for desktop -}
          div
            [ Attr.class "hidden bg-indigo-700 md:flex md:flex-shrink-0"
            ]
            [ div
                [ Attr.class "flex flex-col w-64"
                ]
                [ {- Sidebar component, swap this element with another sidebar if you like -}
                  div
                    [ Attr.class "flex flex-col h-0 flex-1"
                    ]
                    [ div
                        [ Attr.class "flex-1 flex flex-col pt-5 pb-4 overflow-y-auto"
                        ]
                        [ div
                            [ Attr.class "flex items-center flex-shrink-0 px-4"
                            ]
                            [ img
                                [ Attr.class "h-8 w-auto"
                                , Attr.src "images/logo-basic-matching.svg"

                                --, Attr.alt "Workflow"
                                ]
                                []
                            ]
                        , nav
                            [ Attr.class "mt-5 flex-1 px-2 space-y-1"
                            ]
                            [ {- Current: "bg-indigo-800 text-white", Default: "text-white hover:bg-indigo-600 hover:bg-opacity-75" -}
                              a
                                [ Attr.href "#"
                                , Attr.class "bg-indigo-800 text-white group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/home -}
                                  svg
                                    [ SvgAttr.class "mr-3 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
                                        ]
                                        []
                                    ]
                                , text "Dashboard"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/users -}
                                  svg
                                    [ SvgAttr.class "mr-3 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"
                                        ]
                                        []
                                    ]
                                , text "Team"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/folder -}
                                  svg
                                    [ SvgAttr.class "mr-3 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"
                                        ]
                                        []
                                    ]
                                , text "Projects"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/calendar -}
                                  svg
                                    [ SvgAttr.class "mr-3 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z"
                                        ]
                                        []
                                    ]
                                , text "Calendar"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/inbox -}
                                  svg
                                    [ SvgAttr.class "mr-3 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M20 13V6a2 2 0 00-2-2H6a2 2 0 00-2 2v7m16 0v5a2 2 0 01-2 2H6a2 2 0 01-2-2v-5m16 0h-2.586a1 1 0 00-.707.293l-2.414 2.414a1 1 0 01-.707.293h-3.172a1 1 0 01-.707-.293l-2.414-2.414A1 1 0 006.586 13H4"
                                        ]
                                        []
                                    ]
                                , text "Documents"
                                ]
                            , a
                                [ Attr.href "#"
                                , Attr.class "text-white hover:bg-indigo-600 hover:bg-opacity-75 group flex items-center px-2 py-2 text-sm font-medium rounded-md"
                                ]
                                [ {- Heroicon name: outline/chart-bar -}
                                  svg
                                    [ SvgAttr.class "mr-3 h-6 w-6 text-indigo-300"
                                    , SvgAttr.fill "none"
                                    , SvgAttr.viewBox "0 0 24 24"
                                    , SvgAttr.stroke "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.strokeLinecap "round"
                                        , SvgAttr.strokeLinejoin "round"
                                        , SvgAttr.strokeWidth "2"
                                        , SvgAttr.d "M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
                                        ]
                                        []
                                    ]
                                , text "Reports"
                                ]
                            ]
                        ]
                    , div
                        [ Attr.class "flex-shrink-0 flex border-t border-indigo-800 p-4"
                        ]
                        [ a
                            [ Attr.href "#"
                            , Attr.class "flex-shrink-0 w-full group block"
                            ]
                            [ div
                                [ Attr.class "flex items-center"
                                ]
                                [ div []
                                    [ img
                                        [ Attr.class "inline-block h-9 w-9 rounded-full"

                                        --, Attr.src "https://images.unsplash.com/photo-1472099645785-5658abf4ff4e?ixlib=rb-1.2.1&ixqx=MrTtzH5gCw&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=facearea&facepad=2&w=256&h=256&q=80"
                                        , Attr.alt ""
                                        ]
                                        []
                                    ]
                                , div
                                    [ Attr.class "ml-3"
                                    ]
                                    [ p
                                        [ Attr.class "text-sm font-medium text-white"
                                        ]
                                        [ text "Random Girafe" ]
                                    , p
                                        [ Attr.class "text-xs font-medium text-indigo-200 group-hover:text-white"
                                        ]
                                        [ text "View profile" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ Attr.class "flex flex-col w-0 flex-1 overflow-hidden"
            ]
            [ div
                [ Attr.class "md:hidden pl-1 pt-1 sm:pl-3 sm:pt-3"
                ]
                [ button
                    [ Attr.class "-ml-0.5 -mt-0.5 h-12 w-12 inline-flex items-center justify-center rounded-md text-gray-500 hover:text-gray-900 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                    ]
                    [ span
                        [ Attr.class "sr-only"
                        ]
                        [ text "Open sidebar" ]
                    , {- Heroicon name: outline/menu -}
                      svg
                        [ SvgAttr.class "h-6 w-6"
                        , SvgAttr.fill "none"
                        , SvgAttr.viewBox "0 0 24 24"
                        , SvgAttr.stroke "currentColor"
                        , Attr.attribute "aria-hidden" "true"
                        ]
                        [ path
                            [ SvgAttr.strokeLinecap "round"
                            , SvgAttr.strokeLinejoin "round"
                            , SvgAttr.strokeWidth "2"
                            , SvgAttr.d "M4 6h16M4 12h16M4 18h16"
                            ]
                            []
                        ]
                    ]
                ]
            , main_
                [ Attr.class "flex-1 relative z-0 overflow-y-auto focus:outline-none"
                , Attr.tabindex 0
                ]
                [ div
                    [ Attr.class "py-6"
                    ]
                    [ div
                        [ Attr.class "max-w-7xl mx-auto px-4 sm:px-6 md:px-8"
                        ]
                        [ h1
                            [ Attr.class "text-2xl font-semibold text-gray-900"
                            ]
                            [ text "Dashboard" ]
                        ]
                    , div
                        [ Attr.class "max-w-7xl mx-auto px-4 sm:px-6 md:px-8"
                        ]
                        [ {- Replace with your content -}
                          div
                            [ Attr.class "py-4"
                            ]
                            [ div
                                [--Attr.class "border-4 border-dashed border-gray-200 rounded-lg h-96"
                                ]
                                [ content ]
                            ]

                        --,                         {- /End replace -}
                        ]
                    ]
                ]
            ]
        ]
