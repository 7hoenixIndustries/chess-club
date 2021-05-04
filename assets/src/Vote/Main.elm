module Vote.Main exposing (view)

{-| This module has Managed Updates. . . Can Program.Core just manage that for you?
-- Some programs Have no updates (because they are using managed components).
-- This file is generated. Please do not edit.
-}

import Html exposing (Html, a, dd, div, dl, dt, h3, li, nav, p, span, text, ul)
import Html.Attributes as Attr
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


type Model
    = Model


view : Html msg
view =
    let
        emptyState =
            Html.text ""
    in
    --emptyState
    {- This example requires Tailwind CSS v2.0+ -}
    div []
        [ card

        --, viewSelection
        ]


card =
    {- This example requires Tailwind CSS v2.0+ -}
    div
        [ Attr.class "bg-white shadow overflow-hidden sm:rounded-lg"
        ]
        [ div
            [ Attr.class "m-6"

            --Attr.class "px-4 py-5 sm:px-6"
            ]
            [ viewSelection
            , viewNavigation
            ]
        ]



--h3
--    [ Attr.class "text-lg leading-6 font-medium text-gray-900"
--    ]
--    [ text "Applicant Information" ]
--, p
--    [ Attr.class "mt-1 max-w-2xl text-sm text-gray-500"
--    ]
--    [ text "Personal details and application." ]
--, div
--    [ Attr.class "border-t border-gray-200 px-4 py-5 sm:p-0"
--    ]
--    [ dl
--        [ Attr.class "sm:divide-y sm:divide-gray-200"
--        ]
--        [ div
--            [ Attr.class "py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"
--            ]
--            [ dt
--                [ Attr.class "text-sm font-medium text-gray-500"
--                ]
--                [ text "Full name" ]
--            , dd
--                [ Attr.class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"
--                ]
--                [ text "Margot Foster" ]
--            ]
--        , div
--            [ Attr.class "py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"
--            ]
--            [ dt
--                [ Attr.class "text-sm font-medium text-gray-500"
--                ]
--                [ text "Application for" ]
--            , dd
--                [ Attr.class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"
--                ]
--                [ text "Backend Developer" ]
--            ]
--        , div
--            [ Attr.class "py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"
--            ]
--            [ dt
--                [ Attr.class "text-sm font-medium text-gray-500"
--                ]
--                [ text "Email address" ]
--            , dd
--                [ Attr.class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"
--                ]
--                [ text "margotfoster@example.com" ]
--            ]
--        , div
--            [ Attr.class "py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"
--            ]
--            [ dt
--                [ Attr.class "text-sm font-medium text-gray-500"
--                ]
--                [ text "Salary expectation" ]
--            , dd
--                [ Attr.class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"
--                ]
--                [ text "$120,000" ]
--            ]
--        , div
--            [ Attr.class "py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"
--            ]
--            [ dt
--                [ Attr.class "text-sm font-medium text-gray-500"
--                ]
--                [ text "About" ]
--            , dd
--                [ Attr.class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"
--                ]
--                [ text "Fugiat ipsum ipsum deserunt culpa aute sint do nostrud anim incididunt cillum culpa consequat. Excepteur qui ipsum aliquip consequat sint. Sit id mollit nulla mollit nostrud in ea officia proident. Irure nostrud pariatur mollit ad adipisicing reprehenderit deserunt qui eu." ]
--            ]
--        , div
--            [ Attr.class "py-4 sm:py-5 sm:grid sm:grid-cols-3 sm:gap-4 sm:px-6"
--            ]
--            [ dt
--                [ Attr.class "text-sm font-medium text-gray-500"
--                ]
--                [ text "Attachments" ]
--            , dd
--                [ Attr.class "mt-1 text-sm text-gray-900 sm:mt-0 sm:col-span-2"
--                ]
--                [ ul
--                    [ Attr.class "border border-gray-200 rounded-md divide-y divide-gray-200"
--                    ]
--                    [ li
--                        [ Attr.class "pl-3 pr-4 py-3 flex items-center justify-between text-sm"
--                        ]
--                        [ div
--                            [ Attr.class "w-0 flex-1 flex items-center"
--                            ]
--                            [ {- Heroicon name: solid/paper-clip -}
--                              svg
--                                [ SvgAttr.class "flex-shrink-0 h-5 w-5 text-gray-400"
--                                , SvgAttr.viewBox "0 0 20 20"
--                                , SvgAttr.fill "currentColor"
--                                , Attr.attribute "aria-hidden" "true"
--                                ]
--                                [ path
--                                    [ SvgAttr.fillRule "evenodd"
--                                    , SvgAttr.d "M8 4a3 3 0 00-3 3v4a5 5 0 0010 0V7a1 1 0 112 0v4a7 7 0 11-14 0V7a5 5 0 0110 0v4a3 3 0 11-6 0V7a1 1 0 012 0v4a1 1 0 102 0V7a3 3 0 00-3-3z"
--                                    , SvgAttr.clipRule "evenodd"
--                                    ]
--                                    []
--                                ]
--                            , span
--                                [ Attr.class "ml-2 flex-1 w-0 truncate"
--                                ]
--                                [ text "resume_back_end_developer.pdf" ]
--                            ]
--                        , div
--                            [ Attr.class "ml-4 flex-shrink-0"
--                            ]
--                            [ a
--                                [ Attr.href "#"
--                                , Attr.class "font-medium text-indigo-600 hover:text-indigo-500"
--                                ]
--                                [ text "Download" ]
--                            ]
--                        ]
--                    , li
--                        [ Attr.class "pl-3 pr-4 py-3 flex items-center justify-between text-sm"
--                        ]
--                        [ div
--                            [ Attr.class "w-0 flex-1 flex items-center"
--                            ]
--                            [ {- Heroicon name: solid/paper-clip -}
--                              svg
--                                [ SvgAttr.class "flex-shrink-0 h-5 w-5 text-gray-400"
--                                , SvgAttr.viewBox "0 0 20 20"
--                                , SvgAttr.fill "currentColor"
--                                , Attr.attribute "aria-hidden" "true"
--                                ]
--                                [ path
--                                    [ SvgAttr.fillRule "evenodd"
--                                    , SvgAttr.d "M8 4a3 3 0 00-3 3v4a5 5 0 0010 0V7a1 1 0 112 0v4a7 7 0 11-14 0V7a5 5 0 0110 0v4a3 3 0 11-6 0V7a1 1 0 012 0v4a1 1 0 102 0V7a3 3 0 00-3-3z"
--                                    , SvgAttr.clipRule "evenodd"
--                                    ]
--                                    []
--                                ]
--                            , span
--                                [ Attr.class "ml-2 flex-1 w-0 truncate"
--                                ]
--                                [ text "coverletter_back_end_developer.pdf" ]
--                            ]
--                        , div
--                            [ Attr.class "ml-4 flex-shrink-0"
--                            ]
--                            [ a
--                                [ Attr.href "#"
--                                , Attr.class "font-medium text-indigo-600 hover:text-indigo-500"
--                                ]
--                                [ text "Download" ]
--                            ]
--                        ]
--                    ]
--                ]
--            ]
--        ]
--    ]
--]


viewSelection =
    div [ Attr.class "m-6" ]
        [ div []
            [ h3
                [ Attr.class "text-lg leading-6 font-medium text-gray-900"
                ]
                [ text "Join this Scenario?" ]
            ]
        , div []
            [ dl
                [ --Attr.class "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-3"
                  Attr.class "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-3"
                ]
                [ foo
                ]
            ]
        , div []
            [ dl
                [ --Attr.class "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-3"
                  Attr.class "mt-5 grid grid-cols-1 gap-5 sm:grid-cols-2 lg:grid-cols-3"
                ]
                [ bar
                ]
            ]

        --, div []
        --    [ h3
        --        [ Attr.class "text-lg leading-6 font-medium text-gray-900"
        --        ]
        --        [ text "Join this Scenario?" ]
        --    ]
        ]


foo =
    div
        [ Attr.class "relative bg-white pt-5 px-4 pb-12 sm:pt-6 sm:px-6 shadow rounded-lg overflow-hidden"
        ]
        [ dt []
            [ div
                [ Attr.class "absolute bg-indigo-500 rounded-md p-3"
                ]
                [ {- Heroicon name: outline/users -}
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
                        , SvgAttr.d "M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"
                        ]
                        []
                    ]
                ]
            , p
                [ Attr.class "ml-16 text-sm font-medium text-gray-500 truncate"
                ]
                [ text "Defender" ]
            ]
        , dd
            [ Attr.class "ml-16 pb-6 flex items-baseline sm:pb-7"
            ]
            [ p
                [ Attr.class "text-2xl font-semibold text-gray-900"
                ]
                [ text "16 players" ]
            , p
                [ Attr.class "ml-2 flex items-baseline text-sm font-semibold text-red-600"
                ]
                [ {- Heroicon name: solid/arrow-sm-down -}
                  svg
                    [ SvgAttr.class "w-6 h-6"
                    , SvgAttr.fill "none"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.viewBox "0 0 24 24"
                    ]
                    [ path
                        [ SvgAttr.strokeLinecap "round"
                        , SvgAttr.strokeLinejoin "round"
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.d "M19 14l-7 7m0 0l-7-7m7 7V3"
                        ]
                        []
                    ]

                --svg
                --  [ SvgAttr.class "self-center flex-shrink-0 h-5 w-5 text-red-500"
                --  , SvgAttr.viewBox "0 0 20 20"
                --  , SvgAttr.fill "currentColor"
                --  , Attr.attribute "aria-hidden" "true"
                --  ]
                --  [ path
                --      [ SvgAttr.fillRule "evenodd"
                --      , SvgAttr.d "\n                                    \n                                    <svg class=" w - 6 h - 6 " fill=" none " stroke=" currentColor " viewBox=" 0 0 24 24 " xmlns=" http :// www.w3.org / 2000 / svg "><path stroke-linecap=" round " stroke-linejoin=" round " stroke-width=" 2 " d=" M19 14 l - 7 7 m0 0 l - 7 - 7 m7 7 V3 "></path></svg>\n                                    \n                                    M5.293 9.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 7.414V15a1 1 0 11-2 0V7.414L6.707 9.707a1 1 0 01-1.414 0z"
                --
                --      --, SvgAttr.d "M5.293 9.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 7.414V15a1 1 0 11-2 0V7.414L6.707 9.707a1 1 0 01-1.414 0z"
                --      , SvgAttr.clipRule "evenodd"
                --      ]
                --      []
                --  ]
                , span
                    [ Attr.class "sr-only"
                    ]
                    [ text "Increased by" ]
                , text "35% odds"
                ]
            , div
                [ Attr.class "absolute bottom-0 inset-x-0 bg-gray-50 px-4 py-4 sm:px-6"
                ]
                [ div
                    [ Attr.class "text-sm"
                    ]
                    [ a
                        [ Attr.href "#"
                        , Attr.class "font-medium text-indigo-600 hover:text-indigo-500"
                        ]
                        [ text "View all"
                        , span
                            [ Attr.class "sr-only"
                            ]
                            [ text "Total Subscribers stats" ]
                        ]
                    ]
                ]
            ]
        ]


bar =
    div
        [ Attr.class "relative bg-white pt-5 px-4 pb-12 sm:pt-6 sm:px-6 shadow rounded-lg overflow-hidden"
        ]
        [ dt []
            [ div
                [ Attr.class "absolute bg-indigo-500 rounded-md p-3"
                ]
                [ {- Heroicon name: outline/mail-open -}
                  svg
                    [ SvgAttr.class "h-6 w-6 text-black"
                    , SvgAttr.fill "white"
                    , SvgAttr.viewBox "0 0 24 24"
                    , SvgAttr.stroke "currentColor"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ path
                        [ SvgAttr.strokeLinecap "round"
                        , SvgAttr.strokeLinejoin "round"
                        , SvgAttr.strokeWidth "1.5"
                        , SvgAttr.d "M12 4.354a4 4 0 110 5.292M15 21H3v-1a6 6 0 0112 0v1zm0 0h6v-1a6 6 0 00-9-5.197M13 7a4 4 0 11-8 0 4 4 0 018 0z"
                        ]
                        []
                    ]

                -- HERE
                --svg
                --  [ SvgAttr.class "h-6 w-6 text-white"
                --  , SvgAttr.fill "none"
                --  , SvgAttr.viewBox "0 0 24 24"
                --  , SvgAttr.stroke "currentColor"
                --  , Attr.attribute "aria-hidden" "true"
                --  ]
                --  [ path
                --      [ SvgAttr.strokeLinecap "round"
                --      , SvgAttr.strokeLinejoin "round"
                --      , SvgAttr.strokeWidth "2"
                --      , SvgAttr.d "M3 19v-8.93a2 2 0 01.89-1.664l7-4.666a2 2 0 012.22 0l7 4.666A2 2 0 0121 10.07V19M3 19a2 2 0 002 2h14a2 2 0 002-2M3 19l6.75-4.5M21 19l-6.75-4.5M3 10l6.75 4.5M21 10l-6.75 4.5m0 0l-1.14.76a2 2 0 01-2.22 0l-1.14-.76"
                --      ]
                --      []
                --  ]
                ]
            , p
                [ Attr.class "ml-16 text-sm font-medium text-gray-500 truncate"
                ]
                [ text "Aggressor" ]
            ]
        , dd
            [ Attr.class "ml-16 pb-6 flex items-baseline sm:pb-7"
            ]
            [ p
                [ Attr.class "text-2xl font-semibold text-gray-900"
                ]
                [ text "4 players" ]
            , p
                [ Attr.class "ml-2 flex items-baseline text-sm font-semibold text-green-600"
                ]
                [ {- Heroicon name: solid/arrow-sm-up -}
                  svg
                    [ SvgAttr.class "self-center flex-shrink-0 h-5 w-5 text-green-500"
                    , SvgAttr.viewBox "0 0 20 20"
                    , SvgAttr.fill "currentColor"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ path
                        [ SvgAttr.fillRule "evenodd"
                        , SvgAttr.d "M5.293 9.707a1 1 0 010-1.414l4-4a1 1 0 011.414 0l4 4a1 1 0 01-1.414 1.414L11 7.414V15a1 1 0 11-2 0V7.414L6.707 9.707a1 1 0 01-1.414 0z"
                        , SvgAttr.clipRule "evenodd"
                        ]
                        []
                    ]
                , span
                    [ Attr.class "sr-only"
                    ]
                    [ text "Increased by" ]
                , text "65% odds"
                ]
            , div
                [ Attr.class "absolute bottom-0 inset-x-0 bg-gray-50 px-4 py-4 sm:px-6"
                ]
                [ div
                    [ Attr.class "text-sm"
                    ]
                    [ a
                        [ Attr.href "#"
                        , Attr.class "font-medium text-indigo-600 hover:text-indigo-500"
                        ]
                        [ text "View all"
                        , span
                            [ Attr.class "sr-only"
                            ]
                            [ text "Avg. Open Rate stats" ]
                        ]
                    ]
                ]
            ]
        ]


viewNavigation =
    {- This example requires Tailwind CSS v2.0+ -}
    nav
        [ Attr.class "bg-white px-4 py-3 flex items-center justify-between border-t border-gray-200 sm:px-6"
        , Attr.attribute "aria-label" "Pagination"
        ]
        [ div
            [ Attr.class "hidden sm:block"
            ]
            [ p
                [ Attr.class "text-sm text-gray-700"
                ]
                [ text "Showing"
                , span
                    [ Attr.class "font-medium"
                    ]
                    [ text "1" ]
                , text "to"
                , span
                    [ Attr.class "font-medium"
                    ]
                    [ text "10" ]
                , text "of"
                , span
                    [ Attr.class "font-medium"
                    ]
                    [ text "20" ]
                , text "results"
                ]
            ]
        , div
            [ Attr.class "flex-1 flex justify-between sm:justify-end"
            ]
            [ a
                [ Attr.href "#"
                , Attr.class "relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                ]
                [ text "Previous" ]
            , a
                [ Attr.href "#"
                , Attr.class "ml-3 relative inline-flex items-center px-4 py-2 border border-gray-300 text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50"
                ]
                [ text "Next" ]
            ]
        ]
