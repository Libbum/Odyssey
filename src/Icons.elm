module Icons exposing (checkCircle, chevronDown, chevronLeft, chevronRight, chevronUp, circle, github, info, mail, menu, telegram, x)

import Html exposing (Html)
import TypedSvg exposing (line, path, polyline, svg)
import TypedSvg.Attributes exposing (class, cx, cy, d, points, r, viewBox, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (px)


svgIcon : String -> List (Svg msg) -> Html msg
svgIcon className =
    svg
        [ class [ "icon ", className ]
        , viewBox 0 0 24 24
        ]


checkCircle : Html msg
checkCircle =
    svgIcon "check-circle"
        [ path [ d "M22 11.08V12a10 10 0 1 1-5.93-9.14" ] []
        , polyline [ points [ ( 22, 4 ), ( 12, 14.01 ), ( 9, 11.01 ) ] ] []
        ]


chevronDown : Html msg
chevronDown =
    svgIcon "chevron-down"
        [ polyline [ points [ ( 6, 9 ), ( 12, 15 ), ( 18, 9 ) ] ] []
        ]


chevronUp : Html msg
chevronUp =
    svgIcon "chevron-up"
        [ polyline [ points [ ( 18, 15 ), ( 12, 9 ), ( 6, 15 ) ] ] []
        ]


chevronLeft : Html msg
chevronLeft =
    svgIcon "chevron-left"
        [ polyline [ points [ ( 15, 18 ), ( 9, 12 ), ( 15, 6 ) ] ] []
        ]


chevronRight : Html msg
chevronRight =
    svgIcon "chevron-right"
        [ polyline [ points [ ( 9, 18 ), ( 15, 12 ), ( 9, 6 ) ] ] []
        ]


circle : Html msg
circle =
    svgIcon "circle"
        [ TypedSvg.circle [ cx (px 12), cy (px 12), r (px 10) ] []
        ]


info : Html msg
info =
    svgIcon "info"
        [ TypedSvg.circle [ cx (px 12), cy (px 12), r (px 10) ] []
        , line [ x1 (px 12), y1 (px 16), x2 (px 12), y2 (px 12) ] []
        , line [ x1 (px 12), y1 (px 8), x2 (px 12), y2 (px 8) ] []
        ]


github : Html msg
github =
    svgIcon "github"
        [ path [ d "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22" ] []
        ]


mail : Html msg
mail =
    svgIcon "mail"
        [ path [ d "M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z" ] []
        , polyline [ points [ ( 22, 6 ), ( 12, 13 ), ( 2, 6 ) ] ] []
        ]


menu : Html msg
menu =
    svgIcon "menu"
        [ line [ x1 (px 3), y1 (px 12), x2 (px 21), y2 (px 12) ] []
        , line [ x1 (px 3), y1 (px 6), x2 (px 21), y2 (px 6) ] []
        , line [ x1 (px 3), y1 (px 18), x2 (px 21), y2 (px 18) ] []
        ]


telegram : Html msg
telegram =
    svgIcon "telegram"
        [ path [ d "M23.932 3.769l-3.622 17.08c-.273 1.205-.986 1.505-1.999.937l-5.518-4.066-2.663 2.561c-.294.294-.541.541-1.109.541l.397-5.62L19.646 5.96c.444-.397-.097-.616-.692-.22L6.31 13.702.867 11.998c-1.184-.37-1.205-1.184.247-1.752l21.291-8.203c.985-.369 1.848.22 1.527 1.726z" ] []
        ]


x : Html msg
x =
    svgIcon "x"
        [ line [ x1 (px 18), y1 (px 6), x2 (px 6), y2 (px 18) ] []
        , line [ x1 (px 6), y1 (px 6), x2 (px 18), y2 (px 18) ] []
        ]
