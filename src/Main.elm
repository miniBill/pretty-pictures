module Main exposing (main)

import Browser
import Element exposing (Element, column, el, fill, height, padding, spacing, text, width)
import Element.Input as Input
import Html exposing (Html)
import Svg
import Svg.Attributes
import Theme


type alias Flags =
    ()


type alias Model =
    { rxString : String
    , ryString : String
    , widthString : String
    , heightString : String
    , squareSizeString : String
    }


type Msg
    = Rx String
    | Ry String
    | Width String
    | Height String
    | SquareSize String


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = Element.layout [] << view
        , update = update
        , subscriptions = subscriptions
        }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { rxString = "1"
      , ryString = "1"
      , widthString = "3"
      , heightString = "2"
      , squareSizeString = "100"
      }
    , Cmd.none
    )


view : Model -> Element Msg
view model =
    column [ height fill, width fill, spacing Theme.spacing, padding Theme.spacing ]
        [ Input.text [ spacing Theme.spacing ]
            { label = Input.labelLeft [] <| text "Ratio (x)"
            , onChange = Rx
            , placeholder = Nothing
            , text = model.rxString
            }
        , Input.text [ spacing Theme.spacing ]
            { label = Input.labelLeft [] <| text "Ratio (y)"
            , onChange = Ry
            , placeholder = Nothing
            , text = model.ryString
            }
        , Input.text [ spacing Theme.spacing ]
            { label = Input.labelLeft [] <| text "Width"
            , onChange = Width
            , placeholder = Nothing
            , text = model.widthString
            }
        , Input.text [ spacing Theme.spacing ]
            { label = Input.labelLeft [] <| text "Height"
            , onChange = Height
            , placeholder = Nothing
            , text = model.heightString
            }
        , Input.text [ spacing Theme.spacing ]
            { label = Input.labelLeft [] <| text "Square size"
            , onChange = SquareSize
            , placeholder = Nothing
            , text = model.squareSizeString
            }
        , case
            Maybe.map5
                (\rx ry width height squareSize ->
                    { rx = rx
                    , ry = ry
                    , width = width
                    , height = height
                    , squareSize = squareSize
                    }
                )
                (String.toInt model.rxString)
                (String.toInt model.ryString)
                (String.toInt model.widthString)
                (String.toInt model.heightString)
                (String.toInt model.squareSizeString)
          of
            Just parsed ->
                el [] <|
                    Element.html <|
                        ratioToImage parsed

            Nothing ->
                text "Invalid numbers"
        ]


log : String -> a -> a
log =
    --Debug.log
    always identity


ratioToImage : { width : Int, height : Int, rx : Int, ry : Int, squareSize : Int } -> Html msg
ratioToImage ({ width, height, squareSize } as model) =
    let
        ws =
            String.fromInt <| squareSize * width

        hs =
            String.fromInt <| squareSize * height

        gridColor =
            "#E0E0E0"

        grid =
            Svg.g [ Svg.Attributes.stroke gridColor ] <|
                List.foldl
                    (\y ->
                        plotLine 0 (y * squareSize) (width * squareSize) (y * squareSize)
                    )
                    (List.foldl
                        (\x ->
                            plotLine (x * squareSize) 0 (x * squareSize) (height * squareSize)
                        )
                        []
                        (List.range 0 width)
                    )
                    (List.range 0 height)

        go budget rx ry x y img =
            let
                _ =
                    log "go" { budget = budget, x = x, y = y, rx = rx, ry = ry }

                bounce bx by nrx nry =
                    let
                        plotted =
                            plotLine (x * squareSize) (y * squareSize) (bx * squareSize) (by * squareSize) img
                    in
                    if bx == 0 && by == 0 then
                        plotted

                    else
                        go (budget - 1) nrx nry bx by plotted

                -- y = (ry / rx) x + q
                q =
                    y - ry * x // rx

                -- Try bouncing up/down
                ny =
                    if ry < 0 then
                        -- Up
                        0

                    else
                        -- Down
                        height

                -- ny = (ry / rx) nx + q
                -- nx = (ny - q) * rx / ry
                nx =
                    (ny - q) * rx // ry

                -- If cannot bounce up/down, this is the new x
                altx =
                    if rx > 0 then
                        width

                    else
                        0

                -- If cannot bounce up/down, this is the new y
                -- w = m altx + q
                alty =
                    altx * ry // rx + q
            in
            if budget <= 0 then
                img

            else if 0 <= nx && nx <= width then
                bounce nx ny rx -ry

            else
                bounce altx alty -rx ry

        plotLine x0 y0 x1 y1 img =
            Svg.line
                [ Svg.Attributes.x1 <| String.fromInt x0
                , Svg.Attributes.y1 <| String.fromInt y0
                , Svg.Attributes.x2 <| String.fromInt x1
                , Svg.Attributes.y2 <| String.fromInt y1
                ]
                []
                :: img
    in
    Svg.svg
        [ Svg.Attributes.width ws
        , Svg.Attributes.height hs
        , Svg.Attributes.viewBox <| "0 0 " ++ ws ++ " " ++ hs
        , Svg.Attributes.style "vector-effect: non-scaling-stroke"
        ]
        [ Svg.g [ Svg.Attributes.stroke "black" ] <| go (4 * (width + height + 2)) model.rx model.ry 0 0 [], grid ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Rx rx ->
            ( { model | rxString = rx }, Cmd.none )

        Ry ry ->
            ( { model | ryString = ry }, Cmd.none )

        Width width ->
            ( { model | widthString = width }, Cmd.none )

        Height height ->
            ( { model | heightString = height }, Cmd.none )

        SquareSize squareSize ->
            ( { model | squareSizeString = squareSize }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
