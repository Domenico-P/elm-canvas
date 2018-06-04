module Main exposing (..)

import Html
import Html.Styled exposing (..)
import Html.Decoder as Decoder
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Mouse exposing (..)
import Keyboard exposing (..)
import Json.Decode as Json
import Keyboard.Extra exposing (..)
import MultiTouch
import Css as CSS
import Css.Foreign as CSS


-- MODEL

type alias Model =
    { canvas_w : Int
    , canvas_h : Int
    , camera_x : Float
    , camera_y : Float
    , zoom : Float
    , drag : Maybe DragInfo
    , isSpacePressed : Bool
    , isCtrlPressed : Bool
    , cursor : String
    , x : Float
    , y : Float
    , rectangle : Maybe RectangleInfo
    }

type alias DragInfo =
    { initial : Mouse.Position
    , current : Mouse.Position
    }

type alias RectangleInfo =
    { initial : Mouse.Position
    , current : Mouse.Position
    }

model : Model
model =
    { canvas_w = 500
    , canvas_h = 400
    , camera_x = 0
    , camera_y = 0
    , zoom = 20
    , drag = Nothing
    , isSpacePressed = False
    , isCtrlPressed = False
    , cursor = "default"
    , x = 0 -- coordinata X del puntatore del mouse catturata quando scalo
    , y = 0 -- coordinata Y del puntatore del mouse catturata quando scalo
    , rectangle = Nothing
    }

init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


-- MESSAGES

type Msg
    = Sinistra
    | Destra
    | Sopra
    | Sotto
    | Zoom_P
    | Zoom_M
    | SpaceDown
    | SpaceUp
    | CtrlDown
    | CtrlUp
    | Noop
    | Move Float Float
    | Scale Float Float Float
    | DragStart Mouse.Position
    | DragEnd Mouse.Position
    | DragAt Mouse.Position
    | Reload


-- Utilities

px a = toString a ++ "px"

-- CSS

stylesheet = CSS.global
  [ CSS.body
    [ (CSS.property "touch-action" "none") ]
  ]


-- VIEW

canvasStyle : Model -> Attribute msg
canvasStyle model =
    style
        [ ( "width", px model.canvas_w )
        , ( "height", px model.canvas_h )
        , ( "position", "absolute" )
        , ( "top", "50%" )
        , ( "left", "50%" )
        , ( "margin-top", px ((toFloat model.canvas_h) * -0.5) )
        , ( "margin-left", px ((toFloat model.canvas_w) * -0.5) )
        , ( "border-style", "solid" )
        , ( "border-color", "#000000" )
        , ( "background-color", "#FFFFFF" )
        , ( "cursor", model.cursor )
        ]

crossStyle : Model -> Attribute msg
crossStyle model =
  let
    zoom = model.zoom
    dragDelta = getDragDelta model
    cameraX = (toFloat dragDelta.x) + (zoom * model.camera_x)
    cameraY = (toFloat dragDelta.y) + (zoom * model.camera_y)
  in
    style
        [
          ( "width", "20px" )
        , ( "height", "20px" )
        , ( "position", "absolute" )
        , ( "pointer-events", "none" )
        , ( "top", "50%" )
        , ( "left", "50%" )
        , ( "margin-top", toString (round cameraY - 10) ++ "px" ) --round arrotonda all'intero piu vicino
        , ( "margin-left", toString (round cameraX - 10) ++ "px" )
        ]

crossStyleH : Attribute msg
crossStyleH =
    style
        [
          ( "background", "black" )
        , ( "width", "10px" )
        , ( "height", "2px" )
        , ( "position", "absolute" )
        , ( "top", "50%" )
        , ( "left", "50%" )
        , ( "margin-top", "-1px" )
        , ( "margin-left", "-5px" )
        ]

crossStyleV : Attribute msg
crossStyleV =
    style
        [
          ( "background", "black" )
        , ( "width", "2px" )
        , ( "height", "10px")
        , ( "position", "absolute" )
        , ( "top", "50%" )
        , ( "left", "50%" )
        , ( "margin-top", "-5px" )
        , ( "margin-left", "-1px" )
        ]

bottonStyle : Attribute msg
bottonStyle =
    style
    [
      ("cursor","pointer")
    ]

imgStyle : Attribute msg
imgStyle =
     style
     [
         ( "width", "10px" )
       , ( "height", "10px")
     ]

contentStyle : Attribute msg
contentStyle =
    style []

coordinateStyle : Attribute msg
coordinateStyle =
    style
        [
          ( "padding", "10px" )
        ]

renderSquare : Model -> Int -> Int -> Int -> Int ->  Html Msg
renderSquare model centerX centerY w h =
  let
    dragDelta = getDragDelta model
    dragY = toFloat (dragDelta.y)
    dragX = toFloat (dragDelta.x)
    zoom = model.zoom
    realW = zoom * (toFloat w)
    realH = zoom * (toFloat h)
    realX = zoom * (toFloat centerX)
    realY = zoom * (toFloat centerY)
    cameraX = zoom * model.camera_x
    cameraY = zoom * model.camera_y

  in
    div [ style
            [ ( "background", "green" )
            , ( "width", px realW)
            , ( "height", px realH)
            , ( "position", "absolute" )
            , ( "pointer-events", "none" )
            , ( "margin-top", px (cameraY + dragY + realY + (realH * -0.5)))
            , ( "margin-left", px (cameraX +  dragX + realX + (realW * -0.5)))
            , ( "top", "50%" )
            , ( "left", "50%" )
            ]
        ] []

stopAndPreventOptions =
  { stopPropagation = True
  , preventDefault = True }

onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    onWithOptions
    "keydown"
    stopAndPreventOptions
    (Json.map tagger keyCode)

onKeyup : (Int -> msg) -> Attribute msg
onKeyup tagger =
    onWithOptions
    "keyup"
    stopAndPreventOptions
    (Json.map tagger keyCode)

onKeydownTagger : Int -> Msg
onKeydownTagger code =
  case code of
    37 -> Sinistra
    38 -> Sopra
    39 -> Destra
    40 -> Sotto
    32 -> SpaceDown
    17 -> CtrlDown
    _ -> Noop

onKeyupTagger : Int -> Msg
onKeyupTagger code =
  case code of
    --- arrow
    32 -> SpaceUp
    17 -> CtrlUp
    _ -> Noop



onDrag : List (Attribute Msg)
onDrag =
    [ on "mousedown" (Json.map DragStart Mouse.position)
    , on "mouseup" (Json.map DragEnd Mouse.position) ]

getDragDelta : Model -> Mouse.Position
getDragDelta model = case model.drag of
  Just drag -> { x = drag.current.x - drag.initial.x
               , y = drag.current.y - drag.initial.y }
  Nothing -> { x = 0, y = 0 }


onWheel : (Float -> Float -> Bool -> Float -> Float -> Msg) -> Attribute Msg
onWheel tagger =
    onWithOptions "wheel" stopAndPreventOptions (Json.map5 tagger
                                                           ( Json.at ["deltaX"] Json.float )
                                                           ( Json.at ["deltaY"] Json.float )
                                                           ( Json.at ["ctrlKey"] Json.bool )
                                                           ( Json.at ["offsetX"] Json.float )
                                                           ( Json.at ["offsetY"] Json.float )
                                                           )

onWheelTagger deltaX deltaY ctrlKey offsetX offsetY =
  if ctrlKey then
        Scale -deltaY offsetX offsetY

  else Move deltaX deltaY



view : Model -> Html Msg
view model =
              div []
              [ stylesheet
              , div [contentStyle]
                [ div [coordinateStyle]
                  ([ text ("Camera_x: ")
                  , text (toString (round model.camera_x))
                  , text (", Camera_y: ")
                  , text (toString (round (model.camera_y * -1)))
                  , text (", Zoom: ")
                  , text (toString (round (model.zoom)))
                  , text (", ScaleMouseXY: ") -- Posizione mouse per scalare
                  , text (toString (round (model.x)))
                  , text ("x")
                  , text (toString (round (model.y)))
                  , br [] []
                  ] ++ (case model.drag of
                      Nothing -> [  text "Not dragging" ]

                      Just drag -> [ text "Dragging: initial="
                                   , text (px drag.initial.x)
                                   , text "×"
                                   , text (px drag.initial.y)
                                   , text " current="
                                   , text (px drag.current.x)
                                   , text "×"
                                   , text (px drag.current.y)
                                   ])
                    ++ (case model.rectangle of
                        Nothing -> [  text " Not designing!" ]

                        Just rectangle -> [ text " Rectangle_start: "
                                     , text (px rectangle.initial.x)
                                     , text "×"
                                     , text (px rectangle.initial.y)
                                     , text " Rectangle_end: "
                                     , text (px rectangle.current.x)
                                     , text "×"
                                     , text (px rectangle.current.y)
                                     ])
                  )

                , div []
                  [ button [ bottonStyle, onClick Sinistra ] [ text "←" ]
                  , button [ bottonStyle, onClick Sopra ] [ text "↑" ]
                  , button [ bottonStyle, onClick Sotto ] [ text "↓"  ]
                  , button [ bottonStyle, onClick Destra ] [ text "→"  ]
                  , button [ bottonStyle, onClick Zoom_P ] [ img [ imgStyle, src "img/zoom-in.png" ] [] ]
                  , button [ bottonStyle, onClick Zoom_M ] [ img [ imgStyle, src "img/zoom-out.png" ] []  ]
                  , button [ bottonStyle, onClick Reload ] [ img [ imgStyle, src "img/reload.png" ] []  ]
                  ]
                ]
                , div ([ canvasStyle model
                       , (tabindex 0)
                       , onKeydown onKeydownTagger
                       , onKeyup onKeyupTagger
                       , onWheel onWheelTagger
                       , style [ ( "touch-action", "none" ) ]
                       ] ++ onDrag )
                  [ renderSquare model 3 3 2 2
                  , renderSquare model -1 -1 3 1
                  , renderSquare model 5 -5 1 1
                  , div [ crossStyle model ]
                    [ div [crossStyleH] []
                    , div [crossStyleV] []
                    ]
                  ]
              ]


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Sinistra -> (
                    { model | camera_x = model.camera_x - 1 }
                    , Cmd.none )

        Destra -> (
                  { model | camera_x = model.camera_x + 1 }
                  , Cmd.none )

        Sopra ->  (
                  { model | camera_y = model.camera_y - 1 }
                  , Cmd.none )

        Sotto ->  (
                  { model | camera_y = model.camera_y + 1 }
                  , Cmd.none )

        Zoom_P -> (
                  { model | zoom = model.zoom + 1 }
                  , Cmd.none )

        Zoom_M ->
                  if model.zoom > 1 then
                  ({ model | zoom = model.zoom - 1 }
                  , Cmd.none )
                  else
                  ( model , Cmd.none )

        SpaceDown -> ({ model | isSpacePressed = True }, Cmd.none)

        SpaceUp -> ({ model | isSpacePressed = False }, Cmd.none)

        CtrlDown -> ({ model | isCtrlPressed = True }, Cmd.none)

        CtrlUp -> ({ model | isCtrlPressed = False }, Cmd.none)

        Move deltaX deltaY -> ({ model | camera_x = model.camera_x - (deltaX / model.zoom)
                                       , camera_y = model.camera_y - (deltaY / model.zoom)
                               }
                              , Cmd.none)

        Scale deltaScale offsetX offsetY ->
                                          let
                                            nextZoom = model.zoom + deltaScale
                                            x = (offsetX - (toFloat model.canvas_w / 2)) / model.zoom -- centro nella canvas il puntatore del mouse in modo che mi dia le coordinate che partono da (0,0) dal centro
                                            y = (offsetY - (toFloat model.canvas_h / 2)) / model.zoom

                                          in
                                            if nextZoom > 1 then
                                              ({ model | zoom = nextZoom
                                                       , x = x --mando alla model mouse position x
                                                       , y = y --mando alla model mouse position y
                                                       , camera_x = model.camera_x - (deltaScale * x / nextZoom) --aggiorno cameraX
                                                       , camera_y = model.camera_y - (deltaScale * y / nextZoom) --aggiorno cameraY
                                                       }
                                              , Cmd.none)
                                            else
                                              ( model , Cmd.none )

        DragStart pos ->
                      if model.isSpacePressed then
                        ({ model | drag = Just { initial = pos , current = pos }
                                 , cursor = "-webkit-grabbing" --chrome
                                 --, cursor = "grabbing" --firefox
                                 }
                         , Cmd.none )

                      else
                        ({ model | drag = Nothing } , Cmd.none)

        DragAt pos -> (
                      { model | drag = case model.drag of
                                        Just drag -> Just { drag | current = pos }

                                        Nothing -> Nothing
                      }
                      , Cmd.none )

        DragEnd pos ->
                      let
                        delta = getDragDelta model
                      in
                        ({ model | drag = Nothing
                                 , camera_x = (toFloat delta.x / model.zoom) + model.camera_x
                                 , camera_y = (toFloat delta.y / model.zoom) + model.camera_y
                                 , cursor = "default"
                                 }
                        , Cmd.none )

        Reload -> ({ model | camera_x = 0
                           , camera_y = 0
                           , zoom = 20
                           , x = 0
                           , y = 0
                           }
                  , Cmd.none )

        Noop -> (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
            Sub.batch --Rimane in ascolto su più eventi
              [
                Mouse.moves DragAt
              ]



main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = subscriptions
        }
