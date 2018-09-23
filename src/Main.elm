module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Events as E
import Html.Attributes as A
import WebGL
import Math.Vector2 as V2 exposing (Vec2, vec2)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Array exposing (Array)
import Set exposing (Set)
import Json.Decode

import Graphics exposing (initUniforms)

main = Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

maxSpawnedPlayers = 4
initRotVel = 1000.0

type alias Player =
    { position: Vec3
    , velocity: Vec3
    , controlInput: Vec2
    , rotation: Float
    , rotVel: Float
    , t_start: Float
    , t_end: Maybe Float
    , isBot: Bool
    }

type alias Game =
    { numPlayers: Int
    , players: Array Player
    }

type alias Model =
    { t: Float -- Time since game startup, used for some graphics effects.
    , nextGameTops: Int
    , nextGameBots: Int
    , pause: Bool
    , game: Maybe Game
    , lightDir: Vec3
    , pressed: Set String
    }

type Msg
    = AnimationFrame Float
    | PlayerControlChange Bool String
    | StartGame
    | PauseToggle
    | TopCount Int
    | BotCount Int
    | NoOp

init : () -> (Model, Cmd Msg)
init () = 
    let
        model = Model 0.0 2 2 False Nothing V3.k Set.empty
        game = startGame 0.0 model.nextGameTops model.nextGameBots
    in
        ({model|game=Just game},Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of
        AnimationFrame _ ->
            let
                delt = 16.0
            in
                ({model
                    |t = model.t+delt/1000.0
                    ,lightDir = 
                            Mat4.transform (Mat4.makeRotate (1/10.0*delt/1000.0) V3.j) model.lightDir
                    ,game=
                        Maybe.map (stepGame (model.t) (delt/1000.0)) model.game
                 }
                , Cmd.none)
        StartGame ->
            ({model|game = Just <| startGame model.t model.nextGameTops model.nextGameBots}, Cmd.none)
        PauseToggle ->
            ({model|pause=not model.pause},Cmd.none)
        PlayerControlChange pressed key ->
            let
                keySet = 
                    if pressed then
                        Set.insert key model.pressed
                    else
                        Set.remove key model.pressed
            in
                case key of
                    "p" ->
                        update (if pressed then PauseToggle else NoOp) model
                    "r" ->
                        update (if not pressed then StartGame else NoOp) model
                    _ ->
                        ({model|pressed=keySet,game=Maybe.map (updatePlayerControl keySet) model.game},Cmd.none)
        TopCount i ->
            let nextGameTops=clamp 0 4 i in
            ({model|nextGameTops=nextGameTops
             ,nextGameBots=clamp (max 0 (nextGameTops-2)) nextGameTops model.nextGameBots
             }
            ,Cmd.none
            )
        BotCount i ->
            ({model|nextGameBots=clamp (max 0 (model.nextGameTops-2)) model.nextGameTops i},Cmd.none)
        NoOp ->
            (model,Cmd.none)

startGame : Float -> Int -> Int -> Game
startGame t playerCount botCount =
    { numPlayers = playerCount
    , players = Array.initialize playerCount (startPlayer t playerCount botCount)
    }

startPlayer : Float -> Int -> Int -> Int -> Player
startPlayer t playerCount botCount index =
    Player 
        (V3.scale 3.0 <| vec3 (cos <| 2*Basics.pi*(toFloat index)/(toFloat playerCount)) (sin <| 2*Basics.pi*(toFloat index)/(toFloat playerCount)) 0.0)
        (vec3 0.0 0.0 0.0)
        (vec2 0.0 0.0)
        0 
        initRotVel
        t
        Nothing
        (playerCount-1-index<botCount)

updatePlayerControl : Set String -> Game -> Game
updatePlayerControl pressed g =
    let
        zvec = vec2 0.0 0.0
        p0 =
            (if Set.member "w" pressed then vec2 0.0 1.0 else zvec)
            |> (if Set.member "a" pressed then V2.add (vec2 -1.0 0.0) else identity)
            |> (if Set.member "s" pressed then V2.add (vec2 0.0 -1.0) else identity)
            |> (if Set.member "d" pressed then V2.add (vec2 1.0 0.0) else identity)
        p1 =
            (if Set.member "arrowup" pressed then vec2 0.0 1.0 else zvec)
            |> (if Set.member "arrowleft" pressed then V2.add (vec2 -1.0 0.0) else identity)
            |> (if Set.member "arrowdown" pressed then V2.add (vec2 0.0 -1.0) else identity)
            |> (if Set.member "arrowright" pressed then V2.add (vec2 1.0 0.0) else identity)
        p0p = Maybe.map (\p -> {p|controlInput=p0}) <| Array.get 0 g.players
        p1p = Maybe.map (\p -> {p|controlInput=p1}) <| Array.get 1 g.players
    in
        {g|players=
            case (p0p,p1p) of
                (Just z,Just o) ->
                    Array.set 0 z <| Array.set 1 o <| g.players
                (Just z,Nothing) ->
                    Array.set 0 z <| g.players
                _ ->
                    g.players
        }
    

stepGame : Float -> Float -> Game -> Game
stepGame time delt game =
    {game|players=Array.indexedMap (stepPlayer time delt game) game.players}

stepPlayer : Float -> Float -> Game -> Int -> Player -> Player
stepPlayer time delt game index player =
    let
        onPlaneOfPlatform = (abs ((V3.getZ player.position) + 9.0)) < 1.0
        onPlatform = onPlaneOfPlatform && (V3.distance player.position (vec3 0.0 0.0 -9.0) < 10.0)
        toCenter = V2.negate <| vec2 (V3.getX player.position) (V3.getY player.position)
        myPos = V2.negate <| toCenter
        toEnemy =
            game.players
                |> Array.indexedMap (\i p -> (i,p))
                |> Array.toList
                |> List.filter (\(i,p) -> not (i==index))
                |> List.map (\(_,p) -> vec2 (V3.getX p.position - V2.getX myPos) (V3.getY p.position - V2.getY myPos))
                |> List.foldl 
                    (\v v2 ->
                        case v2 of
                            Nothing ->
                                Just v
                            Just v1 ->
                                if V2.length v < V2.length v1 then
                                    Just v
                                else
                                    Just v1
                    ) Nothing
        contact = 
            game.players
                |> Array.indexedMap (\i p -> (i,p))
                |> Array.toList
                |> List.filter (\(i,p) -> not (i==index) && V2.length (vec2 (V3.getX p.position - V2.getX myPos) (V3.getY p.position - V2.getY myPos))<0.8)
                |> (\l -> (not (List.length l==0),l))
        newRotVel = 
            if Tuple.first contact then
                0.8*player.rotVel
            else if onPlatform then
                (0.999-0.01*(V3.length player.velocity))*player.rotVel
            else
                player.rotVel
        newRotation = 
            player.rotation+newRotVel*delt
        rotationCollapse = 1.0-player.rotVel/initRotVel
        rotationAccelDir = Mat4.transform (Mat4.makeRotate newRotation V3.k) V3.i
        rotationAccel = V3.scale (0.1*rotationCollapse*rotationCollapse) rotationAccelDir
        controlInput = 
            if onPlaneOfPlatform then
                if player.isBot then -- AI CODE! Ohno!
                    case toEnemy of
                        Just te ->
                            V2.normalize te
                        Nothing ->
                            V2.scale (0.1*(V2.length toCenter)) (toCenter)
                else
                    let
                        x = V2.getX player.controlInput
                        y = V2.getY player.controlInput
                        ca = cameraAngle time
                        c = cos ca
                        s = sin ca
                    in
                        vec2 (c*x-s*y) (s*x+c*y)
            else
                vec2 0.0 0.0
        newVelocity =
            if not (Tuple.first contact) then
                player.velocity
                    |> V3.add (V3.scale ((1.0-rotationCollapse)*delt) <| vec3 (V2.getX controlInput) (V2.getY controlInput) -1.0)
                    |> (if onPlatform then V3.setZ 0.0 else identity)
                    |> V3.add (V3.scale delt rotationAccel)
            else
                case toEnemy of
                    Nothing ->
                        player.velocity
                    Just te ->
                        (V3.scale (10.0*delt) <| V3.negate <| V3.normalize <| vec3 (V2.getX te) (V2.getY te) -0.01)
        newPosition = 
            player.position
                |> V3.add newVelocity
    in
        { player
        | rotVel = newRotVel
        , rotation = newRotation
        , velocity = newVelocity
        , position = newPosition
        }

        

gameWidth = 800
gameHeight = 680

view : Model -> Browser.Document Msg
view model = 
    { title="game"
    , body=
        [ Html.node "style" [] [Html.text "body{width:100%;margin:0;background:black;}"]
        , Html.div 
            [ A.style "width" <| (String.fromInt gameWidth ++ "px") 
            , A.style "margin" "auto"
            ] 
            [ WebGL.toHtmlWith
                [ WebGL.depth 1.0
                , WebGL.antialias
                , WebGL.clearColor 0.5 0.0 0.0 1.0
                ]
                [ A.width gameWidth
                , A.height gameHeight
                , A.style "padding" "1em 0"
                ]
                [ (Graphics.top <| attatchTransformData model [3] <| initUniforms (vec3 1.0 0.3 0.3) 0.0 model.lightDir)
                , (Graphics.top <| attatchTransformData model [4] <| initUniforms (vec3 0.3 0.3 1.0) 0.0 model.lightDir)
                , (Graphics.top <| attatchTransformData model [5] <| initUniforms (vec3 0.3 1.0 0.3) 0.0 model.lightDir)
                , (Graphics.top <| attatchTransformData model [6] <| initUniforms (vec3 1.0 1.0 0.3) 0.0 model.lightDir)
                , (Graphics.platform <| attatchTransformData model [0] <| initUniforms norecol 0.0 model.lightDir)
                , (Graphics.platform <| attatchTransformData model [1,0] <| initUniforms (vec3 0.5 0.5 0.6) 0.0 model.lightDir)
                , (Graphics.ocean <| attatchTransformData model [2] <| initUniforms norecol model.t model.lightDir)
                ]
            ]
        , Html.div
            [ A.style "width" <| (String.fromInt gameWidth ++ "px")
            , A.style "margin" "auto"
            , A.style "color" "white"
            , A.style "display" "flex"
            ]
            [ Html.div
                [ A.style "flex" "1" ]
                [ Html.p [] [Html.text "Players"]
                , quickButton "+" (TopCount <| model.nextGameTops+1)
                , quickNumInput TopCount 0 4 model.nextGameTops
                , quickButton "-" (TopCount <| model.nextGameTops-1)
                ]
            , Html.div
                [ A.style "flex" "1" ]
                [ Html.p [] [Html.text "Bots"]
                , quickButton "+" (BotCount <| model.nextGameBots+1)
                , quickNumInput TopCount 0 model.nextGameTops model.nextGameBots
                , quickButton "-" (BotCount <| model.nextGameBots-1)
                ]
            , Html.button
                [ A.style "flex" "1"
                , E.onClick StartGame
                ]
                [ Html.text "New game!" ]
            ]
        ]
    }

quickButton : String -> Msg -> Html Msg
quickButton text msg =
    Html.button
        [E.onClick msg]
        [Html.text text]

quickNumInput : (Int -> Msg) -> Int -> Int -> Int -> Html Msg
quickNumInput tagger min max value =
    Html.input
        [ A.type_ "number"
        , A.min (String.fromInt min)
        , A.max (String.fromInt max)
        , A.value (String.fromInt value)
        , E.onInput (String.toInt >> Maybe.map tagger >> Maybe.withDefault NoOp)
        ] []

norecol : Vec3
norecol = vec3 1.0 1.0 1.0

transforms : Model -> Array Mat4
transforms model = Array.append (Array.fromList
    [ Mat4.makeTranslate <| vec3 0.0 0.0 -1.0
    , Mat4.makeTranslate <| vec3 0.0 0.0 -0.1
    , Mat4.makeTranslate <| vec3 0.0 0.0 -40.0
    ])
    <| case model.game of
        Just g -> 
            Array.append 
                (Array.map 
                    (\p ->
                        Mat4.makeScale (vec3 0.1 0.1 0.1)
                            |> Mat4.translate p.position
                            |> Mat4.rotate (p.rotation) V3.k
                            |> Mat4.rotate (0.25*Basics.pi*(1.0-p.rotVel/initRotVel)) V3.i
                    ) g.players
                )
                (Array.repeat (maxSpawnedPlayers-g.numPlayers) (Mat4.makeScale (vec3 0.0 0.0 0.0)))
        Nothing ->
            Array.repeat maxSpawnedPlayers (Mat4.makeScale (vec3 0.0 0.0 0.0))

attatchTransformData : Model -> List Int -> Graphics.Uniforms -> Graphics.Uniforms
attatchTransformData model l uniforms =
    let
        transform = getTransformation model l
        normalTransform = Mat4.transpose <| Maybe.withDefault Mat4.identity <| Mat4.inverse <| transform
        wtc = getWorldToClip <| tempCameraMove model.t
    in
        {uniforms|worldToClip=wtc,transform=transform,normalTransform=normalTransform}


getTransformation : Model -> List Int -> Mat4
getTransformation model l =
    List.foldl (\i -> Mat4.mul <| getTransform model i) Mat4.identity l

getTransform : Model -> Int -> Mat4
getTransform model i =
    Maybe.withDefault Mat4.identity <| Array.get i (transforms model)

cameraAngle time =
    (time/6.0)

tempCameraMove : Float -> Mat4
tempCameraMove time =
    Mat4.makeTranslate (V3.negate V3.k)
        |> Mat4.rotate (cameraAngle time) V3.k
        |> Mat4.rotate 0.3 V3.i
        |> Mat4.translate (V3.scale 1.5 V3.k)

getWorldToClip : Mat4 -> Mat4
getWorldToClip cameraTransform =
    Mat4.mul getPerspective (Maybe.withDefault Mat4.identity <| Mat4.inverse cameraTransform)

getPerspective : Mat4
getPerspective =
    let
        near = 0.01
        far = 100.0
        fov = 80
    in
        Mat4.makePerspective fov (gameWidth/gameHeight) near far

subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.pause then
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta AnimationFrame
            , keySub
            ]
    else
        keySub

keySub : Sub Msg
keySub =
    Sub.batch
        [ Browser.Events.onKeyDown <|
            Json.Decode.map 
                (PlayerControlChange True << String.toLower)
                (Json.Decode.field "key" Json.Decode.string)
        , Browser.Events.onKeyUp <|
            Json.Decode.map
                (PlayerControlChange False << String.toLower)
                (Json.Decode.field "key" Json.Decode.string)
        ]
{--
    Sub.batch
        [ Browser.Events.onKeyDown 
            ( Json.Decode.map
                (\k -> 
                    case String.toLower k of
                        "p" ->
                            PauseToggle
                        "w" ->
                            PlayerControlChange 0 <| vec2 0.0 1.0
                        "a" ->
                            PlayerControlChange 0 <| vec2 -1.0 0.0
                        "s" ->
                            PlayerControlChange 0 <| vec2 0.0 -1.0
                        "d" ->
                            PlayerControlChange 0 <| vec2 1.0 0.0
                        "arrowup" ->
                            PlayerControlChange 1 <| vec2 0.0 1.0
                        "arrowleft" ->
                            PlayerControlChange 1 <| vec2 -1.0 0.0
                        "arrowdown" ->
                            PlayerControlChange 1 <| vec2 0.0 -1.0
                        "arrowright" ->
                            PlayerControlChange 1 <| vec2 1.0 0.0
                        "r" ->
                            StartGame
                        _ ->
                            NoOp
                )
                (Json.Decode.field "key" Json.Decode.string)
            )
        , Browser.Events.onKeyUp
            ( Json.Decode.map
                (\k -> 
                    case String.toLower k of
                        "p" ->
                            PauseToggle
                        "w" ->
                            PlayerControlChange 0 <| vec2 0.0 1.0
                        "a" ->
                            PlayerControlChange 0 <| vec2 -1.0 0.0
                        "s" ->
                            PlayerControlChange 0 <| vec2 0.0 -1.0
                        "d" ->
                            PlayerControlChange 0 <| vec2 1.0 0.0
                        "arrowup" ->
                            PlayerControlChange 1 <| vec2 0.0 1.0
                        "arrowleft" ->
                            PlayerControlChange 1 <| vec2 -1.0 0.0
                        "arrowdown" ->
                            PlayerControlChange 1 <| vec2 0.0 -1.0
                        "arrowright" ->
                            PlayerControlChange 1 <| vec2 1.0 0.0
                        "r" ->
                            StartGame
                        _ ->
                            NoOp
                )
                (Json.Decode.field "key" Json.Decode.string)
            )
        ]
--}