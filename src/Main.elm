port module GuessMyAge exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (value, disabled, style, id, class, classList)
import Html.Events exposing (..)
import String
import Random
import Json.Decode as Json


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { started : Bool
    , age : Int
    , remainingAttempts : Int
    , entryAge : String
    , youngerThan : Maybe Int
    , olderThan : Maybe Int
    , submittedAge : Maybe Int
    , submitError : Bool
    }


type Msg
    = NoOp
    | LaunchGame
    | StartGame Int
    | Submit
    | ChangeEntryAge String
    | EntryKeyUp Int


init : ( Model, Cmd Msg )
init =
    initModel ! [ launchGame ]


lowerLimit : Int
lowerLimit =
    1


higherLimit : Int
higherLimit =
    100


launchGame : Cmd Msg
launchGame =
    Random.generate StartGame (Random.int lowerLimit higherLimit)


initModel : Model
initModel =
    { started = False
    , age = 0
    , remainingAttempts = 0
    , entryAge = "1"
    , youngerThan = Nothing
    , olderThan = Nothing
    , submittedAge = Nothing
    , submitError = False
    }


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (Json.map tagger keyCode)


view : Model -> Html Msg
view model =
    let
        success =
            Maybe.withDefault -1 model.submittedAge == model.age

        finished =
            success || model.remainingAttempts == 0

        tip age =
            if success then
                (toString <| model.age) ++ " is my age! Congratulations!"
            else if finished then
                "You failed to guess my age, I'm " ++ (toString <| model.age) ++ " years old! Please, try again!"
            else if model.youngerThan /= Nothing && model.olderThan /= Nothing then
                "I'm older than "
                    ++ (toString <| Maybe.withDefault -1 model.olderThan)
                    ++ " and younger than "
                    ++ (toString <| Maybe.withDefault -1 model.youngerThan)
            else if model.youngerThan /= Nothing then
                "I'm younger than " ++ (toString <| Maybe.withDefault -1 model.youngerThan)
            else if model.olderThan /= Nothing then
                "I'm older than " ++ (toString <| Maybe.withDefault -1 model.olderThan)
            else
                ""

        forgeTip =
            case model.submittedAge of
                Nothing ->
                    text ""

                Just age ->
                    div [ class "tip" ] [ strong [] [ text <| tip age ] ]
    in
        div
            [ classList [ ( "content", True ), ( "success", success ), ( "failure", finished && not success ) ]
            ]
            [ div [ class "title" ]
                [ text <| "Guess my age (in years)" ++ ", you still have " ++ (toString model.remainingAttempts) ++ " attempts" ]
            , div [ class "input" ]
                [ input
                    [ id "entry"
                    , value <| model.entryAge
                    , onInput ChangeEntryAge
                    , onKeyUp EntryKeyUp
                    , disabled finished
                    ]
                    []
                , button [ onClick Submit, disabled finished ] [ text "submit" ]
                , if model.submitError then
                    span [ style [ ( "color", "red" ) ] ] [ text "Age must be an integer between 1 and 100" ]
                  else
                    text ""
                ]
            , forgeTip
            , div [ class "button" ]
                [ button [ style [ ( "margin-top", "10px" ) ], onClick LaunchGame ] [ text "New game (with another dude)" ] ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        LaunchGame ->
            model ! [ launchGame ]

        StartGame age ->
            start age model ! [ focus "#entry" ]

        ChangeEntryAge ageStr ->
            { model | entryAge = ageStr } ! []

        EntryKeyUp key ->
            if key == 13 then
                submit model
            else
                model ! []

        Submit ->
            submit model


start : Int -> Model -> Model
start age model =
    { model
        | age = age
        , remainingAttempts = 10
        , entryAge = "1"
        , submittedAge = Nothing
        , youngerThan = Nothing
        , olderThan = Nothing
        , submitError = False
    }


submit : Model -> ( Model, Cmd Msg )
submit model =
    let
        entryAgeConverted =
            case String.toInt model.entryAge of
                Ok age ->
                    if age >= 1 && age <= 100 then
                        age
                    else
                        -1

                Err _ ->
                    -1
    in
        if entryAgeConverted == -1 then
            { model | submitError = True } ! [ focus "#entry" ]
        else
            { model
                | submittedAge = Just entryAgeConverted
                , youngerThan = updateYoungerThan entryAgeConverted model
                , olderThan = updateOlderThan entryAgeConverted model
                , submitError = False
                , remainingAttempts = model.remainingAttempts - 1
            }
                ! [ focus "#entry" ]


updateYoungerThan : Int -> Model -> Maybe Int
updateYoungerThan age model =
    let
        youngerThan =
            Maybe.withDefault (higherLimit + 1) model.youngerThan
    in
        if age < youngerThan && age >= model.age then
            Just age
        else
            model.youngerThan


updateOlderThan : Int -> Model -> Maybe Int
updateOlderThan age model =
    let
        olderThan =
            Maybe.withDefault (lowerLimit - 1) model.olderThan
    in
        if age > olderThan && age <= model.age then
            Just age
        else
            model.olderThan


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


port focus : String -> Cmd msg
