port module GuessMyAge exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (value, disabled, style)
import Html.Events exposing (..)
import String
import Random


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
    , trials : Int
    , entryAge : String
    , submittedAge : Int
    , submitError : Bool
    }


type Msg
    = NoOp
    | LaunchGame
    | StartGame Int
    | Submit
    | ChangeEntryAge String


init : ( Model, Cmd Msg )
init =
    initModel ! [ launchGame ]


launchGame : Cmd Msg
launchGame =
    Random.generate StartGame (Random.int 1 100)


initModel : Model
initModel =
    { started = False
    , age = 0
    , trials = 0
    , entryAge = "1"
    , submittedAge = -1
    , submitError = False
    }


view : Model -> Html Msg
view model =
    let
        finished =
            model.submittedAge == model.age

        tip =
            if model.submittedAge > model.age then
                "I'm younger than " ++ (toString model.submittedAge)
            else if model.submittedAge < model.age then
                "I'm older than " ++ (toString model.submittedAge)
            else
                (toString model.submittedAge) ++ " is my age! Congratulations!"
    in
        div
            []
            [ div []
                [ text <| "Guess my age (in years)" ++ ", you still have " ++ (toString model.trials) ++ " attempts" ]
            , div []
                [ input
                    [ value <| model.entryAge
                    , onInput ChangeEntryAge
                    , disabled finished
                    ]
                    []
                , button [ onClick Submit, disabled finished ] [ text "submit" ]
                , if model.submitError then
                    span [ style [ ( "color", "red" ) ] ] [ text "Age must be an integer between 1 and 100" ]
                  else
                    text ""
                ]
            , if model.submittedAge >= 0 then
                text tip
              else
                text ""
            , div []
                [ button [ style [ ( "margin-top", "10px" ) ], onClick LaunchGame ] [ text "New game" ] ]
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        LaunchGame ->
            model ! [ Random.generate StartGame (Random.int 1 100) ]

        StartGame age ->
            { model
                | age = age
                , trials = 10
                , entryAge = "1"
                , submittedAge = -1
                , submitError = False
            }
                ! []

        ChangeEntryAge ageStr ->
            { model | entryAge = ageStr } ! []

        Submit ->
            submit model ! []


submit : Model -> Model
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
            { model | submitError = True }
        else
            { model | submittedAge = entryAgeConverted, submitError = False, trials = model.trials - 1 }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
