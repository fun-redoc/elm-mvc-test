module App.Main where

import Html exposing (ul,li,div, text, input, Html)
import Html.Events exposing (on,keyCode,targetValue)
import Html.Attributes exposing (id,name,value,autofocus)
-- import StartApp
import Signal exposing (Address)
import Json.Decode as Json
import String
import Time as T exposing (Time)

-- # Settings
type alias HtmlId = String
newNameHtmlId : HtmlId
newNameHtmlId = "new_name"

-- # Main
-- main = StartApp.start { model = model, view = view, update = update }
main : Signal Html
main = Signal.map (view actions.address) (Signal.foldp update model mainSignal)

mainSignal : Signal Action
mainSignal = Signal.mergeMany [actions.signal]


-- # Model
type alias Entry = { name:String, age:Maybe Int }
emptyEntry = {name="", age=Nothing}
type alias Model = { new:Entry, all:List Entry}
model : Model
model = {new=emptyEntry, all=[{name="Natalie",age=Just 47},{name="Roland",age=Just 48},{name="Sophie",age=Just 11}]}

-- # Actions

actions : Signal.Mailbox Action
actions = Signal.mailbox NoOp

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    Add -> {model | all<-(model.new :: model.all), new<-emptyEntry}
    New entry -> {model | new <- entry }

-- # View

type Action = NoOp | Add | New Entry

is13 : Int -> Result String ()
is13 code =
    if code == 13 then Ok () else Err "not the right key code"

isAge : String -> Result String Int
isAge str = Err "wrong Format"

view : Address Action -> Model -> Html
view address model =
  let ulEntries = ul [] (List.map (\{name,age}->li [] [ text (name++(Maybe.withDefault "" (Maybe.map toString age))) ]) model.all)
  in div [] [ulEntries
     , input
               [ name "name"
               , autofocus True
               , id newNameHtmlId
               , value model.new.name
               , on "input" targetValue (\str-> Signal.message address (New {name=str,age=model.new.age}))
               , on "keydown" (Json.customDecoder keyCode is13) (\_->Signal.message address Add )
               ] []
      , input
               [ name "age"
               , id "new_age"
               , value (Maybe.withDefault "" (Maybe.map toString model.new.age))
               , on "input" targetValue (\str-> Signal.message address (New {name=model.new.name,age=(case String.toInt str of 
                                                                                                        Err _ -> model.new.age
                                                                                                        Ok n -> Just n)}))
               , on "keydown" (Json.customDecoder keyCode is13) (\_->Signal.message address Add )
               ] []
     ]

addSignals : Signal Action
addSignals = Signal.filter ( (==) Add) NoOp actions.signal

port focusOnNewName : Signal HtmlId
port focusOnNewName = Signal.map (\_->newNameHtmlId) addSignals
--port time : Signal Time
--port time = T.every T.second
