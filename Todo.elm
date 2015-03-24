module Todo where
{-| Demo of Pricefly ticketing technology 

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

-}

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2, lazy3)
import Json.Decode as Json
import List
import Maybe
import Signal
import String
import Window
import Time (..)

---- MODEL ----

-- The full application state of our app.
type alias Model =
    { tasks      : List Task
    , sales      : Int
    , revenue    : Int
    , price      : Int
    , timeLeft   : Int
    , totalTime  : Int
    , tickets    : Int 
    }

type alias Task =
    { description : String}

newTask : String -> Task
newTask desc =
    { description = desc}

emptyModel : Model
emptyModel =
    { tasks = []
    , sales = 0
    , revenue = 0
    , price = 100 
    , timeLeft = 30 
    , totalTime = 30 
    , tickets = 3 
    }

---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application. 
type Action
    = NoOp
    | MakePurchase
    | Reset 

-- How we update our Model on a given Input?
processInput : Input -> Model -> Model
processInput input model = 
  case input of 
    Clicky action -> processAction action model
    TimeStep time -> processTime time model

-- How the model responds to time changes
processTime : Time -> Model -> Model
processTime time model = 
  if model.timeLeft <= 0 
     then model
     else {model | 
            timeLeft <- model.timeLeft - 1,
            price <- priceTickets model
          }

-- How the model responds to user actions 
processAction : Action -> Model -> Model
processAction action model =
    case action of
      NoOp -> model

      MakePurchase ->
          { model |
              sales <- model.sales + 1,
              revenue <- model.revenue + model.price,
              tasks <-
                    (newTask (toSale model.price model.timeLeft) ) :: model.tasks,
              price <- priceTickets model
          }

      Reset -> emptyModel

-- Utility function for formatting sale descriptions
toSale price timeLeft = 
  let middle = if timeLeft > 1 
                then " seconds"
                else " second"
      front = "Sold ticket for " 
            ++ (toString price) 
            ++ " dollars with "
            ++ (toString timeLeft)
      end = " remaining."
  in
     front ++ middle ++ end

---- Utility Functions for Updating the Price
--priceTickets : Int -> Int -> Int -> Int -> Int
priceTickets model = 
  let tt = toFloat model.totalTime
      tu = toFloat (model.totalTime - model.timeLeft)
      it = toFloat model.tickets
      iu = toFloat model.sales
  in if (tu / tt) > (iu / it) 
        then transform (model.price - 1) model
        else transform (model.price + 1) model

transform suggestion model = 
  if suggestion >= 0 then suggestion else 0

---- VIEW ----

view : Model -> Html
view model =
    div
      [ class "todomvc-wrapper"
      , style [ ("visibility", "hidden") ]
      ]
      [ section
          [ id "todoapp" ]
          [ lazy taskEntry "" 
          , lazy taskList model.tasks
          , lazy3 controls model.tasks model.tickets model.timeLeft
          ]
      , otherFooter model 
      ]

taskEntry : String -> Html
taskEntry task =
    header 
      [ id "header" ]
      [ h1 [] [ text "Pricefly" ]
      , input
          [ id "new-todo"
          , placeholder "Get rid of me"
          , autofocus True
          , value task
          , name "newTodo"
          ]
          []
      ]

taskList : List Task -> Html
taskList tasks =
    let cssVisibility = if List.isEmpty tasks 
                           then "hidden" 
                           else "visible"
    in
    section
      [ id "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ ul
          [ id "todo-list" ]
          (List.map todoItem tasks)
      ]

todoItem : Task -> Html
todoItem todo =
    let className = ""
    in

    li
      [ class className ]
      [ div
          [ class "view" ]
          [ label
              [ onDoubleClick (Signal.send updates (NoOp)) ]
              [ text todo.description ]
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          ]
          []
      ]

controls : List Task -> Int -> Int -> Html
controls tasks tickets timeLeft =
    let totalSales = List.length tasks 
        ticketsLeft = tickets - totalSales 
        ticket_ = if ticketsLeft == 1 then " ticket" else " tickets"
    in
    footer
      [ id "footer"
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString ticketsLeft) ]
          , text (ticket_ ++ " left")
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , onClick (Signal.send updates Reset)
          ]
          [ text ("Reset") ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (ticketsLeft <= 0 || timeLeft <= 0)
          , onClick (Signal.send updates MakePurchase)
          ]
          [ text ("Simulate Purchase") ]
      ]

otherFooter : Model -> Html
otherFooter model =
    footer [ id "info" ]
      [
        p [] [ text ("sales: " ++ (toString model.sales)) ]
      , p [] [ text ("revenue: " ++ (toString model.revenue)) ]
      , p [] [ text ("price: " ++ (toString model.price)) ]
      , p [] [ text ("timeLeft: " ++ (toString model.timeLeft)) ]
      , p [] [ text ("tickets: " ++ (toString model.tickets)) ]
      ]

---- INPUTS ----

-- wire the entire application together
main : Signal Html
main = Signal.map view model

-- manage the model of our application over time
model : Signal Model
model = Signal.foldp processInput emptyModel (inputs)

-- updates from user input
updates : Signal.Channel Action
updates = Signal.channel NoOp

-- merge signals from user input and time passing
type Input = Clicky Action | TimeStep Time

inputs : Signal Input
inputs = Signal.merge actionSig timeSig

actionSig : Signal Input 
actionSig = Signal.map Clicky (Signal.subscribe updates)

timeSig : Signal Input 
timeSig = Signal.map TimeStep (every second)
