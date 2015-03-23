module Todo where
{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

This clean division of concerns is a core part of Elm. You can read more about
this in the Pong tutorial: http://elm-lang.org/blog/Pong.elm

This program is not particularly large, so definitely see the following
document for notes on structuring more complex GUIs with Elm:
http://elm-lang.org/learn/Architecture.elm
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


---- MODEL ----

-- The full application state of our todo app.
type alias Model =
    { tasks      : List Task
    , field      : String
    , uid        : Int
    , visibility : String
    , sales      : Int
    , revenue    : Int
    , price      : Int
    , timeLeft   : Int
    , tickets    : Int 
    }

type alias Task =
    { description : String
    , completed   : Bool
    , editing     : Bool
    , id          : Int
    }

newTask : String -> Int -> Task
newTask desc id =
    { description = desc
    , completed = False 
    , editing = False
    , id = id
    }

emptyModel : Model
emptyModel =
    { tasks = []
    , visibility = "All"
    , field = ""
    , uid = 0
    , sales = 0
    , revenue = 0
    , price = 100 
    , timeLeft = 120 
    , tickets = 10
    }

---- UPDATE ----

-- A description of the kinds of actions that can be performed on the model of
-- our application. See the following post for more info on this pattern and
-- some alternatives: http://elm-lang.org/learn/Architecture.elm
type Action
    = NoOp
    | UpdateField String
    | EditingTask Int Bool
    | UpdateTask Int String
    | Add
    | Delete Int
    | DeleteComplete
    | Check Int Bool
    | CheckAll Bool
    | ChangeVisibility String
    | MakePurchase

-- How we update our Model on a given Action?
update : Action -> Model -> Model
update action model =
    case action of
      NoOp -> model

      Add ->
          { model |
              uid <- model.uid + 1,
              field <- "",
              tasks <-
                  if String.isEmpty model.field
                    then model.tasks
                    else model.tasks ++ [newTask model.field model.uid]
          }

      UpdateField str ->
          { model | field <- str }

      EditingTask id isEditing ->
          let updateTask t = if t.id == id then { t | editing <- isEditing } else t
          in
              { model | tasks <- List.map updateTask model.tasks }

      UpdateTask id task ->
          let updateTask t = if t.id == id then { t | description <- task } else t
          in
              { model | tasks <- List.map updateTask model.tasks }

      Delete id ->
          { model | tasks <- List.filter (\t -> t.id /= id) model.tasks }

      DeleteComplete ->
          { model | tasks <- List.filter (not << .completed) model.tasks }

      Check id isCompleted ->
          let updateTask t = if t.id == id then { t | completed <- isCompleted } else t
          in
              { model | tasks <- List.map updateTask model.tasks }

      CheckAll isCompleted ->
          let updateTask t = { t | completed <- isCompleted }
          in
              { model | tasks <- List.map updateTask model.tasks }

      ChangeVisibility visibility ->
          { model | visibility <- visibility }

      MakePurchase ->
          { model |
              sales <- model.sales + 1,
              revenue <- model.revenue + model.price,
              tickets <- model.tickets - 1 
          }

---- VIEW ----

view : Model -> Html
view model =
    div
      [ class "todomvc-wrapper"
      , style [ ("visibility", "hidden") ]
      ]
      [ section
          [ id "todoapp" ]
          [ lazy taskEntry model.field
          , lazy2 taskList model.visibility model.tasks
          , lazy3 controls model.visibility model.tasks model.tickets
          ]
      , otherFooter model 
      ]

onEnter : Signal.Message -> Attribute
onEnter message =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

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
          , on "input" targetValue (Signal.send updates << UpdateField)
          , onEnter (Signal.send updates Add)
          ]
          []
      ]

taskList : String -> List Task -> Html
taskList visibility tasks =
    let isVisible todo =
            case visibility of
              "Completed" -> todo.completed
              "Active" -> not todo.completed
              "All" -> True

        allCompleted = List.all .completed tasks

        cssVisibility = if List.isEmpty tasks then "hidden" else "visible"
    in
    section
      [ id "main"
      , style [ ("visibility", cssVisibility) ]
      ]
      [ input
          [ id "toggle-all"
          , type' "checkbox"
          , name "toggle"
          , checked allCompleted
          , onClick (Signal.send updates (CheckAll (not allCompleted)))
          ]
          []
      , label
          [ for "toggle-all" ]
          [ text "Mark all as complete" ]
      , ul
          [ id "todo-list" ]
          (List.map todoItem (List.filter isVisible tasks))
      ]

todoItem : Task -> Html
todoItem todo =
    let className = (if todo.completed then "completed " else "") ++
                    (if todo.editing   then "editing"    else "")
    in

    li
      [ class className ]
      [ div
          [ class "view" ]
          [ input
              [ class "toggle"
              , type' "checkbox"
              , checked todo.completed
              , onClick (Signal.send updates (Check todo.id (not todo.completed)))
              ]
              []
          , label
              [ onDoubleClick (Signal.send updates (EditingTask todo.id True)) ]
              [ text todo.description ]
          , button
              [ class "destroy"
              , onClick (Signal.send updates (Delete todo.id))
              ]
              []
          ]
      , input
          [ class "edit"
          , value todo.description
          , name "title"
          , id ("todo-" ++ toString todo.id)
          , on "input" targetValue (Signal.send updates << UpdateTask todo.id)
          , onBlur (Signal.send updates (EditingTask todo.id False))
          , onEnter (Signal.send updates (EditingTask todo.id False))
          ]
          []
      ]

controls : String -> List Task -> Int -> Html
controls visibility tasks ticketsLeft =
    let tasksCompleted = List.length (List.filter .completed tasks)
        tasksLeft = List.length tasks - tasksCompleted
        item_ = if tasksLeft == 1 then " item" else " items"
    in
    footer
      [ id "footer"
      ]
      [ span
          [ id "todo-count" ]
          [ strong [] [ text (toString tasksLeft) ]
          , text (item_ ++ " left")
          ]
      , ul -- this should be where we put the reset button
          [ id "filters" ]
          [ visibilitySwap "#/" "All" visibility
          , text " "
          , visibilitySwap "#/active" "Active" visibility
          , text " "
          , visibilitySwap "#/completed" "Completed" visibility
          ]
      , button
          [ class "clear-completed"
          , id "clear-completed"
          , hidden (ticketsLeft <= 0)
          , onClick (Signal.send updates MakePurchase)
          ]
          [ text ("Simulate Purchase") ]
      ]

visibilitySwap : String -> String -> String -> Html
visibilitySwap uri visibility actualVisibility =
    let className = if visibility == actualVisibility then "selected" else "" in
    li
      [ onClick (Signal.send updates (ChangeVisibility visibility)) ]
      [ a [ class className, href uri ] [ text visibility ] ]

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
model = Signal.foldp update initialModel (Signal.subscribe updates)

initialModel : Model
initialModel =
  Maybe.withDefault emptyModel Nothing 

-- updates from user input
updates : Signal.Channel Action
updates = Signal.channel NoOp

port focus : Signal String
port focus =
    let needsFocus act =
            case act of
              EditingTask id bool -> bool
              _ -> False

        toSelector (EditingTask id _) = ("#todo-" ++ toString id)
    in
        Signal.subscribe updates
          |> Signal.keepIf needsFocus (EditingTask 0 True)
          |> Signal.map toSelector


-- interactions with localStorage to save the model
--port getStorage : Maybe Model

--port setStorage : Signal Model
--port setStorage = model
