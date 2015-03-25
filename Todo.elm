module Todo where
{-| Demo of Pricefly ticketingThis application is broken up into four distinct parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML
  4. Inputs - the signals necessary to manage events

-}

import Html (..)
import Html.Attributes as A
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
    { saleList   : List Sale 
    , sales      : Int
    , revenue    : Int
    , price      : Int
    , timeLeft   : Int
    , totalTime  : Int
    , tickets    : Int 
    }

type alias Sale =
    { price    : Int
    , timeLeft : Int
    }

newSale : Int -> Int -> Sale 
newSale p t =
    { price    = p 
    , timeLeft = t
    }

emptyModel : Model
emptyModel =
    { saleList = []
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
        if canPurchase model 
           then
            { model |
                sales <- model.sales + 1,
                revenue <- model.revenue + model.price,
                saleList <-
                      (newSale model.price model.timeLeft) :: model.saleList,
                price <- priceTickets model
            }
           else
            model

      Reset -> emptyModel

canPurchase : Model -> Bool
canPurchase model =
  (model.tickets - model.sales > 0)
  &&
  (model.timeLeft > 0)

---- Utility Functions for Updating the Price
--priceTickets : Int -> Int -> Int -> Int -> Int
priceTickets model = 
  if canPrice model then makePrice model else model.price

canPrice = canPurchase
  
makePrice model = 
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
  div [ A.class "global"] [
    div
      [ ]
      [ section
          [ A.id "todoapp" ]
          [ myHeader 
          ]
      ],
      div [A.class "div-class", A.id "top"] 
        [
          lazy inputForm model,
          lazy buttonEntry model 
        ],
      div [A.class "div-class", A.id "bottom"] 
        [
          lazy stateEntry model,
          lazy salesTable model
        ] 
      ]

inputEntry : Model -> Html
inputEntry model =
   section 
      [A.class "entry", A.id "inputs" ]
      [p [A.id "myP"] [text instructions]]

instructions = """
This is a simplified, sped up demonstration of the ticket
pricing algorithm.
"""

inputForm : Model -> Html  
inputForm model = 
   section 
      [A.class "entry", A.id "inputs" ]
      [ 
        inputCreator "Starting Price" model.tickets  
      , inputCreator "Tickets Available" model.price
      , inputCreator "Max Discount" model.price
      , inputCreator "Max Markup" model.price
      , inputCreator "Length of Sale" model.price
      ]
      

inputCreator : String -> Int -> Html
inputCreator str int =
  div [A.class "input-div"]
    [ text str
    , input 
        [
          A.id "myinput"
        , A.type' "number" 
        , A.step 1 
        , A.min "0" 
        , A.max "1000"
        ] 
        []
    ]

buttonEntry : Model -> Html
buttonEntry model =
   section 
      [A.class "entry", A.id "buttons" ]
      [ button
          [ A.class "clear-completed"
          , A.id "clear-completed"
          , A.hidden (model.tickets - model.sales <= 0 
                || model.timeLeft <= 0)
          , onClick (Signal.send updates MakePurchase)
          ]
          [ text ("Purchase") ]
      , button
          [ A.class "clear-completed"
          , A.id "clear-completed"
          , onClick (Signal.send updates Reset)
          ]
          [ text ("Reset") ]
      ]

stateEntry : Model -> Html
stateEntry model =
   section 
      [A.class "entry", A.id "states" ]
      [
        p [] [ text ("price: " ++ (toString model.price)) ]
      , p [] [ text ("sales: " ++ (toString model.sales)) ]
      , p [] [ text ("revenue: " ++ (toString model.revenue)) ]
      , p [] [ text ("timeLeft: " ++ (toString model.timeLeft)) ]
      , p [] [ text ("ticketsLeft: " ++
          (toString (model.tickets - model.sales))) ]
      ]
      
salesTable: Model -> Html
salesTable model = 
   section 
      [A.class "entry", A.id "sales" ]
      [
        table []
          [ 
            caption []
              [
                text "Ticket Sales"
              ],
            thead [] --represents the block of rows that
              --describes column summaries of the table
              [
                tr [] -- the row with column headers
                [
                  td [] [text "Price"],
                  td [] [text "Seconds Left"]
                ]
              ]
          , salesToRows model.saleList
          ] 
      ]

salesToRows : List Sale -> Html
salesToRows sales =
  tbody []
  (List.map saleToRow sales)

saleToRow : Sale -> Html
saleToRow sale = 
  tr []
  [
    td [] [text (toString sale.price)   ],
    td [] [text (toString sale.timeLeft)]
  ]

myHeader : Html
myHeader =
      h1 [] [ text "Pricefly"]

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
