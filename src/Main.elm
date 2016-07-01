import Html exposing (..)
import Html.App as App

import List exposing (..)
import Time exposing (Time, second)
import Random

import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Cell =
    { x : Int
    , y : Int
    , n : Int  -- count of neighbour
    , t : Int  -- tick when last changed
    }

type alias Universe = List Cell
type alias NeighbourCount = List Cell

type alias Rules =
    { born : List Int
    , stay : List Int
    }

type alias Model =
    { universe  : Universe
    , rules     : Rules
    , tick      : Int
    }

type Msg = Tick Time

main = App.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }


evolve : Rules -> Universe -> Universe
evolve rules universe =
    List.map (\cell ->
        { cell | x = cell.x + 1, y = cell.y + 1}
    ) universe


init : (Model, Cmd Msg)
init =
    let
        cell1 = Cell 5 5 1 0
        cell2 = Cell 5 6 2 0
        cell3 = Cell 5 7 1 0
        universe = [ cell1, cell2, cell3 ]
        rules    = Rules [3] [2, 3]
        tick     = 0

    in (Model universe rules tick, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            let
                m = { model
                    | universe = evolve model.rules model.universe
                    , tick = model.tick + 1
                    }
            in (m, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


view : Model -> Html Msg
view model =

    let
        cell_width = 2
        cell_height = 2
        cell_radius = 1
        cells = List.map (\cell ->
            Svg.circle
                [ cx <| toString <| cell.x * cell_width + cell_width // 2
                , cy <| toString <| cell.y * cell_height + cell_height // 2
                , r <| toString cell_radius
                ]
                []
        ) model.universe
    in
        Svg.svg
            [ width "640", height "480", viewBox "0 0 120 120", color "yellow" ]
            cells
