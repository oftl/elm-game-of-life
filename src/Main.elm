import Html exposing (..)
import Html.App as App
import List exposing (..)
import Time exposing (Time, second)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug


type alias Cell =
    { x : Int
    , y : Int
    }

type alias Cells = List Cell

type alias Universe = List Cell

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


-- remove duplicates from list
--
undupe : Cells -> Cells
undupe cells =
    List.foldl (\cell acc ->
        if not (List.member cell acc) then
            cell :: acc
        else
            acc
    ) [] cells


-- list of a cells' immediate neighbours
--
neighbours : Cell -> Cells
neighbours cell =
    let
        offsets =
            [ (-1, -1)
            , (-1,  0)
            , (-1,  1)
            , ( 0, -1)
         -- , ( 0,  0)
            , ( 0,  1)
            , ( 1, -1)
            , ( 1,  0)
            , ( 1,  1)
            ]
    in
        List.map (\(x, y) ->
            Cell (x + cell.x) (y + cell.y) -- -1 -1
        ) offsets


-- minuend - subtrahend = difference
-- 1,2,3 minus 2 = 1,3
-- a not in b
--
-- minus : Cells -> Cells -> Cells
-- minus minuend subtrahend =
--     List.filter (\cell ->
--         not <| List.member cell subtrahend
--     ) minuend


-- intersection : Cells -> Cells -> Cells
-- intersection a b =
--     List.filter (\cell ->
--         List.member cell b
--     ) a


alive_neighbours : Cell -> Universe -> Cells
alive_neighbours cell universe =
    List.filter (\cell ->
        List.member cell universe
    ) <| neighbours cell


dead_neighbours : Cell -> Universe -> Cells
dead_neighbours cell universe =
    List.filter (\cell ->
        not <| List.member cell universe
    ) <| neighbours cell

evolve : Rules -> Universe -> Universe
evolve rules universe =
    let
        stay =
            List.filter (\cell ->
                List.member (List.length (alive_neighbours cell universe)) rules.stay
            ) universe

        dead = undupe (
            List.concatMap (\cell ->
                dead_neighbours cell universe
            ) universe
        )

        born =
            List.filter (\cell ->
                List.member (List.length (alive_neighbours cell universe)) rules.born
            ) dead

    -- in sortWith (\a b -> [a b]) born ++ stay
    in List.append stay (Debug.log "born" born)

init : (Model, Cmd Msg)
init =
    let
        cell1 = Cell 5 5    -- start of with a simple blinker to test
        cell2 = Cell 5 6
        cell3 = Cell 5 7
        universe = [ cell1, cell2, cell3 ]
        rules    = Rules [3] [2, 3]
        tick     = 0

    in (Model universe rules tick, Cmd.none)


logUniverse : Universe -> Universe
logUniverse universe = Debug.log "universe" universe


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            let
                m = { model
                    -- | universe = logUniverse <| evolve model.rules model.universe
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
