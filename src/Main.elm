import Html exposing (..)
import Html.App as App
import List exposing (..)
import Time exposing (Time, second)
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Debug
import Random


type alias Cell =
    { x : Int
    , y : Int
    }

type alias Dimension =
    { x : Int
    , y : Int
    }

type alias Density = Int

type alias Area = Int

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
    , tick_interval : Int
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

dupes : Cells -> Int
dupes cells =
    List.length (cells) - List.length (undupe cells)

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

        -- only a dead cell with alive neighbours can be born
        -- but every dead cell has possibly up to eight alive neighbours
        -- therefore the dupes :-/
        dead = undupe <|
            List.concatMap (\cell ->
                dead_neighbours cell universe
            ) universe

        born =
            List.filter (\cell ->
                List.member (List.length (alive_neighbours cell universe)) rules.born
            ) dead

    -- in sortWith (\a b -> [a b]) born ++ stay
    in List.append stay born

randomCells : Random.Generator ( Int, Int ) -> Random.Seed -> number -> List Cell
randomCells generator seed count =
    case count of
        0 ->
            []
        _ ->
            let
                rand = Random.step generator seed
                ints = fst rand
                seed' = snd rand
            in
                Cell (fst ints) (snd ints) :: (randomCells generator seed' (count - 1))

-- density .. density of population in % of whole universe
--            (100 % means every cell is alive)
-- area ..... % of universe's area populated
--
bigBang : Dimension -> Density -> Area -> Universe
bigBang dimension density area =
    let
        cell_count =
            dimension.x * dimension.y * density // 100
        x_range =
            { upper = dimension.x // 2 - (dimension.x * area // 100 // 2)
            , lower = dimension.x // 2 + (dimension.x * area // 100 // 2)
            }
        y_range =
            { upper = dimension.y // 2 - (dimension.y * area // 100 // 2)
            , lower = dimension.y // 2 + (dimension.y * area // 100 // 2)
            }
        seed =
            Random.initialSeed 73843987428974   -- use current time dummy ;-)
        generator =
            Random.pair
                (Random.int x_range.lower x_range.upper)
                (Random.int y_range.lower y_range.upper)
    in
        randomCells generator seed cell_count -- List ((Int, Int), seed)

init : (Model, Cmd Msg)
init =
    let
        density  = 2
        area     = 20
        universe = bigBang (Dimension 50 50) density area
        rules    = Rules [3] [2, 3]
        tick     = 0
        tick_interval = 250

    in (Model universe rules tick tick_interval, Cmd.none)


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
  Time.every (toFloat (model.tick_interval) * Time.millisecond) Tick


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
