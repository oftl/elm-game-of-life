import Html exposing (..)
import Html.App as App
import List exposing (..)
import Time exposing (Time, second)
import Task
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
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

type alias Stats =
    { cnt_stay  : Int
    , cnt_dead  : Int
    , cnt_born  : Int
    , cnt_alive : Int
    }

type alias Model =
    { universe  : Universe
    , rules     : Rules
    , tick      : Int
    , tick_interval : Int
    , stats     : Stats
    }

type alias InitialSeed = Int

type Msg =
      Tick Time
    | ResetUniverse
    | GetSeedError String
    | GetSeedSuccess (InitialSeed -> Universe) Float


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


evolve : Rules -> Universe -> (Universe, Stats)
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

        stats = Stats
            ( List.length stay )
            ( List.length dead )
            ( List.length born )
            ( List.length universe )

        universe = List.append stay born

    in (universe, stats)


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
bigBang : Dimension -> Density -> Area -> Int -> Universe
bigBang dimension density area seed =
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
        seed' =
            Random.initialSeed seed
        generator =
            Random.pair
                (Random.int x_range.lower x_range.upper)
                (Random.int y_range.lower y_range.upper)
    in
        randomCells generator seed' cell_count -- List ((Int, Int), seed)

-- -- explicitly create one piece of glider for showcasing
-- --
-- bigBangGlider : Dimension -> Density -> Area -> Universe
-- bigBangGlider dimension density area =
--     [ Cell 11 10
--     , Cell 12 11
--     , Cell 10 12
--     , Cell 11 12
--     , Cell 12 12
--     ]


init : (Model, Cmd Msg)
init =
    let
        density     = 2
        area        = 20
        dimension   = Dimension 50 50
        rules       = Rules [3] [2, 3]
        tick        = 0
        tick_interval = 250
        callback    = bigBang dimension density area
        stats       = Stats 0 0 0 0
        model       = Model [] rules tick tick_interval stats
    in
        (model, getSeed callback)


getSeed : (InitialSeed -> Universe) -> Cmd Msg
getSeed callback =
    Task.perform GetSeedError (GetSeedSuccess callback) Time.now


toInitialSeed : Float -> InitialSeed
toInitialSeed seed = round seed


logUniverse : Universe -> Universe
logUniverse universe = Debug.log "universe" universe


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            let
                evolve_res = evolve model.rules model.universe
                universe   = fst evolve_res
                stats      = snd evolve_res

                m = { model
                    -- | universe = logUniverse <| evolve model.rules model.universe
                    | universe = universe
                    , stats    = stats
                    , tick     = model.tick + 1
                    }
            in (m, Cmd.none)

        ResetUniverse -> init

        GetSeedError error ->
            Debug.crash error

        GetSeedSuccess callback newSeed ->
            let
                m = { model | universe = callback (toInitialSeed newSeed) }
            in
                (m, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (toFloat (model.tick_interval) * Time.millisecond) Tick


view : Model -> Html Msg
view model =

    let
        cell_width  = 2
        cell_height = 2
        cell_radius = 1

        cells = List.map (\cell ->
            Svg.circle
                [ cx <| toString <| cell.x * cell_width + cell_width // 2
                , cy <| toString <| cell.y * cell_height + cell_height // 2
                , r <| toString cell_radius
                , fill "blue"
                ]
                []
        ) model.universe

        str_stay  = "stay: "  ++ toString model.stats.cnt_stay
        str_born  = "born: "  ++ toString model.stats.cnt_born
        str_dead  = "dead: "  ++ toString model.stats.cnt_dead
        str_alive = "alive: " ++ toString model.stats.cnt_alive

        controls = Svg.svg
            [ width "640", height "100" ]
            --
            -- statistics
            --
            [ Svg.text'
                [ x "250" , y "20" ]
                [ Svg.text str_stay ]
            , Svg.text'
                [ x "250" , y "40" ]
                [ Svg.text str_born ]
            , Svg.text'
                [ x "250" , y "60" ]
                [ Svg.text str_dead ]
            , Svg.text'
                [ x "250" , y "80" ]
                [ Svg.text str_alive ]
            --
            -- user-actions
            --
            , Svg.a
                [ x "0"
                , y "0"
                , Svg.Events.onClick ResetUniverse
                ]
                [ Svg.text'
                    [ fill "blue"
                    , x "250"
                    , y "100"
                    ]
                    [ Svg.text "reset universe " ]
                ]
            ]

        content = cells ++ [ controls ]
    in
        Svg.svg
            -- [ width "250", height "250", viewBox "0 0 100 100" ]
            [ width "350", height "350" ]
            content
