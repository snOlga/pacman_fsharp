module Game
open System
open System.Threading

type Position = { X: int; Y: int }

type Direction =
    | Left
    | Right
    | Up
    | Down
    | None

type State = 
    {
        Maze: string[][]
        PlayerPosition: Position
    }

let initGame = 
    {
        Maze = Maze.mazeMatrix
        PlayerPosition = {X = 0; Y = 0}
    }

let printField (gameState:State) =
    for i = 0 to (gameState.Maze.Length - 1) do
        let line = gameState.Maze[i]
        for j = 0 to (line.Length - 1) do
            let chr = line[j]
            if gameState.PlayerPosition.X = j && gameState.PlayerPosition.Y = i then
                printf "%s" Maze.PLAYER_SYMBOL
            else
                printf "%s" chr
        printfn ""

let getDirectionInput () =
    match Console.ReadKey().Key with
    | ConsoleKey.UpArrow -> Direction.Up
    | ConsoleKey.DownArrow -> Direction.Down
    | ConsoleKey.LeftArrow -> Direction.Left
    | ConsoleKey.RightArrow -> Direction.Right
    | _ -> Direction.None

let moveSomeone position direction =
    match direction with
    | Direction.Up -> {position with Y = position.Y-1} // inversion of y
    | Direction.Left -> {position with X = position.X-1}
    | Direction.Down -> {position with Y = position.Y+1}
    | Direction.Right -> {position with X = position.X+1}
    | Direction.None -> position

let rec run (gameState:State) =
    Thread.Sleep(100)
    Console.Clear()
    printField gameState
    let inputDirection = getDirectionInput ()
    run { gameState with PlayerPosition = (moveSomeone gameState.PlayerPosition inputDirection) }
