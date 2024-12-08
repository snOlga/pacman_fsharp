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
        Score: int
    }

let initGame = 
    {
        Maze = Maze.MazeMatrix
        PlayerPosition = {X = 0; Y = 0}
        Score = 0
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
    | Direction.Up when position.Y > 0 && 
        Maze.MazeMatrix[position.Y-1][position.X] <> Maze.WALL -> 
        { position with Y = position.Y-1 } // inversion of y
    | Direction.Left when position.X > 0 && 
        Maze.MazeMatrix[position.Y][position.X-1] <> Maze.WALL -> 
        { position with X = position.X-1 }
    | Direction.Down when position.Y < (Maze.MazeMatrix.Length - 1) && 
        Maze.MazeMatrix[position.Y+1][position.X] <> Maze.WALL -> 
        { position with Y = position.Y+1 }
    | Direction.Right when position.Y < (Maze.MazeMatrix.Length - 1) && 
        Maze.MazeMatrix[position.Y][position.X+1] <> Maze.WALL -> 
        { position with X = position.X+1 }
    | _ -> position

let eatApple (gameState:State) =
    let playerX = gameState.PlayerPosition.X
    let playerY = gameState.PlayerPosition.Y
    if gameState.Maze[playerY][playerX] = Maze.APPL then
        gameState.Maze |> Array.mapi (fun i line -> 
            if i = playerY then 
                line |> Array.mapi (fun j chr -> if j = playerX then Maze.EMPT else chr)
            else
                line)
    else
        gameState.Maze

let rec run (gameState:State) =
    Thread.Sleep(100)
    Console.Clear()
    printField gameState
    let inputDirection = getDirectionInput ()
    run { gameState with PlayerPosition = (moveSomeone gameState.PlayerPosition inputDirection); Maze = eatApple gameState}
