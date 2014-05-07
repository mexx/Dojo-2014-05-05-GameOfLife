module GameOfLife

open FsUnit.Xunit
open Xunit

type Cell = Alive | Dead_

type Field = Cell [] []

let generationOne : Field =
    [|
        [|Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_|]
        [|Dead_; Dead_; Dead_; Dead_; Alive; Dead_; Dead_; Dead_|]
        [|Dead_; Dead_; Dead_; Alive; Alive; Dead_; Dead_; Dead_|]
        [|Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_|]
    |]

let generationTwo =
    [|
        [|Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_|]
        [|Dead_; Dead_; Dead_; Alive; Alive; Dead_; Dead_; Dead_|]
        [|Dead_; Dead_; Dead_; Alive; Alive; Dead_; Dead_; Dead_|]
        [|Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_; Dead_|]
    |]

let read filePath =
    let deadOrAlive char =
        match char with
        | '.' -> Dead_
        | '*' -> Alive

    System.IO.File.ReadAllLines(filePath)
    |> Seq.skip 1
    |> Seq.map (Seq.map deadOrAlive >> Array.ofSeq)
    |> Array.ofSeq

module GameOfLife =
    let nextGeneration (source: Field) =
        let rowCount = source.Length
        let columnCount = source.[0].Length
        let field =
            Array.init (rowCount + 2) (fun rowIndex ->
                let row = Array.init (columnCount + 2) (fun columnIndex -> Dead_)
                if rowIndex <> 0 && rowIndex <> (rowCount + 1) then
                    Array.blit (source.[rowIndex - 1]) 0 row 1 columnCount
                row
            )

        let neighbours rowIndex columnIndex =
            seq {
                yield field.[rowIndex - 1].[columnIndex - 1]; yield field.[rowIndex - 1].[columnIndex]; yield field.[rowIndex - 1].[columnIndex + 1]
                yield field.[rowIndex]    .[columnIndex - 1];   (*cell;*)                               yield field.[rowIndex]    .[columnIndex + 1]
                yield field.[rowIndex + 1].[columnIndex - 1]; yield field.[rowIndex + 1].[columnIndex]; yield field.[rowIndex + 1].[columnIndex + 1]
            }

        let nextGenForCell rowIndex columnIndex cell =
            let aliveNeigbourCount =
                neighbours (rowIndex + 1) (columnIndex + 1)
                |> Seq.filter ((=) Alive)
                |> Seq.length

            match aliveNeigbourCount, cell with
            | 2, Alive
            | 3, _ -> Alive
            | _, _ -> Dead_

        source
        |> Array.mapi (fun rowIndex row -> row |> Array.mapi (fun columnIndex cell -> nextGenForCell rowIndex columnIndex cell))







[<Fact>]
let OneByOneDeadMatrixStaysDead ()=
    [|[|Dead_|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Dead_|]|]













[<Fact>]
let OneByOneAliveMatrixBecomesDead ()=
    [|[|Alive|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Dead_|]|]







[<Fact>]
let OneByTwoDeadMatrixStaysDead ()=
    [|[|Dead_; Dead_|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Dead_; Dead_|]|]







[<Fact>]
let TwoByOneDeadMatrixStaysDead ()=
    [|[|Dead_|]
      [|Dead_|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Dead_|];[|Dead_|]|]







[<Fact>]
let TwoByTwoAliveMatrixStaysAlive ()=
    [|[|Alive; Alive|]
      [|Alive; Alive|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Alive; Alive|];[|Alive; Alive|]|]








[<Fact>]
let TwoByTwoGliderMatrixBecomesBlock ()=
    [|[|Dead_; Alive|]
      [|Alive; Alive|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Alive; Alive|];[|Alive; Alive|]|]







[<Fact>]
let TwoByTwoHalfAliveMatrixBecomesDead ()=
    [|[|Dead_; Alive|]
      [|Dead_; Alive|]|]
    |> GameOfLife.nextGeneration
    |> should equal [|[|Dead_; Dead_|];[|Dead_; Dead_|]|]








[<Fact>]
let GenerationOneMatrixBecomesGenerationTwo ()=
    generationOne
    |> GameOfLife.nextGeneration
    |> should equal generationTwo










[<Fact>]
let GenerationOneAsFileBecomesGenerationTwo ()=
    "Generation1.txt"
    |> read
    |> GameOfLife.nextGeneration
    |> should equal generationTwo


