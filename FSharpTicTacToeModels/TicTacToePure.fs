namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { row: int ; col : int  }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { player: Player ; size:int ; board: Map<int*int,Player> }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.player
                member this.Size with get()    = this.size
                member this.getPiece(row, col) = match this.board.TryFind((row,col)) with
                                                 | Some player -> match player with
                                                                  | Nought -> "O" 
                                                                  | Cross -> "X"
                                                 | _ -> ""


        let CreateMove row col = { row = row; col = col}

        let ApplyMove (oldState:GameState) (move: Move) =   
                      match oldState.player with
                      | Nought -> { player = Cross; size = oldState.size; board = oldState.board.Add( (move.row,move.col),  Nought)}
                      | Cross -> { player = Nought; size = oldState.size; board = oldState.board.Add( (move.row,move.col), Cross)}
                                        

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> = 
            
            // Create functions for row and columns
            let rowLine row = seq {for y in 0..size-1 do yield (row,y)}
            let colLine col = seq {for x in 0..size-1 do yield (x,col)}
            // Setup diagonal lines
            let diagLeft = seq {for x in 0..size-1 do yield (x,x)}
            let diagRight = seq {for x in 0..size-1 do yield (x,size-1-x)}
            let diagonals = seq {yield diagLeft
                                 yield diagRight}
            //Construct all rows and columns
            let rowLines = seq {for x in 0..size-1 do yield rowLine x}
            let colLines = seq {for y in 0..size-1 do yield colLine y}
            // Combine it all in one sequence
            let final = Seq.append rowLines colLines |> Seq.append diagonals
            final



        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
                // Gather all players in the board regardless of type
                let players = seq { for single in line do if game.board.ContainsKey(single) then yield game.board.Item(single) }
                // Count the respective number of cross and noughts by filtering
                let countN  = players 
                              |> Seq.filter (fun player -> player = Nought) 
                              |> Seq.length
                let countC  = players 
                              |> Seq.filter (fun player -> player = Cross) 
                              |> Seq.length
                // Win if either counters equal to game size
                if (countN = game.size) then
                     Win(Nought,line)
                     elif (countC = game.size)
                           then Win(Cross,line) 
                           elif (countN > 0) && (countC > 0)  // Draw if both greater than 0
                                then Draw
                                else Undecided
                     

        let GameOutcome game = 
                
                // Create a sequence of outcomes
                let outcome = Lines game.size 
                              |> Seq.map (fun line -> CheckLine game line)
                // Attempt to find a single win
                let checkWin = outcome 
                               |> Seq.tryFind (fun result -> match result with
                                                                      | Win(a,b) -> true
                                                                      | _ -> false   )
                if not(checkWin = None)
                then checkWin.Value
                // Since there are no wins only need to find one undecided 
                else let unDecResult = outcome 
                                       |> Seq.tryFind (fun result -> result = Undecided)
                     if not(unDecResult = None)
                     then Undecided
                     else Draw
                   

        let GameStart (firstPlayer:Player) size = { player = firstPlayer; size = size; board = Map.empty<int*int, Player>}


        // plus other helper functions ...

        let getTurn game = game.player

        let heuristic game perspective = 
                      let player = getTurn game
                      let outcome = GameOutcome game
                      if (outcome = Draw)
                      then 0
                      // This checks the "turn" after the game ended
                      else if(player = perspective)
                           then -1
                           else 1

        let gameOver game = let outcome = GameOutcome game 
                            match outcome with      
                            | Undecided -> false
                            | _ -> true // Covers both win and draw

        let moveGenarator game = // Generate sequence that contain all moves
                                 let allMoves = seq {for x in 0..game.size-1 do for y in 0..game.size-1 do yield (x,y)}
                                 // Filter out spaces already taken
                                 // then convert to move
                                 let usableMoves = allMoves 
                                                   |> Seq.filter (fun coOrd -> not(game.board.ContainsKey(coOrd))) 
                                                   |> Seq.map (fun tuple -> CreateMove (fst tuple) (snd tuple))
                                 usableMoves

        let MiniMax game = GameTheory.MiniMaxGenerator heuristic getTurn gameOver moveGenarator ApplyMove game game.player
  
        
        let MiniMaxWithPruning game = GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenarator ApplyMove -1 1 game game.player

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = fst(MiniMax game).Value
                                               


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = fst(MiniMaxWithPruning game).Value
                                               