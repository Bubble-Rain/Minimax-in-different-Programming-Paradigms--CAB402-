namespace QUT

    module FSharpImpureTicTacToeModel =
        
        open System.Collections.Generic

        type Player = Nought | Cross


        // Advantage of dictionary is similar syntax to Pure
        type GameState = 
            { mutable player: Player ; size:int ; board: Dictionary<int*int,Player> } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.player
                member this.Size with get()    = this.size
                member this.getPiece(row, col) =  match this.board.ContainsKey((row,col)) with
                                                 | true -> match this.board.Item((row,col)) with
                                                           | Nought -> "O" 
                                                           | Cross -> "X"
                                                 | _ -> ""

        type Move = 
            { row: int ; col : int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        let ApplyMove game move  =  match game.player with
                                    | Nought -> game.board.Add((move.row,move.col), Nought)
                                                game.player <- Cross
                                                game
                                    | Cross -> game.board.Add((move.row,move.col), Cross)
                                               game.player <- Nought
                                               game

        let UndoMove game move  =  game.board.Remove(move.row,move.col) |> ignore
                                   match game.player with
                                   | Nought -> game.player <- Cross                                               
                                   | Cross -> game.player <- Nought
                  
                                   

                                                

        let CreateMove row col   = {row = row; col = col}

        let GameStart first size = {player = first; size = size; board = new Dictionary<int*int,Player>()}

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
            let rowlines = seq {for x in 0..size-1 do yield rowLine x}
            let colLines = seq {for y in 0..size-1 do yield colLine y}
            // Combine it all in one sequence
            let final = Seq.append rowlines colLines |> Seq.append diagonals
            final

        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
                // Gather all players in the board regardless of type
                let players = seq { for single in line do if game.board.ContainsKey(single) then yield game.board.Item(single) }
                // Count the number of cross and noughts using filter
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

        
        let getTurn game = game.player

        let heuristic game perspective = 
                      let player = getTurn game
                      let outcome = GameOutcome game
                      if (outcome = Draw)
                      then 0
                      // This check the "turn" after the game ended
                      else if(player = perspective)
                           then -1 
                           else 1

        let gameOver game = let outcome = GameOutcome game 
                            match outcome with      
                            | Undecided -> false
                            | _ -> true

        let moveGenarator game = // Generate sequence containing all moves
                                 let allMoves = seq {for x in 0..game.size-1 do for y in 0..game.size-1 do yield (x,y)}
                                 // Filter out spaces already taken
                                 let usableMoves = allMoves 
                                                   |> Seq.filter (fun coOrd -> not(game.board.ContainsKey(coOrd))) 
                                                   |> Seq.map (fun tuple -> CreateMove (fst tuple) (snd tuple)) 
                                                   |> Seq.toArray // Convert to array for alpha beta
                                 usableMoves

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->'Move[]) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()

                // Store the changing alpha and beta
                let mutable newAlpha = alpha
                let mutable newBeta = beta
                
                // Check if the game is over
                if(gameOver oldState) 
                 // No move possible, return relative score
                then (None, heuristic oldState perspective)     
                else     

                       // Create Array of moves
                       let mutable usableMoves = moveGenarator oldState
                       // boolean for while loop
                       let mutable continueLoop = true
                       // counter for while loop
                       let mutable counter = 0
                       // A generic move to start with
                       let genericOption = Some(usableMoves.[0])

                       // Check if maximising or minimising
                       if (getTurn oldState = perspective) 
                       then
                            // Starting point with reasonably large number
                            let mutable currentBest = (genericOption, -10000)            
                            // Convenient function for finding best value
                            let findMax currentBest childNode =
                                 // Only return next child if better not equal
                                 if( (snd currentBest < snd childNode))
                                 then childNode
                                 else currentBest
                 
                            while continueLoop do 
   
                                let newState = applyMove oldState usableMoves.[counter]
                                // Store the heuristic and the move that leads to it
                                let childValue = (Some(usableMoves.[counter]), MiniMax newAlpha beta newState perspective |> snd)
                                // Undo move so that the apply move isn't permanent
                                UndoMove oldState usableMoves.[counter]
                                currentBest <- findMax currentBest childValue
                                newAlpha <- max alpha (snd currentBest)
                                counter <- counter+1
                                if ((newBeta <= newAlpha) || counter = usableMoves.Length ) 
                                then continueLoop <- false

                            currentBest
                        
                       else
                            // Starting point with reasonably large number
                            let mutable currentBest = (genericOption, 10000)
                            // Convenient function for finding best value
                            let findMin currentBest childNode =
                                // Only return next child if better not equal
                                if( (snd currentBest > snd childNode))
                                then childNode
                                else currentBest

                            while continueLoop do 
                                
                                let newState = applyMove oldState usableMoves.[counter]
                                
                                // Store the best child and the move that leads to it
                                let childValue = (Some(usableMoves.[counter]), MiniMax alpha newBeta newState perspective |> snd)
                                // Undo move so that the apply move isn't permanent
                                UndoMove oldState usableMoves.[counter]
                                currentBest <- findMin currentBest childValue
                                newBeta <- min beta (snd currentBest)
                                counter <- counter+1
                                if ((newBeta <= newAlpha) || counter = usableMoves.Length ) 
                                then continueLoop <- false

                            currentBest

            NodeCounter.Reset()
            MiniMax

        let FindBestMove game    = MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenarator ApplyMove -1 1 game game.player

        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game move
                member this.FindBestMove(game)           = fst(FindBestMove game).Value