namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            
            let rec MiniMax game perspective =
                NodeCounter.Increment()
                
                // Check if the game is over
                if(gameOver game) 
                 // No move possible, return relative score
                then (None, heuristic game perspective)     
                else     
                      
                       // Create children nodes by passing move genrator to apply move
                       // Store as tuple to store move that led to child node
                       let childNodes = moveGenerator game 
                                        |> Seq.map (fun move -> (move,(applyMove game move)))

                       // A sequence of a tuple of a tuple storing the move that led
                       // to the child and the tuple received from Minimaxed child node
                       let results = childNodes 
                                     |> Seq.map(fun tuple -> (fst tuple, MiniMax (snd tuple) perspective))

                       // Check if maximising or minimising
                       if (getTurn game = perspective) 
                       then // Maximising

                            // Access the tuple with the highest score by
                            // sorting using a lambda function that accesses
                            // each child's heuristic
                            let bestTuple = results 
                                            |> Seq.maxBy (fun (a, (b,c)) -> c)
                            // Store best move and score for clarity
                            let bestMove  = fst bestTuple
                            let bestScore = bestTuple 
                                            |> snd 
                                            |> snd
                            (Some(bestMove), bestScore)  // Return Value
                        
                       else //Minimizing

                            // Access the tuple with the lowest score by
                            // sorting using a lambda function that accesses
                            // each child's heuristic
                            let bestTuple = results |> Seq.minBy (fun (a, (b,c)) -> c)
                            // Store best move and score for clarity
                            let bestMove  = fst bestTuple
                            let bestScore = bestTuple 
                                            |> snd 
                                            |> snd
                            (Some(bestMove), bestScore) // Return Value
                       

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                
                // Check if the game is over
                if(gameOver oldState) 
                 // No move possible, return relative score
                then (None, heuristic oldState perspective)     
                else     

                       // Create children nodes by passing move genrator apply move
                       // Store as tuple to store move that led to child node
                       let childNodes = moveGenerator oldState 
                                        |> Seq.map (fun move -> (move,(applyMove oldState move)))

                       // Check if maximising or minimising
                       if (getTurn oldState = perspective) 
                       then // Recursive function to prune from Maximising perspective
                            let rec alphaPruning alpha beta (childrens: seq<'Move*'Game>) perspective currentBest =
                                
                                // Convenient function for finding best value
                                let findMax currentBest childNode =
                                    // Only return next child if better not equal
                                    if( (snd currentBest < snd childNode))
                                    then childNode
                                    else currentBest

                                // Store the heuristic and the move that leads to it
                                let childValue = (Some(Seq.head(childrens)|> fst), MiniMax alpha beta (Seq.head childrens |> snd) perspective |> snd)
                                let trueBest = findMax currentBest childValue
                                let newAlpha = max alpha (snd trueBest)
                                // If statemetnt to exit recursion
                                if ((beta <= newAlpha) || Seq.isEmpty (Seq.tail childrens) ) 
                                then trueBest
                                else alphaPruning newAlpha beta (Seq.tail childrens) perspective trueBest 
                            
                            // Create a generic option for starting point with reasonably large starting number
                            let startingPoint = (Some(Seq.head(moveGenerator oldState)),-100000)   
                            let result = alphaPruning alpha beta childNodes perspective startingPoint                          
                            result
                        
                       else //Recursive function to prune from Minmising perspective
                            let rec betaPruning alpha beta (childrens: seq<'Move*'Game>) perspective currentBest =
                                
                                // Convenient function for finding best value
                                let findMin currentBest childNode =
                                    if(snd currentBest > snd childNode)
                                    then childNode
                                    else currentBest

                                // Store the heuristic and the move that leads to it
                                let childValue = (Some(Seq.head(childrens)|> fst), MiniMax alpha beta (Seq.head childrens |> snd) perspective |> snd)
                                let trueBest = findMin currentBest childValue
                                let newBeta = min beta (snd trueBest)
                                // If statemetnt to exit recursion
                                if ((newBeta <= alpha) || Seq.isEmpty (Seq.tail childrens) ) 
                                then trueBest
                                else betaPruning alpha newBeta (Seq.tail childrens) perspective trueBest
                            
                            // Create a generic option for starting point with reasonably large starting number
                            let startingPoint = (Some(Seq.head(moveGenerator oldState)),100000) 
                            let result = betaPruning alpha beta childNodes perspective startingPoint
                            result

            NodeCounter.Reset()
            MiniMax