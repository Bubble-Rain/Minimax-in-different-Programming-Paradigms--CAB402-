using System;
using System.Collections.Generic;
namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.CROSS;
        public Player Nought => Player.NOUGHT;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }
        public Game ApplyMove(Game game, Move move)
        {
            // Update board and current turn
            game.addPiece(move);
            game.nextTurn();
            return game;

        }

        public Move CreateMove(int row, int col)
        {
            Move move = new Move(row, col);
            return move;
        }

        public Move FindBestMove(Game game)
        {
            Player perspective = game.Turn;
            int alpha = -1;
            int beta = 1;
            NodeCounter.Reset();
            (Move, int) bestMove = miniMaxGenerator(alpha,beta,game, perspective);       
            return bestMove.Item1;

        }

        // Note to tutor: Placed in one function 
        // to make finding Win and draw convenient
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {

            // Check how many lines have draws
            int numDraw = 0;
            // Define variables for reuse
            int countN = 0; // Count Noughts
            int countC = 0; // Count Crosses
            List<Tuple<int, int>> winningLine = new List<Tuple<int, int>>(); // Assume a winning line

            //Horizontal Lines
            // For each row
            for (int x = 0; x < game.Size; x++)
            {
                countN = 0;
                countC = 0;
                winningLine = new List<Tuple<int, int>>();
                // For each column
                for (int y = 0; y < game.Size; y++)
                {
                    Tuple<int, int> coOrd = Tuple.Create<int, int>(x, y);
                    winningLine.Add(coOrd);
                    if (game.getPiece(x, y) == "X")
                    {
                        countC++;
                    }
                    else if (game.getPiece(x, y) == "O")
                    {
                        countN++;
                    }

                }

                // Check for wins and draw
                if (countN == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.NOUGHT, winningLine);
                }
                else if (countC == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.CROSS, winningLine);
                }
                else if ((countC > 0) && (countN > 0))
                {
                    numDraw++;
                }

            }

            //Vertical Lines
            // For each column
            for (int y = 0; y < game.Size; y++)
            {
                countN = 0;
                countC = 0;
                winningLine = new List<Tuple<int, int>>();
                // For each row
                for (int x = 0; x < game.Size; x++)
                {
                    Tuple<int, int> coOrd = Tuple.Create<int, int>(x, y);
                    winningLine.Add(coOrd);
                    if (game.getPiece(x, y) == "X")
                    {
                        countC++;
                    }
                    else if (game.getPiece(x, y) == "O")
                    {
                        countN++;
                    }

                }

                // Check for wins and draw
                if (countN == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.NOUGHT, winningLine);
                }
                else if (countC == game.Size)
                {
                    return TicTacToeOutcome<Player>.NewWin(Player.CROSS, winningLine);
                }
                else if ((countC > 0) && (countN > 0))
                {
                    numDraw++;
                }

            }

            // Check Top Left Diagonal
            countN = 0;
            countC = 0;
            winningLine = new List<Tuple<int, int>>();
            // Top left diagonal is where both co-ordinates are thee same
            for (int x = 0; x < game.Size; x++)
            {
                Tuple<int, int> coOrd = Tuple.Create<int, int>(x, x);
                winningLine.Add(coOrd);
                if (game.getPiece(x, x) == "X")
                {
                    countC++;

                }
                else if (game.getPiece(x, x) == "O")
                {
                    countN++;
                }

            }

            // Check for wins and draw
            if (countN == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.NOUGHT, winningLine);
            }
            else if (countC == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.CROSS, winningLine);
            }
            else if ((countC > 0) && (countN > 0))
            {
                numDraw++;
            }

            // Top right diagonal
            // Reset Variables
            countN = 0;
            countC = 0;
            winningLine = new List<Tuple<int, int>>();
            for (int x = 0; x < game.Size; x++)
            {
                Tuple<int, int> coOrd = Tuple.Create<int, int>(x, game.Size - 1 - x);
                winningLine.Add(coOrd);
                if (game.getPiece(x, game.Size - 1 - x) == "X")
                {
                    countC++;
                }
                else if (game.getPiece(x, game.Size - 1 - x) == "O")
                {
                    countN++;
                }

            }

            // Check for wins and draw
            if (countN == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.NOUGHT, winningLine);
            }
            else if (countC == game.Size)
            {
                return TicTacToeOutcome<Player>.NewWin(Player.CROSS, winningLine);
            }
            else if ((countC > 0) && (countN > 0))
            {
                numDraw++;
            }

            // Only draws if the number of draws fond
            // equal to number of lines
            if (numDraw == (game.Size * 2 + 2))
            {
                return TicTacToeOutcome<Player>.Draw;
            }

            // Else undecided
            return TicTacToeOutcome<Player>.Undecided;

        }

        // Construct game
        public Game GameStart(Player first, int size)
        {
            // Construct board by filling 
            // every square with empty string
            string[,] board = new string[size, size];
            for (int i = 0; i < size; i++)
            {
                for (int j = 0; j < size; j++)
                {
                    board[i, j] = "";
                }

            }
            Game game = new Game(first, size, board);
            return game;
        }

        // Helper Functions

        public bool gameOver(Game game)
        {
            if (GameOutcome(game) == TicTacToeOutcome<Player>.Undecided)
            {
                return false;
            }
            else
            {
                return true;
            }

        }

        public int heuristic(Game game, Player perspective)
        {

            if (GameOutcome(game) == TicTacToeOutcome<Player>.Draw)
            {
                return 0;
            }
            // This check the "turn" after the game ended
            else if (game.Turn == perspective)
            {
                return -1;
            }
            else
            {
                return 1;
            }

        }

        public List<Move> movegenerator(Game game)
        {
            List<Move> usableMoves = new List<Move>();

            // Check every space in the board
            // for an empty space
            for (int i = 0; i < game.Size; i++)
            {
                for (int j = 0; j < game.Size; j++)
                {
                    if (game.getPiece(i, j) == "")
                    {
                        usableMoves.Add(CreateMove(i, j));
                    }

                }
            }
            // return empty spaces where moves can be placed
            return usableMoves;

        }

        // Optimized MiniMax algorithm that uses alpha beta pruning to 
        // eliminate parts of the search tree that don't need to be explored
        public (Move, int) miniMaxGenerator(int alpha, int beta, Game oldState, Player perspective)
        {
            NodeCounter.Increment();
            // Store the changing alpha and beta
            int newAlpha = alpha;
            int newBeta = beta;

            List<Move> usableMoves = movegenerator(oldState);
            // Store best value and best move sepeartely
            int bestValue = 0;
            Move bestMove = CreateMove(9, 9);

            if (gameOver(oldState))
            {
                return (null, heuristic(oldState, perspective));
            }
            else if (oldState.Turn == perspective)
            {
                // Maximising

                // Assume reasonably large starting value
                bestValue = -10;

                foreach (Move move in usableMoves)
                {
                    Game newState = ApplyMove(oldState, move);  
                    int childValue = miniMaxGenerator(newAlpha, newBeta,newState, perspective).Item2;
                    // Undo move so that the apply move isn't permanent
                    oldState.undoMove(move);

                    if (bestValue < childValue)
                    {
                        bestValue = childValue;
                        bestMove = move;
                    }
                    newAlpha = Math.Max(newAlpha, bestValue);

                    if (beta <= newAlpha)
                        break;
     
                }

                return (bestMove, bestValue);

            }
            else
            {
                // Minimising
                bestValue = 10;


                foreach (Move move in usableMoves)
                {
                    Game newState = ApplyMove(oldState, move);
                    int childValue = miniMaxGenerator(newAlpha, newBeta,newState, perspective).Item2;
                    // Undo move so that the apply move isn't permanent
                    oldState.undoMove(move);
                    if (bestValue > childValue)
                    {
                        bestValue = childValue;
                        bestMove = move;
                    }
                    newBeta = Math.Min(newBeta, bestValue);
                    if (newBeta <= alpha)
                        break;
                }

                return (bestMove, bestValue);

            }
        }
    }
}