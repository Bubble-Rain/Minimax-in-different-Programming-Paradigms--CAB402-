using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        private string[,] board;
        private int size;
        private Player currentPlayer;

        // Constructor for game
        public Game(Player player, int size, string[,] board)
        {
            currentPlayer = player;
            this.size = size;
            this.board = board;
        }

        // Place respective piece in board
        public void addPiece(Move move)
        {
            if (currentPlayer.Equals(Player.CROSS))
            {
                board[move.Row, move.Col] = "X";
            }
            else
            {
                board[move.Row, move.Col] = "O";
            }

        }

        // Extra function for clarity
        public void nextTurn()
        {
            if (currentPlayer.Equals(Player.CROSS))
            {
                currentPlayer = Player.NOUGHT;
            }
            else
            {
                currentPlayer = Player.CROSS;
            }

        }

        // Undo a given move and go back to previous palyer
        public void undoMove(Move move)
        {

            board[move.Row, move.Col] = "";

            if (currentPlayer.Equals(Player.CROSS))
            {
                currentPlayer = Player.NOUGHT;
            }
            else
            {
                currentPlayer = Player.CROSS;
            }

        }

        public int Size => size;
        public Player Turn => currentPlayer;
        public string getPiece(int row, int col)
        {
            return board[row, col];
        }
    }
}