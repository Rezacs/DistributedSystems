package com.example;

import static spark.Spark.*;

import java.util.*;
import com.google.gson.Gson;

public class GameServer {

    // Represents a single checkers game
    static class CheckersGame {
        String gameId;
        String player1;
        String player2;
        String[][] board = new String[8][8];
        String currentTurn;

        public CheckersGame(String gameId, String player1) {
            this.gameId = gameId;
            this.player1 = player1;
            this.player2 = null;
            this.currentTurn = player1;
            initBoard();
        }

        // Initializes the board with standard checkers layout
        private void initBoard() {
            for (String[] row : board) Arrays.fill(row, "empty");

            // Black
            for (int i = 0; i < 3; i++)
                for (int j = (i % 2); j < 8; j += 2)
                    board[i][j] = "b";
            // White
            for (int i = 5; i < 8; i++)
                for (int j = (i % 2); j < 8; j += 2)
                    board[i][j] = "w";

        }



        // Attempt to join the game as player2
        public boolean join(String player) {
            if (player.equals(player1) || player.equals(player2)) {
                // Allow rejoining by original players
                return true;
            }
            if (player2 == null && !player.equals(player1)) {
                player2 = player;
                return true;
            }
            return false;
        }


        // Get color for current player ("w" or "b")
        public String getCurrentPlayerColor() {
            if (currentTurn.equals(player1)) return "w";
            if (currentTurn.equals(player2)) return "b";
            return "";
        }

        // Move logic: only allow correct player's turn and a simple legal move
        public boolean move(String player, int fromRow, int fromCol, int toRow, int toCol) {
            String piece = board[fromRow][fromCol];
            if (!player.equals(currentTurn)) return false; // not your turn
            String color = getCurrentPlayerColor();
            if (!piece.equals(color)) return false; // can't move other's piece

            // Simple move: single diagonal
            if (board[toRow][toCol].equals("empty") &&
                Math.abs(toRow - fromRow) == 1 &&
                Math.abs(toCol - fromCol) == 1) {
                board[fromRow][fromCol] = "empty";
                board[toRow][toCol] = piece;
                // Switch turn
                currentTurn = player.equals(player1) ? player2 : player1;
                return true;
            }

            // Capture move: jump over enemy piece
            if (board[toRow][toCol].equals("empty") &&
                Math.abs(toRow - fromRow) == 2 &&
                Math.abs(toCol - fromCol) == 2) {
                int midRow = (fromRow + toRow) / 2;
                int midCol = (fromCol + toCol) / 2;
                String midPiece = board[midRow][midCol];
                if (!midPiece.equals("empty") && !midPiece.equals(color)) {
                    // Jump is legal; remove enemy piece
                    board[fromRow][fromCol] = "empty";
                    board[midRow][midCol] = "empty";
                    board[toRow][toCol] = piece;
                    // Switch turn
                    currentTurn = player.equals(player1) ? player2 : player1;
                    return true;
                }
            }

            return false;
        }
        public int countPieces(String color) {
            int count = 0;
            for (int i = 0; i < 8; i++)
                for (int j = 0; j < 8; j++)
                    if (board[i][j].equals(color)) count++;
            return count;
        }

        public String checkWinner() {
            int white = countPieces("w");
            int black = countPieces("b");
            if (white == 0) return player2; // black wins
            if (black == 0) return player1; // white wins
            return null; // no winner yet
        }
    }

    private static final Map<String, CheckersGame> games = new HashMap<>();
    private static final Gson gson = new Gson();

    public static void main(String[] args) {
        port(8081);

        // Create a new game
        post("/newgame", (req, res) -> {
            Map<String, String> body = gson.fromJson(req.body(), Map.class);
            String player = body.get("player");
            String gameId = UUID.randomUUID().toString();

            games.put(gameId, new CheckersGame(gameId, player));
            return gson.toJson(Collections.singletonMap("gameId", gameId));
        });

        // Join an existing game
        post("/joingame", (req, res) -> {
            Map<String, String> body = gson.fromJson(req.body(), Map.class);
            String gameId = body.get("gameId");
            String player = body.get("player");

            System.out.println("JOIN attempt: player='" + player + "' gameId='" + gameId + "'");

            // Don't allow null/empty names
            if (player == null || player.trim().isEmpty()) {
                return gson.toJson(Collections.singletonMap("error", "Name required"));
            }

            CheckersGame game = games.get(gameId);
            if (game != null && game.join(player)) {
                return gson.toJson(Collections.singletonMap("status", "joined"));
            } else {
                return gson.toJson(Collections.singletonMap("error", "Cannot join"));
            }
        });



        // Get current game state
        get("/gamestate/:gameId", (req, res) -> {
            String gameId = req.params("gameId");
            CheckersGame game = games.get(gameId);

            if (game != null) {
                Map<String, Object> state = new HashMap<>();
                state.put("gameId", game.gameId);
                state.put("player1", game.player1);
                state.put("player2", game.player2);
                state.put("currentTurn", game.currentTurn);
                state.put("board", game.board);
                state.put("whiteScore", game.countPieces("w"));
                state.put("blackScore", game.countPieces("b"));
                String winner = game.checkWinner();
                if (winner != null) state.put("winner", winner);
                return gson.toJson(state); // <--- must be return!
            } else {
                return gson.toJson(Collections.singletonMap("error", "Not found")); // <--- must be return!
            }
        });



        // Handle a move with turn and move validation
        post("/move", (req, res) -> {
            Map<String, Object> body = gson.fromJson(req.body(), Map.class);
            String gameId = (String) body.get("gameId");
            String player = (String) body.get("player");

            int fromRow = ((Double) body.get("fromRow")).intValue();
            int fromCol = ((Double) body.get("fromCol")).intValue();
            int toRow = ((Double) body.get("toRow")).intValue();
            int toCol = ((Double) body.get("toCol")).intValue();

            CheckersGame game = games.get(gameId);
            if (game == null) {
                return gson.toJson(Collections.singletonMap("error", "No game"));
            }

            boolean ok = game.move(player, fromRow, fromCol, toRow, toCol);
            return gson.toJson(Collections.singletonMap("status", ok ? "move ok" : "illegal move or not your turn"));
        });

        get("/games", (req, res) -> {
            List<Map<String, Object>> gameList = new ArrayList<>();
            for (CheckersGame game : games.values()) {
                Map<String, Object> g = new HashMap<>();
                g.put("gameId", game.gameId);
                g.put("player1", game.player1);
                g.put("player2", game.player2);
                // Optionally, show if the game is full or still waiting for player2
                g.put("isOpen", game.player2 == null);
                gameList.add(g);
            }
            return gson.toJson(gameList);
        });

    }
}

