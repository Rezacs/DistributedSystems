package com.example;

import kong.unirest.Unirest;
import kong.unirest.HttpResponse;
import static spark.Spark.*;
import java.util.*;
import com.google.gson.Gson;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;


public class GameServer {

    private static String serverId = "srv1";
    private static int port = 8081;
    private static String coordinatorUrl = "http://localhost:8080";
    private static String host = "127.0.0.1";

    // Represents a single checkers game
    static class CheckersGame {
        public String gameId;       // <-- Declare gameId here!
        public String player1;
        public String player2;
        public String[][] board = new String[8][8];
        public String currentTurn;

        public CheckersGame(String gameId, String player1) {
            this.gameId = gameId;   // <-- Initialize gameId from constructor parameter!
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
            // Only allow a new player if player2 slot is open
            if (player2 == null && !player.equals(player1)) {
                player2 = player;
                return true;
            }
            // If both slots are filled, do NOT allow joining at all
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

            boolean moved = false;

            // Simple move: single diagonal
            if (board[toRow][toCol].equals("empty") &&
                Math.abs(toRow - fromRow) == 1 &&
                Math.abs(toCol - fromCol) == 1) {
                board[fromRow][fromCol] = "empty";
                board[toRow][toCol] = piece;
                moved = true;
            }

            // Capture move: jump over enemy piece
            if (!moved &&
                board[toRow][toCol].equals("empty") &&
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
                    moved = true;
                }
            }

            if (moved) {
                // Switch turn
                currentTurn = player.equals(player1) ? player2 : player1;

                // 🔁 Save game state after move
                try {
                    String gameJson = serializeGameState(); // implement this method
                    Unirest.post("http://localhost:8080/savegame")
                        .header("Content-Type", "application/json")
                        .body(gameJson)
                        .asString();
                } catch (Exception e) {
                    e.printStackTrace(); // optionally handle error
                }

                return true;
            }

            return false;
        }

        public String serializeGameState() throws JsonProcessingException {
            ObjectMapper mapper = new ObjectMapper();
            Map<String, Object> gameData = new HashMap<>();
            gameData.put("game_id", gameId);       // must include game_id
            gameData.put("board", board);          // 2D array of board state
            gameData.put("current_turn", currentTurn);
            // Add other fields if necessary
            return mapper.writeValueAsString(gameData);
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
        if (args.length > 0) port = Integer.parseInt(args[0]);
        if (args.length > 1) serverId = args[1];
        port(port);
        System.out.println("Starting server " + serverId + " on port " + port);

        // Register with Erlang coordinator on startup
        HttpResponse<String> regResp = Unirest.post(coordinatorUrl + "/register")
            .header("Content-Type", "application/json")
            .body("{\"server_id\":\"" + serverId + "\",\"host\":\"" + host + "\",\"port\":" + port + "}")
            .asString();
        System.out.println("Register response: " + regResp.getStatus() + " " + regResp.getBody());

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

            if (player == null || player.trim().isEmpty()) {
                return gson.toJson(Collections.singletonMap("error", "Name required"));
            }

            CheckersGame game = games.get(gameId);

            // Strictly prevent new joins if both slots are filled and name doesn't match
            if (game != null) {
                boolean canJoin = game.join(player);
                if (canJoin) {
                    return gson.toJson(Collections.singletonMap("status", "joined"));
                } else if (game.player1 != null && game.player2 != null) {
                    // Both slots filled, and name doesn't match
                    return gson.toJson(Collections.singletonMap("error", "Game full: cannot join as a third player"));
                } else {
                    return gson.toJson(Collections.singletonMap("error", "Cannot join"));
                }
            } else {
                return gson.toJson(Collections.singletonMap("error", "Game not found"));
            }
        });




        // Get current game state
        get("/gamestate/:gameId", (req, res) -> {
            String gameId = req.params("gameId");
            String player = req.queryParams("player"); // Require player name!
            CheckersGame game = games.get(gameId);

            if (game == null) {
                return gson.toJson(Collections.singletonMap("error", "Not found"));
            }

            // Only allow access if requester is player1 or player2
            if (player == null || !(player.equals(game.player1) || player.equals(game.player2))) {
                return gson.toJson(Collections.singletonMap("error", "You are not a player in this game!"));
            }

            // Return game state
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
            return gson.toJson(state);
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

        // Start heartbeat thread
        new Thread(() -> {
            while (true) {
                try { Thread.sleep(5000); } catch (InterruptedException e) {}
                HttpResponse<String> hbResp = Unirest.post(coordinatorUrl + "/heartbeat")
                    .header("Content-Type", "application/json")
                    .body("{\"server_id\":\"" + serverId + "\"}")
                    .asString();
                System.out.println("Heartbeat response: " + hbResp.getStatus() + " " + hbResp.getBody());
            }
        }).start();


    }
}

