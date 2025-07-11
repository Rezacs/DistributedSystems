package com.example;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import kong.unirest.Unirest;
import com.google.gson.*;
import java.util.Map;
import java.util.HashMap;


public class CheckersClient extends Application {

    private String gameId;
    private String player;
    private int selectedRow = -1, selectedCol = -1; // Tracks the selected "from" cell
    private TextField gameIdField = new TextField();
    private Label turnLabel = new Label("Turn: ");
    private Label winnerLabel = new Label("Winner: ");
    private Label piecesLabel = new Label("Pieces left - W: 12, B: 12");
    private ListView<String> gamesListView = new ListView<>();
    private Map<String, String> gameIdByListItem = new HashMap<>();



    public static void main(String[] args) {
        launch(args);
    }

    @Override
    public void start(Stage primaryStage) {
        // UI Components
        TextField nameField = new TextField();
        Button createButton = new Button("Create Game");
        Button joinButton = new Button("Join Game");
        TextField joinGameField = new TextField();
        Label statusLabel = new Label("Enter your name:");
        GridPane boardGrid = new GridPane();
        boardGrid.setGridLinesVisible(true);

        // Set up gameIdField for copying the gameId
        gameIdField.setEditable(false);
        gameIdField.setPromptText("Game ID will appear here...");

        // 🧩 Function to update the board UI
        final Runnable[] refreshBoard = new Runnable[1];
        refreshBoard[0] = () -> {
            if (gameId == null || gameId.isEmpty()) return;

            var res = Unirest.get("http://localhost:8081/gamestate/" + gameId).asString();
            JsonObject obj = JsonParser.parseString(res.getBody()).getAsJsonObject();

            if (!obj.has("board")) return;

            // Show whose turn it is
            String whoseTurn = obj.has("currentTurn") ? obj.get("currentTurn").getAsString() : "";
            if (!whoseTurn.isEmpty() && player != null) {
                if (player.equals(whoseTurn)) {
                    statusLabel.setText("It's your turn!");
                } else {
                    statusLabel.setText("Waiting for " + whoseTurn + " ...");
                }
            }

            // After updating turnLabel:
            if (obj.has("whiteScore") && obj.has("blackScore")) {
                int w = obj.get("whiteScore").getAsInt();
                int b = obj.get("blackScore").getAsInt();
                piecesLabel.setText("Pieces left - W: " + w + ", B: " + b);

                // New: If any side has less than 6, declare the other side as winner!
                if (w < 6 && b >= 6) {
                    winnerLabel.setText("WINNER: Black (B) 🎉");
                    turnLabel.setText("Game over!");
                } else if (b < 6 && w >= 6) {
                    winnerLabel.setText("WINNER: White (W) 🎉");
                    turnLabel.setText("Game over!");
                }
            }



            boardGrid.getChildren().clear(); // Clear previous buttons

            for (int i = 0; i < 8; i++) {
                for (int j = 0; j < 8; j++) {
                    String cell = obj.getAsJsonArray("board").get(i).getAsJsonArray().get(j).getAsString();
                    Button cellBtn = new Button(cell.equals("empty") ? "" : cell.substring(0, 1).toUpperCase());
                    cellBtn.setPrefSize(32, 32);

                    int fi = i, fj = j;
                    cellBtn.setOnAction(e -> {
                        if (selectedRow == -1 && selectedCol == -1 && !cell.equals("empty")) {
                            // First click: select your own piece
                            selectedRow = fi;
                            selectedCol = fj;
                            statusLabel.setText("Selected from: " + fi + "," + fj);
                        } else if (selectedRow != -1 && selectedCol != -1) {
                            // Second click: select destination and send move
                            int fromRow = selectedRow, fromCol = selectedCol;
                            int toRow = fi, toCol = fj;
                            selectedRow = selectedCol = -1; // Reset

                            // Send move to backend
                            JsonObject move = new JsonObject();
                            move.addProperty("gameId", gameId);
                            move.addProperty("player", player);
                            move.addProperty("fromRow", fromRow);
                            move.addProperty("fromCol", fromCol);
                            move.addProperty("toRow", toRow);
                            move.addProperty("toCol", toCol);
                            var resp = Unirest.post("http://localhost:8081/move")
                                .header("Content-Type", "application/json")
                                .body(move.toString())
                                .asString();
                            statusLabel.setText("Move result: " + resp.getBody());
                            refreshBoard[0].run();
                        }
                    });

                    boardGrid.add(cellBtn, j, i);
                }
            }
        };


        // Auto-refresh board every 2 seconds (for real-time play)
        Thread refresher = new Thread(() -> {
            while (true) {
                try { Thread.sleep(2000); } catch (InterruptedException e) {}
                javafx.application.Platform.runLater(refreshBoard[0]);
            }
        });
        refresher.setDaemon(true);
        refresher.start();

        createButton.setOnAction(e -> {
            player = nameField.getText();
            var res = Unirest.post("http://localhost:8081/newgame")
                    .header("Content-Type", "application/json")
                    .body("{\"player\":\"" + player + "\"}").asString();
            JsonObject obj = JsonParser.parseString(res.getBody()).getAsJsonObject();
            gameId = obj.get("gameId").getAsString();
            statusLabel.setText("Game created! Game ID: " + gameId);
            gameIdField.setText(gameId);   // <-- for copying
            refreshBoard[0].run();
        });

        joinButton.setOnAction(e -> {
            player = nameField.getText();
            gameId = joinGameField.getText();

            if (player == null || player.trim().isEmpty()) {
                statusLabel.setText("Please enter your name before joining!");
                return;
            }
            if (gameId == null || gameId.trim().isEmpty()) {
                statusLabel.setText("Please enter the game ID to join!");
                return;
            }

            var res = Unirest.post("http://localhost:8081/joingame")
                    .header("Content-Type", "application/json")
                    .body("{\"gameId\":\"" + gameId + "\",\"player\":\"" + player + "\"}")
                    .asString();
            if (res.getBody().contains("joined")) {
                statusLabel.setText("Joined game " + gameId);
                gameIdField.setText(gameId);   // <-- for copying
                refreshBoard[0].run();
            } else {
                statusLabel.setText("Error joining game. Maybe wrong ID or name already taken?");
            }
        });

        Button refreshGamesButton = new Button("Refresh Game List");
        refreshGamesButton.setOnAction(e -> {
            var res = Unirest.get("http://localhost:8081/games").asString();
            JsonArray arr = JsonParser.parseString(res.getBody()).getAsJsonArray();
            gamesListView.getItems().clear();
            gameIdByListItem.clear();
            for (JsonElement el : arr) {
                JsonObject obj = el.getAsJsonObject();
                String gameId = obj.get("gameId").getAsString();
                String player1 = obj.has("player1") && !obj.get("player1").isJsonNull() ? obj.get("player1").getAsString() : "";
                String player2 = obj.has("player2") && !obj.get("player2").isJsonNull() ? obj.get("player2").getAsString() : "";
                boolean isOpen = obj.has("isOpen") && obj.get("isOpen").getAsBoolean();
                String label = "ID: " + gameId + " | P1: " + player1 + " | P2: " + player2 + (isOpen ? " [JOINABLE]" : " [FULL]");
                gamesListView.getItems().add(label);
                gameIdByListItem.put(label, gameId);
            }
        });

        Button joinSelectedButton = new Button("Join Selected Game");
        joinSelectedButton.setOnAction(e -> {
            String selected = gamesListView.getSelectionModel().getSelectedItem();
            String userName = nameField.getText();
            if (userName == null || userName.trim().isEmpty()) {
                statusLabel.setText("Please enter your name before joining!");
                return;
            }
            if (selected != null && gameIdByListItem.containsKey(selected)) {
                joinGameField.setText(gameIdByListItem.get(selected));
                joinButton.fire();
            }
        });




        // 🧱 Layout setup
        VBox root = new VBox(8,
            new Label("Name:"), nameField,
            createButton,
            new Label("Or join by game ID:"),
            joinGameField, joinButton,
            statusLabel,
            new Label("Current Game ID:"), gameIdField,
            turnLabel,    // <--- add this
            piecesLabel,  // <-- add
            winnerLabel,
            boardGrid,
            refreshGamesButton, gamesListView, joinSelectedButton
        );


        primaryStage.setScene(new Scene(root, 400, 900));
        primaryStage.setTitle("Checkers Client");
        primaryStage.show();
    }
}
