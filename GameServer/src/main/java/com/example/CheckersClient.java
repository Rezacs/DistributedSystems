package com.example;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import kong.unirest.Unirest;
import com.google.gson.*;

public class CheckersClient extends Application {

    private String gameId;
    private String player;
    private int selectedRow = -1, selectedCol = -1; // Tracks the selected "from" cell
    private TextField gameIdField = new TextField();
    private Label turnLabel = new Label("Turn: ");
    private Label winnerLabel = new Label("Winner: ");
    private Label piecesLabel = new Label("Pieces left - W: 12, B: 12");


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
            if (obj.has("whiteLeft") && obj.has("blackLeft")) {
                int w = obj.get("whiteLeft").getAsInt();
                int b = obj.get("blackLeft").getAsInt();
                piecesLabel.setText("Pieces left - W: " + w + ", B: " + b);
            }
            if (obj.has("winner") && !obj.get("winner").isJsonNull()) {
                String winner = obj.get("winner").getAsString();
                winnerLabel.setText("WINNER: " + winner + " 🎉");
                turnLabel.setText("Game over!");
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
            boardGrid
        );


        primaryStage.setScene(new Scene(root, 400, 450));
        primaryStage.setTitle("Checkers Client");
        primaryStage.show();
    }
}
