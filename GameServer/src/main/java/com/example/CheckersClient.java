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
import javafx.geometry.Insets;



public class CheckersClient extends Application {

    private String gameId;
    private String player;
    private int selectedRow = -1, selectedCol = -1; // Tracks the selected "from" cell
    private TextField gameIdField = new TextField();
    private Label turnLabel = new Label("Turn: ");
    private Label winnerLabel = new Label("Winner: ");
    private Label piecesLabel = new Label("Pieces left - W: 12, B: 12");

    private Label whitePlayerLabel = new Label("White (W): ");
    private Label blackPlayerLabel = new Label("Black (B): ");

    private ListView<String> gamesListView = new ListView<>();
    private Map<String, String> gameIdByListItem = new HashMap<>();


    private ListView<String> serversListView;
    private Map<String, String> serverUrlByLabel = new HashMap<>();
    private String selectedServerUrl = "http://localhost:8081";
    private Button refreshServersButton;

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


        serversListView = new ListView<>();
        refreshServersButton = new Button("Refresh Servers");

        refreshServersButton.setOnAction(e -> {
            var res = Unirest.get("http://localhost:8080/servers").asString();
            JsonArray arr = JsonParser.parseString(res.getBody()).getAsJsonArray();
            serversListView.getItems().clear();
            serverUrlByLabel.clear();
            for (JsonElement el : arr) {
                JsonObject obj = el.getAsJsonObject();
                String host = obj.get("host").getAsString();
                int port = obj.get("port").getAsInt();
                String serverId = obj.get("server_id").getAsString();
                String label = serverId + " - " + host + ":" + port;
                String url = "http://" + host + ":" + port;
                serversListView.getItems().add(label);
                serverUrlByLabel.put(label, url);
            }
        });

        serversListView.getSelectionModel().selectedItemProperty().addListener((obs, oldV, newV) -> {
            if (newV != null) {
                selectedServerUrl = serverUrlByLabel.get(newV);
                System.out.println("Selected server: " + selectedServerUrl);
            }
        });


        // Set up gameIdField for copying the gameId
        gameIdField.setEditable(false);
        gameIdField.setPromptText("Game ID will appear here...");

        // 🧩 Function to update the board UI
        final Runnable[] refreshBoard = new Runnable[1];
        refreshBoard[0] = () -> {
            if (gameId == null || gameId.isEmpty()) return;

            var res = Unirest.get(selectedServerUrl + "/gamestate/" + gameId)
            .queryString("player", player)
            .asString();
            JsonObject obj = JsonParser.parseString(res.getBody()).getAsJsonObject();

            if (!obj.has("board")) return;

            // Show whose turn it is
            String whoseTurn = obj.has("currentTurn") ? obj.get("currentTurn").getAsString() : "";
            String p1Name = obj.has("player1") && !obj.get("player1").isJsonNull() ? obj.get("player1").getAsString() : "";
            String p2Name = obj.has("player2") && !obj.get("player2").isJsonNull() ? obj.get("player2").getAsString() : "";

            // Use p1Name and p2Name below
            if (!whoseTurn.isEmpty()) {
                String turnPlayerName = "";
                if (whoseTurn.equals(p1Name)) {
                    turnPlayerName = p1Name + " (White)";
                } else if (whoseTurn.equals(p2Name)) {
                    turnPlayerName = p2Name + " (Black)";
                } else {
                    turnPlayerName = whoseTurn;
                }
                turnLabel.setText("Turn: " + turnPlayerName);

                if (player != null) {
                    if (player.equals(whoseTurn)) {
                        statusLabel.setText("It's your turn!");
                    } else {
                        statusLabel.setText("Waiting for " + turnPlayerName + " ...");
                    }
                }
            }



            // After updating turnLabel:
            String player1 = obj.has("player1") && !obj.get("player1").isJsonNull() ? obj.get("player1").getAsString() : "";
            String player2 = obj.has("player2") && !obj.get("player2").isJsonNull() ? obj.get("player2").getAsString() : "";

            // Assume: player1 is always white, player2 is always black (if both are set)
            whitePlayerLabel.setText("White (W): " + player1);
            blackPlayerLabel.setText("Black (B): " + player2);

            // p1Name and p2Name should be the names you parsed earlier in refreshBoard[0]
            if (obj.has("whiteScore") && obj.has("blackScore")) {
                int w = obj.get("whiteScore").getAsInt();
                int b = obj.get("blackScore").getAsInt();
                piecesLabel.setText("Pieces left - W: " + w + ", B: " + b);

                if (w < 6 && b >= 6) {
                    // Black (player2) wins
                    winnerLabel.setText("WINNER: " + p2Name + " (Black) 🎉");
                    turnLabel.setText("Game over!");
                    // Optionally: disable the board or moves here
                } else if (b < 6 && w >= 6) {
                    // White (player1) wins
                    winnerLabel.setText("WINNER: " + p1Name + " (White) 🎉");
                    turnLabel.setText("Game over!");
                    // Optionally: disable the board or moves here
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
                            var resp = Unirest.post(selectedServerUrl + "/move")
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
            var res = Unirest.post(selectedServerUrl + "/newgame")
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

            var res = Unirest.post(selectedServerUrl + "/joingame")
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
            var res = Unirest.get(selectedServerUrl + "/games").asString();
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
        // LEFT: Lobby and controls
        VBox leftBox = new VBox(8,
            refreshServersButton, serversListView,
            new Label("Name:"), nameField,
            createButton,
            new Label("Or join by game ID:"),
            joinGameField, joinButton,
            statusLabel,
            refreshGamesButton, gamesListView, joinSelectedButton
        );
        leftBox.setPrefWidth(230); // You can adjust width

        // RIGHT: Game info and board
        VBox rightBox = new VBox(8,
            new Label("Current Game ID:"), gameIdField,
            whitePlayerLabel,
            blackPlayerLabel,
            turnLabel,
            piecesLabel,
            winnerLabel,
            boardGrid
        );
        rightBox.setPrefWidth(170);

        // Combine left and right in an HBox
        HBox root = new HBox(leftBox, rightBox);

        // Optional: add padding and spacing
        root.setSpacing(10);
        leftBox.setPadding(new Insets(10));
        rightBox.setPadding(new Insets(10));

        // Show in window
        primaryStage.setScene(new Scene(root, 420, 700)); // Adjust size as you like
        primaryStage.setTitle("Checkers Client");
        primaryStage.show();

    }
}
