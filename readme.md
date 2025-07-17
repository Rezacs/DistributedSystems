# Distributed Multiplayer Checkers Platform

**A fault-tolerant, distributed, real-time multiplayer checkers game, built as a demo of distributed systems concepts.**

## Features

- Multiple independent Java game servers, each able to host multiple checkers games
- Central Erlang/OTP coordination service for registration, discovery, and heartbeat-based server health monitoring
- Fully distributed architecture: start, stop, and add servers at runtime
- Clients (JavaFX GUI) can browse available servers, pick one, and create/join checkers games
- Strict player identity: only original players can re-join games, ensuring robust session management
- Real-time synchronization, move validation, and turn-taking enforced across the network
- Automatic removal of failed servers from the registry (via heartbeat timeout)
- Clean, modern GUI (JavaFX), showing server list, game list, game board, players, and winner
- (Optional) Game session save/restore for fault tolerance and session handover

## Architecture

- **Erlang Coordination Service:**  
  Handles server registry (`/register`, `/servers`), monitors server health (heartbeat), and enables service discovery for clients. Optionally stores session data for failover.

- **Java Game Servers:**  
  Each server registers with Erlang, exposes a REST API for checkers operations, and maintains its own set of game sessions and players. Servers send regular heartbeats to stay listed.

- **Clients:**  
  JavaFX client (desktop GUI) allows users to see active servers, select a server, and play games. All communication uses REST over HTTP.

## How To Run

### 1. Start Erlang Coordinator

```bash
cd CoordinationService
rebar3 shell
```

### 2. Start One or More Java Game Servers

In different terminal windows:

```bash
java -jar target/gameserver-1.0-SNAPSHOT.jar 8081 srv1
java -jar target/gameserver-1.0-SNAPSHOT.jar 8082 srv2
```

### 3. Start the JavaFX Client

```bash
java --module-path ~/javafx-sdk-17.0.15/lib --add-modules javafx.controls,javafx.fxml -cp target/gameserver-1.0-SNAPSHOT.jar com.example.CheckersClient
```

Browse available servers, create/join games, and play.

### 4. Play and Demo

- Create or join games as different users.
- Show server discovery, turn-based gameplay, and real-time updates.
- Close/kill a server: it disappears from the registry; other servers and games continue running.

## Test Scenarios

- **Distributed Play:** Multiple servers, multiple clients, real-time play across the network.
- **Server Failure:** Kill a server; it is removed from `/servers`, other servers/games continue.
- **Player Reconnect:** Player can close GUI, reopen and resume with same name.
- **Access Control:** Only original players can join/continue a session; others are blocked.
- **Win Detection:** When a player drops below 6 pieces, the game ends and the winner’s name is shown.

## Why Is This a Distributed System?

- No single point of failure: any server can join/leave at runtime, system continues operating.
- Central coordinator for service discovery and health monitoring, but all game state and logic are distributed across independent nodes.
- Robust communication and synchronization via REST and heartbeats.
- Demonstrates classic distributed concepts: registration, coordination, communication, synchronization, and fault tolerance.

## (Optional) Next Steps

- Add web frontend for browser play.
- Full session handover: reconnect clients to a new server after failure.
- Dockerize and deploy across multiple machines or VMs.

## Credits

Developed by [Your Name] for Distributed Systems & Middleware Technologies @ University of Pisa, 2024/2025.  
Supervisor: Prof. Alessio Bechini
