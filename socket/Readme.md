# Goal

- Create a client/server that uses local socket to communicate
```
+-----------------------------+----------------------------+
|      Server                 |         Client             |
|-----------------------------+----------------------------|
| Create socket               | Create socket              |
| Bind                        |                            |
| Listen                      |                            |
| Accept (wait for client) <--|--  Connect                 |
| Recv (wait for client)   <--|--   Send                   |
| send                      --|-->  Recv (wait for server) |
+-----------------------------+----------------------------+
```
# Build & Run

- `dune build`
- `./_build/default/bin/server.exe`
- `./_build/default/bin/client.exe`

# links

- https://www.geeksforgeeks.org/understanding-unix-sockets/
