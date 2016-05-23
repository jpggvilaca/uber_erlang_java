## uber_erlang_java
### Simplified uber app - Server side made in erlang, client side made in Java
Open terminal and, on 'client' folder:
- Compile all .java files by running: javac @sources.txt
- Run the client: java Client 127.0.0.1 port. (e.g Client 127.0.0.1 8888)

Open another terminal window and, on 'client' folder:
- Compile all .erl files by running: erl -make
- uber:start(port). (e.g uber:start(8888))