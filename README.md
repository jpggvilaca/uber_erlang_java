## uber_erlang_java
### Simplified uber app - Server side made in erlang, client side made in Java
Open terminal and:
- Compile all .java files: javac @sources.txt
- Compile all .erl files: erl -make

Open erlang shell running 'erl' and:
- run uber:start(port). (e.g uber:start(8888))

Open another shell window and:
- run java Client 127.0.0.1 port. (e.g Client 127.0.0.1 8888)