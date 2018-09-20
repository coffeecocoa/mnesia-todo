# mnesia-todo

1. run ```erl``` inside directory.
2. compile with files using ```c(db_server).``` , ```c(db_app).``` , ```c(db_sup).``` .
3. run ```db_server:install().```
4. run ```application:start(mnesia).```
5. ```mnesia:system_info().``` for db info.
6. run ```db_server:start_link().```
