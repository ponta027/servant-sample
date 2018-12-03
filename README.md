# servant-sample


## Server

* src/Server.hs
* app/Main.hs
> stack exec servant-sample-exe


## Client
* client/Main.hs
* client/Client.hs

> stack exec servant-sample-client 
> ```Position {xCoord = 10, yCoord = 10}
HelloMessage {msg = "Hello, servant"}
Email {from = "great@company.com", to = "alp@foo.com", subject = "Hey Alp, we miss you!", body = "Hi Alp,\n\nSince you've recently turned 26, have you checked out our latest haskell, mathematics products? Give us a visit!"}```


## Static strings

```
"xxxx" :> "yyyyy" :> "zzzz" :> Get '[JSON] Users
```
のように記述すると

/xxxx/yyyyy/zzzz 固定パスでアクセスできるようになる。


## Static Files

```
type StaticAPI = "static" :> Raw
server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

echo "Hello World" > static-files/foo.txt
curl http://localhost:8081/static/foo.txt
```


