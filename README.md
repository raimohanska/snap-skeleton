Snap Skeleton
=============

Skeleton for new Haskell/Snap RESTful Web Services. I've removed all unrelated
complexity such as Snaplets, Templating, Application state etc to make
it as simple as possible. However, I'm planning to include facilities
and examples for

- Parsing UTF8 encoded JSON request body into Haskell data values
- Generating a JSON response from Haskell data values
- Extracting values from RESTful paths like /users/jack
- Automated testing


What's wrong with `snap init`?
==============================

Well, you can do that too, but you'll get a lot of stuff that you ain't
gonna need for a typical RESTful Web Service. Don't you just hate to
look a ton of generated code that you don't understand?

Don't get me wrong here though: I really dig Snap. It's just `snap
init` I find unsuitable for my current needs.

Snap intro
==========

Basically the bare-bones Snap applcation is really simple, like this:

~~~ .haskell
lol :: Snap()
lol = do 
    reqBody <- liftM (T.unpack . E.decodeUtf8) getRequestBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = "You got lolld"
    writeLBS $ (E.encodeUtf8 . T.pack) $ reply  

main :: IO ()
main = quickHttpServe $ route [ ("/", lol) ] 
~~~ 

The main method starts Snap and routes the root url to a function named
`lol`. This function reads the request body (assuming it was a POST with
UTF-8 encoding),
then prints it to stdout and finally replies with the same string.

Hope this helps you as much as it does me!

JSON
====

Using Text.JSON.Generic, working with JSON data is easy and fun. Like in
my extremely simple example "JsonEcho", you just define your data type
to match the JSON structure and call `encode`:

~~~ .haskell
{-# LANGUAGE DeriveDataTypeable #-}
import           Text.JSON.Generic

data Hello = Hello { message :: String } deriving (Data, Typeable, Show)

jsonMessage = encode $ Hello "Hello!"
~~~

This will generate a JSON string as in

~~~ .JSON
{ "message" : "Hello!" }
~~~

Parsing JSON is similarly easy. Just use the `encode` function.

Status
======

This stuff is under progress! Here's the backlog:

- Path variable extraction
- Pattern matching examples in tests
- Giter8 template for cloning the skeleton for _your_project_
