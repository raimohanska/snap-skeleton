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

Hope this helps you as much as it does me!
