Snap Skeleton
=============

Skeleton for new Haskell/Snap RESTful Web Services. I've included some facilities
and examples for

- Parsing UTF8 encoded JSON request body into Haskell data values
- Generating a JSON response from Haskell data values
- Extracting values from RESTful paths like /users/jack
- Automated testing

So this is kind of a tutorial, or a skeleton, or a framework.. say what? Let's just
say I've put together some shit I like to use when I write web services in Haskell..
This thing emerged when I found myself copy-pasting a lot of code from one project to another.

Requires GHC 7. What? Try this:

~~~ .bash
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 7.0.3
$ 
~~~

If it looks like that, you're okay. If you don't have GHC, you can get started by downloading 
the [Haskell Platform](http://hackage.haskell.org/platform/), or `brew update; brew install haskell-platform` on a Mac.

Snap intro
==========

Please have a look at [Snap API Intro](http://snapframework.com/docs/tutorials/snap-api) for a brief intro on
getting started with Snap. Or, have a look at this minimal Snap App:

~~~ .haskell
lol :: Snap ()
lol = method POST $ do 
    reqBody <- readBody
    liftIO $ putStrLn $ "Received " ++ reqBody
    let reply = "You got lolled"
    writeResponse reply  

main :: IO ()
main = quickHttpServe $ route [ ("/", lol) ] 
~~~ 

The main method starts Snap and routes the root url to a function named
`lol`. This function reads the request body (if it was a POST),
then prints it to stdout and finally replies with "You got lolled". 

The `readBody` and `writeResponse` functions are part of the included `HttpUtil` module,
and take care of the UTF-8 encoding and decoding involved, so that you can work with Strings
only. That's not very performant but should be fine to start with.

I think this is not bad: there's hardly any boilerplate there. You could drop the type
signratures too, if you don't find them useful.

Or, if you get to write a lot of actions that parse the request body as
a string and then produce some other string, you could extract this
boilerplate into a function like

~~~ .haskell
processPost :: (String -> String) -> Snap ()
processPost f = do 
    reqBody <- liftM (T.unpack . E.decodeUtf8) getRequestBody
    writeLBS $ E.encodeUtf8 $ T.pack $ f $ reqBody
~~~

JSON
====

Using Data.Aeson.Generic, working with JSON data is easy and fun. Like in
my extremely simple example "JsonEcho", you just define your data type
to match the JSON structure and call `encode`:

~~~ .haskell
{-# LANGUAGE DeriveDataTypeable #-}
import qualified Data.Aeson.Generic as JSON

data Hello = Hello { message :: String } deriving (Data, Typeable, Show)

jsonMessage = JSON.encode $ Hello "Hello!"
~~~

This will generate a JSON string as in

~~~ .JSON
{ "message" : "Hello!" }
~~~

Parsing JSON is similarly easy. Just use the `decode` function.

Aeson uses lazy ByteStrings instead of Strings. Luckily, that's exactly what Snap uses too, so it's a perfect match.

I included a function `readBodyJson` in my Util.Json module to make it as easy as possible to read JSON from the request body.

RESTful Web Services
====================

Suppose you wanted to create a RESTful web service for storing bananas (ok, you can kick me).
You'd want to store new bananas by

~~~
POST /banana {"color": "yellow"}

=> 1
~~~

and get existing bananas by

~~~
GET /banana/1

=> {"color": "yellow"}
~~~

I included this example in `examples/Restful.hs`. It looks like this:

~~~ .haskell
data Banana = Banana { color :: String } deriving (Data, Typeable, Show)

bananas :: Snap ()
bananas = newBanana <|> getBanana 

newBanana = method POST $ catchError "Banana is rotten" $ do 
    banana <- readBodyJson :: Snap Banana
    liftIO $ putStrLn $ "New banana: " ++ (show banana)
    writeLBS $ JSON.encode $ ("1" :: String) 


getBanana = restfulGet getBanana'    
  where getBanana' "1" = writeLBS $ JSON.encode $ Banana "yellow"
        getBanana' _   = notFound

~~~

The `restfulGet` function is a helper that extracts the `id` parameter from the URL for you. 
The URL mapping is defined in `Main.hs` using the URL pattern `/banana/:id`. 
This tells Snap that the rest of the path should be mapped into the parameter named "id".
The only inconvenient thing is that Snap won't give you access to params as `Strings`, 
but as strict `ByteStrings` instead. I wrote some plumbing code to get that sorted; in `HttpUtil.hs`
there's a function named `getPar` that gives you just that.

Automatic testing
=================

Cabal supports testing. There are tricks to learn though. For instance,

- You need to define a Test-suite in your Cabal file
- It needs to have a decent set of dependencies
- The "main file" for tests must contain a module named "Main". (can be accomplished by omitting module name completely too)
- Before running `cabal test` you need to `cabal configure --enable-tests` and `cabal build`
- `cabal test` does not directly support HUnit: In your main function, you must make sure to return an error code to tell Cabal that the test failed.

I included automatic tests for the sample code. The `run-tests.sh` script does the required Cabal trickery for you.
It's slow though, so if you've not changed any dependencies, you should use `cabal build;cabal test` instead.

The file `Specs.hs` is used to hook all your tests into the "test suite".

I use [HSpec] for testing/speccing pure code the [BDD](http://en.wikipedia.org/wiki/Behavior_Driven_Development) way.
In `FunctionalTest.hs` there are also [functional tests](http://en.wikipedia.org/wiki/Functional_testing) for the example web services.
These tests are implemented using [HUnit](http://hunit.sourceforge.net/), because HSpec doesn't currently support testing non-pure code.
I've included some facilities for making web service testing easy, so you can just write

~~~ .haskell
{-# LANGUAGE QuasiQuotes #-}
module FunctionalSpec where

import Snap.Http.Server.Config
import Test.HUnit
import Server
import Util.HttpTester
import Data.Aeson.QQ
import Data.Aeson.Types
import Data.Text

functionalTests = wrapTest withTestServer $ TestList [
  post "Echo string" url "/echo" "lol" $ Matching "l.*l"
  , postJson "Echo JSON" url "/jsonecho" [aesonQQ|{message:"hola"}|] $ Json [aesonQQ| {message:"hola"} |]
  , get "Echo JSON with GET = 404" url "/jsonecho" $ ReturnCode 404
  , postJson "POST restful Banana" url "/banana" [aesonQQ|{color:"yellow"}|] $ Exactly "\"1\""
  , post "POST rotten Banana" url "/banana" "{wtf?}" $ All $ [ReturnCode 500, Matching ".*rotten.*"]
  , get "GET restful Banana" url "/banana/1" $ Json [aesonQQ|{color:"yellow"}|] 
  , get "Unknown Banana not found - 404" url "/banana/2" $ ReturnCode 404
  ]

port = 8001
url= "http://localhost:" ++ (show port) 

withTestServer = withForkedServer $ Server.serve (setPort port defaultConfig) 
~~~

This will do an HTTP POST to your web service using the path `/echo`, 
writing `lol` into the request body and finally testing that the server will respond with a string 
starting with `l` and ending with `l`. Yep, that's a regex.

It also tests the other example services I included in snap-skeleton, in a self-documenting way (think so?).

This test module uses the utilities defined in HttpTester:

- `wrapTest' wraps any HUnit Test with a given wrapper, so that you can do stuff before and after the actual test
- `withForkedServer` is a wrapper that forks a given action in its own thread and kills the thread after the test
- `post` creates a Test that POSTs given data to given URL and verifies the result using `Matching`, `Exactly` or `ReturnCode`
- `get` creates a similar Test for HTTP GET
- `postJson` posts a JSON object which is in this case defined using [Aeson-QQ](https://github.com/finnsson/aeson-qq), which allows me to embed JSON syntax into my Haskell source.
- `Matching`, `Exactly`, `Json`, `ReturnCode` for verifying the HTTP response in different ways
- `All` for combining different response matchers such as `ReturnCode` and `Json`.

This test uses `Main.serve (setPort port defaultConfig)` as the argument for `withForkedServer` to start up the example web services in the port 8001.

Building and running
====================

[Cabal](http://www.haskell.org/cabal/) is the Maven for Haskell. 
You use it to define your package (project) along with its dependencies in a `yourproject.cabal` file.
For each dependency you can optionally specify a version like "json >= 0.5".
Cabal will download (from [Hackage](http://hackage.haskell.org/packages/hackage.html)) and install the packages you depend on, so you just say

~~~ .bash
cabal install
..
..
..
Installing executable(s) in /Users/juha/.cabal/bin
~~~

.. and you'll get an executable. If this is your first time running Cabal, it'll take some time because it compiles all
the dependencies too.
So if you build snap-skeleton without changing the name of the generated executable (defined in the cabal file),
you'll get an executable named `snap-skeleton`. Make sure you've got Cabal's output directory (usually ~/.cabal/bin) on your path, and it'll
be easy to run the executable too:

~~~ .bash
Juha-Paananens-MacBook-Pro:snap-skeleton juha$ snap-skeleton
no port specified, defaulting to port 8000
Listening on http://0.0.0.0:8000/
~~~

Troubleshooting
===============

If you have GHC 7.2.1, you will get error on cabal install (at least for
HSpec 0.9). This is fixed in repo, but new release is not yet available.

No worries, do this:

~~~ .bash
cabal unpack hspec
cd hspec-0.9.0
~~~

Edit Setup.lhs and remove line "import System".

~~~ .bash
cabal install
~~~

... and you are done.

Status
======

This stuff is under progress! Here's the backlog:

- Learn from Snap tests, possibly employ Snap.Test facilities
- Cabal repl or similar (cabal-dev ghci is close but doesn't allow reloading of srcs)
- Convert this into a template for `snap init`
