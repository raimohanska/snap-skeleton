Snap Skeleton
=============

Skeleton for new Haskell/Snap "Web Services". I've removed all unrelated
complexity such as Snaplets, Templating, Application state etc to make
it as simple as possible. However, I'm planning to include facilities
and examples for

- Parsing UTF8 encoded JSON request body into Haskell data values
- Generating a JSON response from Haskell data values
- Extracting values from RESTful paths like /users/jack
- Automated testing
