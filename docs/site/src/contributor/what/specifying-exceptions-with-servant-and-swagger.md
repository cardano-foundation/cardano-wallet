# Specifying Exceptions with Servant and Swagger

## Contents

- [Specifying Exceptions with Servant and Swagger](#specifying-exceptions-with-servant-and-swagger)
  - [Contents](#contents)
  - [Goal](#goal)
  - [Background](#background)
  - [Example](#example)
  - [What do we really want?](#what-do-we-really-want)
  - [How can we achieve this?](#how-can-we-achieve-this)
    - [Adding custom errors to the generated Swagger output](#adding-custom-errors-to-the-generated-swagger-output)
    - [Removing default errors from the generated Swagger output](#removing-default-errors-from-the-generated-swagger-output)
  - [Complete working example project](#complete-working-example-project)

## Goal

To be able to exert fine-grained control over errors defined in Swagger API specifications.

## Background

Our API is defined in Haskell with [Servant](https://haskell-servant.github.io/), and translated into [Swagger](https://swagger.io/) format using the [`servant-swagger`](http://hackage.haskell.org/package/servant-swagger) library.

Swagger makes it possible to specify that an endpoint can return one or more errors. For example, the following specification states that the endpoint can return a `404` (not found) error:

```JSON
"responses" :
  { "200" : { "schema" : {"$ref" : "#/definitions/Location" }
            , "description" : "the matching location" } }
  , "404" : { "description" : "a matching location was not found" }
```

By default, Servant doesn't provide a way for API authors to manually specify errors they might wish to return. However, this might be desirable: consider the case where you'd like to perform validation based on constraints that are not conveniently expressible in the Haskell type system. In this case, you would reject input at run-time, but this would typically not be reflected in the Servant type.

Since Servant itself doesn't provide a way to manually specify errors, and since it is typical to define errors when writing a Swagger specification, `servant-swagger` takes the approach of **auto-generating** errors when various Servant combinators appear in the API. For example, when a `Capture` combinator is used, `servant-swagger` automatically inserts a `404` (not found) error in the generated Swagger output.

However, auto-generation of error responses has two problems:
1. The generated error responses are sometimes **incomplete**.
2. The generated error responses are sometimes **inappropriate**.

## Example

Consider the following endpoint, allowing the caller to add a new `Location` to a location database:

```Haskell
type AddLocation = "location"
  :> "add"
  :> Summary "Add a new location"
  :> Capture "locationName" Text
  :> Put '[JSON] Location
```
By default, the generated Swagger output includes a 404 error (not found):

```JSON
"/location/add/{locationName}" :
  { "put" :
    { "parameters" : [ { "in" : "path"
                       , "type" : "string"
                       , "name" : "locationName"
                       , "required" : true } ]
    , "responses" :
      { "200" : { "schema" : { "$ref" : "#/definitions/Location" }
                , "description" : "the added location" }
      , "404" : { "description" : "`locationName` not found" } }
    , "summary" : "Add a new location"
    , "produces" : ["application/json;charset=utf-8"] } }
```
In the above example:
1. The generated error is **inappropriate**. Since we're adding a new location (and not looking up an existing location), we don't want to ever return a `404`.
2. The error we really want is **missing**. We'd like to perform various validation checks on the new location name, and possibly return a `400` error if validation fails. However, this isn't included in the generated Swagger output.

## What do we really want?

Suppose that adding a `Location` can fail in two ways, either because:
* the location name is too short; or
* the location name contains invalid characters.

We'd ideally like for the `"responses"` section to reflect the above modes of failure:
```JSON
"responses" :
  { "200" : { "schema" : { "$ref" : "#/definitions/Location" }
            , "description" : "the added location" }
  , "400" : { "description" :
                "the location name was too short
                 OR
                 the location name contained invalid characters" } }
```
## How can we achieve this?

The [`servant-checked-exceptions`](http://hackage.haskell.org/package/servant-checked-exceptions) package defines the [`Throws` combinator](http://hackage.haskell.org/package/servant-checked-exceptions-core-2.0.0.0/docs/Servant-Checked-Exceptions-Internal-Servant-API.html#t:Throws), making it possible to specify individual exceptions as part of the endpoint definition.

Let's have a look at how we might use the `Throws` combinator to define our modes of failure:
```Haskell
type AddLocation = "location"
  :> "add"
  :> Summary "Add a new location"
  :> Throws LocationNameHasInvalidCharsError
  :> Throws LocationNameTooShortError
  :> Capture "locationName" Text
  :> Put '[JSON] Location

data LocationNameTooShortError = LocationNameTooShortError
  deriving (Eq, Generic, Read, Show)
data LocationNameHasInvalidCharsError = LocationNameHasInvalidCharsError
  deriving (Eq, Generic, Read, Show)
```
The above type specifies an endpoint that can throw two different types of exception.

It's possible to assign specific response codes to individual exceptions by defining [`ErrStatus`](http://hackage.haskell.org/package/servant-checked-exceptions-core-2.0.0.0/docs/Servant-Checked-Exceptions-Internal-Servant-API.html#t:ErrStatus) instances. In our example, both exceptions will share the same response code `400` (bad request):
```Haskell
instance ErrStatus LocationNameHasInvalidCharsError where
  toErrStatus _ = toEnum 400
instance ErrStatus LocationNameTooShortError where
  toErrStatus _ = toEnum 400
```
For client code that's written in Haskell, the `servant-checked-exceptions` library provides the very useful [`catchesEnvelope`](https://hackage.haskell.org/package/servant-checked-exceptions-core/docs/Servant-Checked-Exceptions-Internal-Envelope.html#v:catchesEnvelope) function, allowing the caller to perform exception case analysis on values returned by an API.

So far so good.

However there are two problems that we need to solve:
1. `servant-swagger` doesn't know what to do with the `Throws` combinator.
2. `servant-swagger` inserts its own default error response codes.

In the sections below, we attempt to solve these errors.

### Adding custom errors to the generated Swagger output

Recall that Swagger error definitions include a description:
```JSON
"400" : { "description" : "the location name was too short" }
```
By default, `servant-checked-exceptions` doesn't provide a way to define descriptions for exceptions. We can solve this by defining our own `ErrDescription` class, and providing instances:
```Haskell
class ErrDescription e where
  toErrDescription :: e -> Text

instance ErrDescription LocationNameHasInvalidCharsError where
  toErrDescription _ =
    "the location name contained non-alphabetic characters"
instance ErrDescription LocationNameTooShortError where
  toErrDescription _ =
    "the location name was too short"
```
To include these descriptions in the generated Swagger output, we need to define a new instance of the [`HasSwagger`](http://hackage.haskell.org/package/servant-swagger-1.1.7/docs/Servant-Swagger-Internal.html) type class:
```Haskell
type IsErr err = (ErrDescription err, ErrStatus err)

instance (IsErr err, HasSwagger sub) => HasSwagger (Throws err :> sub)
  where
    toSwagger _ =
      toSwagger (Proxy :: Proxy sub) &
        setResponseWith
          (\old _ -> addDescription old)
          (fromEnum $ toErrStatus (undefined :: err))
          (return $ mempty & description .~ errDescription)
        where
          addDescription = description %~ ((errDescription <> " OR ") <>)
          errDescription = toErrDescription (undefined :: err)
```
Note that in the above instance, if multiple errors share the same response code, then we concatenate together the descriptions, separating the descriptions with `" OR "`.

Let's have a look at the auto-generated output:
```JSON
"responses" :
  { "200" : { "schema" : { "$ref" : "#/definitions/Location" }
            , "description" : "the added location" }
  , "400" : { "description" :
                "the location name was too short
                 OR
                 the location name contained invalid characters" }
  , "404" : { "description" : "`locationName` not found" } }
```
The `400` section now contains what we want.

However, the unwanted `404` section is still there. Recall that this is generated automatically by `servant-swagger`. How can we remove this default error response?

### Removing default errors from the generated Swagger output

Currently, `servant-swagger` doesn't provide a way to disable the generation of default error responses. There are several possible ways to solve this:

1. Provide a patch to `servant-swagger` that adds a configuration option to disable the generation of default error responses. See [here](https://github.com/jonathanknowles/servant-swagger/tree/jonathanknowles/disable-default-error-responses) for a simple fork that disables all default error responses.

2. Define a new alternative `Capture` operator and associated `HasSwagger` instances that don't generated default error responses. The downside is that this might require us to define instances for multiple other type classes.

3. Amend the `HasSwagger` instance of `Throws` to detect and erase any default error responses. This solution would be rather brittle, as it would require the `Throws` combinator to appear at a particular place in endpoint definition.

4. Add a new combinator that disables the generation of default error responses. This solution would also be rather brittle, as it would require the new combinator to appear at a particular place in the endpoint definition.

## Complete working example project

See the following example project for a complete working example:

https://github.com/jonathanknowles/servant-checked-exceptions-example

Note that the above example also uses a **patched** version of `servant-client`, to allow pattern matching on error responses with the `catchesEnvelope` function.
