# NationStates for Haskell

[NationStates] is an online government simulation game, created by Max Barry. The site generates a wealth of data, some of which can be accessed through its [official API].

This library lets you query this API using the Haskell programming language.

[NationStates]: https://nationstates.net
[official API]: https://www.nationstates.net/pages/api.html


## Features

* **Type safe**: you can't refer to a shard unless you explicitly request it.

* **Automatic rate limiting**, which can be disabled or overridden if you want.

* **HTTP support** via the [tls] library.

* **Free and open source** under the Apache License, version 2.0.

[tls]: https://hackage.haskell.org/package/tls


## Dependencies

* GHC 7.6 or newer


## Installation

`nationstates` is hosted on [Hackage].

    cabal install nationstates

[Hackage]: https://hackage.haskell.org/package/nationstates


## Example

```haskell
import NationStates
import qualified NationStates.Nation as Nation
import Text.Printf

main = withContext "ExampleBot/2000" $ \c -> do
    (name, motto) <- Nation.run "Montesardo-East Adanzi" shards c
    printf "%s has the motto: %s\n" name motto
  where
    shards = (,) <$> Nation.name <*> Nation.motto
```
