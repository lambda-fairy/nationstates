# NationStates for Haskell

[NationStates] is an online government simulation game, created by Max Barry. The site generates a wealth of data, some of which can be accessed through its [official API].

This library lets you query this API using the Haskell programming language.

[NationStates]: https://nationstates.net
[official API]: https://www.nationstates.net/pages/api.html


## Dependencies

* GHC 7.10

    + GHC 7.8 support is planned – patches welcome!


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
