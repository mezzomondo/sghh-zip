## sghh-zip
How to create a zip archive passing a folder name as a command line parameter in Haskell

Even this apparenly easy task involves many complex language constructs

## sghh?
"Self-help group for haskellers in heat" will try to solve everyday problems in Haskell.
Because our "real world" is not their "real world".

## Code explanation
*This is a real world problem.* The main difference between Haskell and, say, Python is that in Python you can just read a couple of pages of documentation, and a couple of threads on StackOverflow, and have enough information to achieve a satisfactory result. In Haskell you need to reach the end of a couple of 200 pages manuals and probably something is still out of your hands.

So, let's start analysing the main function.
```haskell
(dir:_)   <- getArgs
```
Here nothing special. The type signature is
```haskell
getArgs :: IO [String]
```
meaning, uhm, that eventually in the ```dir``` variable will end the first argument ignoring all the rest.

Let's see what
```haskell
getDirectoryContents :: FilePath -> IO [FilePath]
```
does. First, ```FilePath``` is nothing more than a String. So ```getDirectoryContents``` will return a list of strings wrapped in the IO thing. Each string being a filename or foldername relative to ```dir```. But in order to create a zip file I need the absolute path, right? It could be something like:
```haskell
relativePaths <- getDirectoryContents dir
let absolutePaths = map (dir </>) relativePaths
```
using
```haskell
(</>) :: FilePath -> FilePath -> FilePath
```
But what if I want it all in a single line? Can I do something like:
```haskell
absolutePaths <- map (dir </>) (getDirectoryContents dir)
```
????

NOOOOOOOAOAOAOAOAO because of the IO thing.
What I can do is reading the Functor chapter on a couple of handbooks (it's around page 100, usually), to learn that the fmap function
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```
can be used to mimic the behaviour of
```haskell
map :: (a -> b) -> [a] -> [b]
```
when we are using structures other that List. The pattern is the same: apply a function to elements that live inside a structure. So it could be something like:
```haskell
absolutePaths <- fmap (map (dir </>)) (getDirectoryContents dir)
```
```fmap``` here is applying ```map (dir </>)``` inside the IO thing. But unfortunately the Applicative chapter is usually right after the Functor one, so we learn very soon (page 120) that fmap has also a much nicer infix notation:
```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
```

So that's how I ended up writing:
```haskell
files     <- map (dir </>) <$> getDirectoryContents dir
```

At the moment ```files``` is a list of filePaths containing everything, folders included. What if I need to exclude all the folders from this list? Sounds like a filter on a list? IT IS! But.
But one function we can use as a filter is:
```haskell
doesFileExist :: FilePath -> IO Bool
```
(The operation doesFileExist returns True if the argument file exists and is not a directory, and False otherwise.)
NOT Bool!! IO Bool!!! 
So it's not exactly a filter, that has signature:
```haskell
filter :: (a -> Bool) -> [a] -> [a]
```
while we need:
```haskell
something :: (a -> IO Bool) -> [a] -> IO [a]
```
(that returns ```IO [a]``` because, you know, once in IO in IO forever). And infact if you enter this query "(a -> IO Bool) -> [a] -> IO [a]" in [Hoogle](https://www.haskell.org/hoogle/) the very first result is exactly:
```haskell
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
```
YOU. ARE. MY. MAN!
Hence:
```haskell
filesOnly <- filterM doesFileExist files
```
(tbc)

## Instructions
```
$ stack build
$ stack exec sghh-zip .
```
