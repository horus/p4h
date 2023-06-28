# p4h

(dysfunctional) Perforce API binding

I don't quite like command line wrappers, so here it is. Probably not what you expected, as I know nothing about C++. I just make it compile, that's all.

If you have both Haskell and C++ expertise please help.

## Examples

An alternative version of [this](https://www.perforce.com/manuals/p4api/Content/P4API/clientapi.dropped.html#clientapi.dropped.example) example:

```haskell
example :: IO ()
example = withP4Env env $ \p4 -> do
  setProg p4 "unnamed haskell script"
  setVersion p4 "2023.06"
  setInput p4 "my-super-password"
  setHandler p4 (OutputInfo $ putStrLn . ("[Info] " ++))
  run p4 "login"
  showOutput p4
  let loop i = do
        connDropped <- dropped p4
        unless (connDropped || i > 9) $ do
          putStrLn $ "loop #" ++ show i
          run p4 "have"
          showOutput p4
          loop (i+1)
  loop 0
  where
    env = P4Env (Just "super") Nothing (Just "localhost") (Just "tcp:1666") (Just "my-workspace")
```

... would print something similiar to:

```
[Info] User super logged in.
User super logged in.
loop #0
[Info] //depot/test.txt#45 - /workspace/depot/test.txt
//depot/test.txt#45 - /workspace/depot/test.txt
... ...
```

## Rationale

> The Helix Server protocol is not designed to support multiple concurrent queries over the same connection. Multithreaded applications that use the C++ API or derived APIs should ensure that a separate connection is used for each thread, or that only one thread may use a shared connection at a time.

That's what the document says, so I go create a new connection every time.

## Build

Download the Helix Core C/C++ API [here](https://www.perforce.com/downloads/helix-core-c/c-api).

While `p4api/include` may contain the up-to-date version of the header files, feel free to replace them. You also need to put the libs into the corresponding directory, check `extra-lib-dirs` in `package.yaml`.

__Note__ (for macOS): Don't forget to add the directory where `libssl` is located.
