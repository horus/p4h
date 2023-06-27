# p4h

(dysfunctional) Perforce API binding

I don't quite like command line wrappers, so here it is. Probably not what you expected, as I know nothing about C++. I just make it compile, that's all.

If you have both Haskell and C++ expertise please help.

## Examples

```haskell
example = runP4Env env ["login", "-as", "super"] $ \p4 -> do
  setProg p4 "unnamed haskell script"
  setVersion p4 "2023.06"
  -- setInput p4 "my-super-password"
  setHandler p4 (OutputInfo $ putStrLn . ("[Info] " ++))
  where
    env = P4Env Nothing Nothing Nothing (Just "tcp:1666") Nothing
```

... would leave a trace in the server log `commands.csv` as:

```
2.56,1687838320,413824960,2023/06/27 11:58:40 413824960,89388,-,master,1,super,local,user-login,127.0.0.1,unnamed haskell script,2023.06,-as:super,,.017s,,21,21,.000s
```

and print something similiar to:

```
[Info] User super ticket expires in 11 hours 57 minutes.
[Info] User super on host 127.0.0.1: Ticket: Unset
```

## Rationale

> The Helix Server protocol is not designed to support multiple concurrent queries over the same connection. Multithreaded applications that use the C++ API or derived APIs should ensure that a separate connection is used for each thread, or that only one thread may use a shared connection at a time.

That's what the document says, so I go create a new connection every time.

## Build

Download the Helix Core C/C++ API [here](https://www.perforce.com/downloads/helix-core-c/c-api).

While `p4api/include` may contain the up-to-date version of the header files, feel free to replace them. You also need to put the libs into the corresponding directory, check `extra-lib-dirs` in `stack.yaml`.

__Note__: I can't simply tell which OS it is, check [stack#2048](https://github.com/commercialhaskell/stack/issues/2048) and [stack#3369](https://github.com/commercialhaskell/stack/issues/3369).
