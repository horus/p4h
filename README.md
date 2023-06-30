# p4h

(dysfunctional) Perforce Helix Core API wrapper

## Goal

This is the very implementation with my personal bias. It's far from mature.

- ✅ Run basic Perforce (p4) commands
- 🧪 Parse and format specs
- 🧪 Handle errors
- 🧪 Auxiliary functions
- ❓ Progress
- ❓ Revision & Integration
- ❓ Merge & Resolver
- ❓ Graph / Stream mode support

I don't quite like command line wrappers, so here it is. Probably not what you expected, as I know nothing about C++. I just make it compile, that's all.

If you have both Haskell and C++ expertise please help.

## Examples

The functions are named after their official siblings.

| P4Perl                      | p4h                                      |
|:----------------------------|:-----------------------------------------|
| `$p4->SetUser($username);`  |  `setUser p4 username`                   |
| `$p4->Run($cmd, $arg, ...)` |  `setArgv p4 [arg ...]`<br>`run p4 cmd`  |

An alternative version of [this](https://www.perforce.com/manuals/p4api/Content/P4API/clientapi.dropped.html#clientapi.dropped.example) example:

```haskell
example :: IO ()
example = withP4Env env $ \p4 -> do
  setProg p4 "unnamed haskell script"
  setVersion p4 "2023.06"
  setInput p4 "my-super-password"
  setHandler p4 (OutputInfo $ putStrLn . ("[Info] " ++))
  run p4 "login" >>= print
  let loop i = do
        isDropped <- dropped p4
        unless (isDropped || i > 9) $ do
          putStrLn $ "loop #" ++ show i
          run p4 "have" >>= print
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

In order to run `p4` commands, please use the following pattern:

```haskell
withP4Env env $ \p4 -> do
  ... -- other settings here
  setArgv p4 ["arg1", "arg2", ...]
  output <- run p4 "cmd"
  case output of
    Left err -> handle err
    Right ... -> ...
  ... -- do other stuff
  where
    env = P4Env client host password port user -- settings
```

### Spec parsing & formatting

```haskell
-- Spec [("field", "Field") ... ]
--      [ ("field1", Left "singleValue")
--      , ("field2", Right ["value1", "value2", ...])]

spec <- parseSpec p4 typ form
form <- formatSpec p4 typ spec
```

## Rationale

> The Helix Server protocol is not designed to support multiple concurrent queries over the same connection. Multithreaded applications that use the C++ API or derived APIs should ensure that a separate connection is used for each thread, or that only one thread may use a shared connection at a time.

That's what the document says, so I go create a new connection every time.

Use [Simple Haskell](https://www.simplehaskell.org/): avoid involving any advanced skills makes it easy to use. I know there's a Reader which could save some keystrokes, however I keep everything strictly in the IO monad.

I code C++ in C-style, I only make sure it compiles and won't leak serious memory.
Please also check [these](https://github.com/perforce/p4perl/) [official](https://github.com/perforce/p4python/) [repos](https://github.com/perforce/p4ruby).

## Build

Download the Helix Core C/C++ API [here](https://www.perforce.com/downloads/helix-core-c/c-api).

Place up-to-date version of the header files into `p4api/include`. You also need to put the libs into the corresponding directory, check `extra-lib-dirs` in `package.yaml`.

### macOS

Don't forget to add the directory where `libssl` is located, you probably need to install OpenSSL via [Homebrew](https://brew.sh/).

### Linux

API variants available:

- GLIBC 2.12 / 2.3
- OpenSSL 1.0.2 / 1.1.1 / 3

#### Debian

```
# apt install build-essential haskell-stack libssl-dev
```

#### CentOS

```
# yum install openssl-devel gcc gcc-c++ make
```
