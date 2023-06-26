# p4h

(dysfunctional) Perforce API binding

I don't quite like command line wrappers, so here it is. Probably not what you expected, as I know nothing about C++. I just make it compile, that's all.

If you have both Haskell and C++ expertise please help.

## Examples

```haskell
example = runP4 ["login"] $ \p4 -> do
  setUser p4 "super"
  setInput p4 "my-super-password"
  setHandler p4 (OutputInfo $ putStrLn . ("[Info]\n" ++))
```
