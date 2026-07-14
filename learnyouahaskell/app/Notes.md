### Function guards

```haskell
-- Syntax
funcName x
| condition1 = result1
| condition2 = result2
| otherwise = defaultResult -- 'otherwise' is literally just
'True'
```

> [!NOTE]
> There is no `=` on the first line of the definition.


### Type constructors

```haskell
data Maybe a = Nothing | Just a
```
`Maybe` is a **type constructor** with one parameter `a`.

`Noting` and `Just a` are **constructors**

