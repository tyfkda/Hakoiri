箱入り娘ソルバー
================

#### Rust

```sh
$ cd rust
$ time cargo run --release
```

#### Haskell

```sh
$ cd haskell
$ time stack run
```


### 計測結果

| 実装方法 | 実行速度(秒) |
|:--------|-----------:|
| Rust (naive) | 0.08 |
| Haskell      | 0.56 |
