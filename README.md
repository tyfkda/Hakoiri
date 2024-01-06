箱入り娘ソルバー
================

#### Rust

```sh
$ cd rust
$ cargo run --release -- --interactive
step # 81: 0, ←
3━━┓1━━┓4━━┓2━━┓
┃<>┃┃<>┃┃<>┃┃<>┃
┃<>┃┃<>┃┃<>┃┃<>┃
┗━━┛┗━━┛┗━━┛┗━━┛
5━━━━━━┓7━━┓8━━┓
┗━━━━━━┛┗━━┛┗━━┛
9━━┓    0━━━━━━┓
┗━━┛   ←┃<><><>┃
6━━┓   ←┃<><><>┃
┗━━┛    ┗━━━━━━┛
>
```

```sh
$ time cargo run --release
Solved!, steps=81, check=#19306, left=93, hash=23805
Hands #118: [(6, Right), (3, Down), (5, Left), (8, Down), (4, Left), (9, Up), (8, Right), (4, Down), (5, Right), (5, Right), (7, Up), (7, Left), (6, Up), (6, Up), (4, Left), (9, Left), (9, Down), (5, Down), (6, Right), (6, Right), (7, Right), (7, Right), (3, Up), (4, Up), (9, Left), (9, Left), (8, Left), (8, Left), (5, Down), (7, Down), (7, Right), (4, Right), (3, Right), (1, Down), (1, Down), (0, Left), (2, Left), (6, Up), (6, Up), (7, Up), (7, Up), (4, Right), (2, Down), (2, Down), (0, Right), (1, Up), (1, Up), (3, Left), (8, Up), (8, Up), (9, Right), (9, Up), (5, Left), (5, Left), (2, Down), (4, Down), (8, Right), (8, Right), (0, Down), (6, Left), (6, Left), (7, Up), (7, Left), (8, Up), (8, Up), (4, Up), (4, Up), (2, Right), (9, Right), (9, Down), (0, Down), (6, Down), (6, Right), (1, Right), (3, Up), (3, Up), (0, Left), (6, Down), (6, Down), (7, Down), (8, Left), (4, Up), (2, Up), (9, Right), (6, Down), (0, Right), (3, Down), (3, Down), (1, Left), (7, Left), (8, Left), (4, Left), (2, Up), (2, Up), (0, Right), (7, Down), (7, Down), (8, Down), (8, Down), (1, Right), (3, Up), (3, Up), (7, Left), (7, Up), (5, Up), (6, Left), (6, Left), (9, Left), (9, Left), (0, Down), (8, Right), (8, Right), (7, Right), (7, Right), (5, Up), (9, Up), (9, Left), (0, Left)]
3142
3142
5578
900.
600.
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
