# AtCoder

![atcoder](ac.png)

## AtCoder with Haskell

競技プログラミングを通して純粋な実装力を身につけることを目指す。
関数型プログラミング言語Haskellを使用して問題に取り組む。

## 問題の進め方

コンテストの情報やテストケースを取得する。

```bash
acc new abc000
```

問題を解くフォルダに移動してファイルを作成する。

```bash
cd abc000/a
vi main.hs
```

サンプルケースを用いてテストを行う。

```
oj t -d tests -c "runhaskell main.hs"
```