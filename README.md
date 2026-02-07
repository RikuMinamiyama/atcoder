# AtCoder

![atcoder](ac.png)

## AtCoder in Haskell

競技プログラミングを通して純粋な実装力を身につけることを目指す。
関数型プログラミング言語Haskellを使用して問題に取り組む。

## 問題の進め方

コンテストの情報やテストケースを取得する。

```bash
acc new abc000
```

移動して問題を解く。

```bash
cd abc000/a
```

サンプルケースを用いてテストを行う。

```bash
oj t -d tests -c "runhaskell main.hs"
```

提出したら次の問題に移動する。

```bash
cd ../b
```
