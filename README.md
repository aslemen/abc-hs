# abc-hs（暫定名）
Haskell Toolkit for ABC Treebank

## 機能（暫定版）
- abc-relabel：範疇の書き換え。（ただし、dep markingは`--`ではなくて`|`を要求する）
- abc-validate：範疇のチェック。結果はもとの範疇の右隣に表示される。

## インストール
stackでコンパイル。戸次研ならばノウハウはあるはず。
あるいは、binaryのダウンロード：https://github.com/aslemen/abc-hs/releases

## 実行方法
いずれも、stdinからツリーを読み込ませる。stdoutで吐き出される。

## TODO
- アルゴリズムのさらなる検討（特にparsingあたり）
- コマンドライン
- 