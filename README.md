DDDの人じゃないけど複雑なビジネスロジックのモデリングが面白そうだったので、参加してみました。

DDDのサンプルコードとしては参照しない方が良いと思います（プラクティスに沿っていない）。

## Ticket Modelling Challenge

[元ツイート](https://twitter.com/j5ik2o/status/1150589065432952832)

[チケット料金表](https://cinemacity.co.jp/ticket/)

## 気をつけたこと
以下の責務を分割した。

- カスタマーの提示した特典物から、可能なチケット種を判定する
- 上映回とチケット種から、定価を算出する
- カスタマーに最安となるチケット種を案内する


## 未実装
- 障がい者の同伴者の人数制限。「同伴者であることを自己申告する」という特典だと捉え、チケット価格には反映している（性善説運用…）
- DateType(日付,土日祝日)の判定。
