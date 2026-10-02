## インストール

```bash
curl https://raw.githubusercontent.com/laysakura/dotfiles/master/install.bash |bash -eu
```

## cmux

現在の設定を以下のファイルで管理する。

- `.config/cmux/cmux.json`: ダーク表示、選択時のコピー、`Ctrl+t` をprefixにしたtmux風ショートカット。
- `.config/ghostty/config`: フォント、配色、透過、カーソル、`Ctrl+Shift+H/J/K/L` によるペインリサイズ。

通常のインストールで `~/.config/` にコピーされる。cmux設定だけを反映する場合は、リポジトリのルートで以下を実行する（既存の同名設定を上書きする）。

```bash
mkdir -p ~/.config/cmux ~/.config/ghostty
cp .config/cmux/cmux.json ~/.config/cmux/cmux.json
cp .config/ghostty/config ~/.config/ghostty/config
cmux reload-config
```

フォントは `HackGen Console NF` を別途インストールする。Ghostty設定内で言及している任意のKarabinerリサイズルールと `~/bin/cmux-resize` は、このリポジトリには含めていない。

設定ファイルの役割と読み込み先は [cmux公式ドキュメント](https://cmux.com/ja/docs/configuration) を参照。
