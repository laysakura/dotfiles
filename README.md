## インストール

```bash
curl https://raw.githubusercontent.com/laysakura/dotfiles/master/install.bash |bash -eu
```

## Orca

cmuxから移行した設定の原本は `.config/orca/` に置く。Orca 1.4.242 / macOSで確認。

- `settings.json`: ダーク表示、HackGen Console NF 11pt、黒背景・白文字・端末背景不透明度0.7（macOSではデスクトップ透過にならない。下記参照）、ANSI 16色、非点滅ブロックカーソル、選択時コピー、OptionをMetaとして扱う設定、50,000行のスクロールバック。
- `keybindings.json`: Orcaのアクションとショートカットの対応。標準キーに加えてKarabinerからの内部キー `Ctrl+F13`〜`Ctrl+F21` を受け取る。
- `karabiner.json`: Orcaが最前面のときだけ `Ctrl+t` prefixを処理するKarabinerルール。
- `apply.py`: 起動済みプロファイルの設定へ上記をマージし、Karabinerの選択中プロファイルにルールを有効化する。既存設定のバックアップは `~/.local/state/orca-dotfiles/` に保存する。

Orcaを一度起動してから終了し、Karabiner-ElementsとPython 3をインストールした状態で実行する。フォントは `HackGen Console NF` を別途インストールする。

```bash
# リポジトリのルートで実行。通常のinstall.bashは原本を~/.configへコピーする。
python3 .config/orca/apply.py
open -a Orca
```

Orcaのアプリ設定は `~/Library/Application Support/Orca/profiles/local-default/profile-state.db` に保存される。`settings.json` を置くだけでは反映されない。`apply.py` はOrca終了中に既存のsettings文書だけを更新し、リビジョンとSHA-256も更新する。別バージョンでは保存形式を確認してから実行する。認証情報やセッションを含むDB自体はGit管理しない。

| キー | 動作 |
| --- | --- |
| `Ctrl+t` → `2` / `3` | 下 / 右に分割 |
| `Ctrl+t` → `Ctrl+n` / `Ctrl+p` | 次 / 前のペイン |
| `Ctrl+t` → `z` / `=` | ペイン拡大・解除 / 均等化 |
| `Ctrl+t` → `c` | ターミナルタブを作成 |
| `Ctrl+t` → `n` / `p` | 次 / 前のターミナルタブ |
| `Ctrl+t` → `Ctrl+t` | 端末へ通常のCtrl+tを送信 |
| `Ctrl+t` → `Esc` | prefix解除 |

prefixの待機時間は1.2秒。cmuxの `Ctrl+t [` コピーモード、Emacs風の選択操作、`Ctrl+Shift+H/J/K/L` リサイズはOrcaに対応アクションがないため未移行。選択はマウス、コピーは選択時の自動コピーまたは `Cmd+C`、検索は `Cmd+F`、リサイズは分割境界のドラッグを使う。Ghosttyの100MBスクロールバックはOrca上限の50,000行に対応付けた。Ghostty固有のフォントスムージングなど、描画エンジンによる差は残る。

Orca 1.4.242のmacOS版はメインウィンドウを不透明な背景色で生成しており、`terminalBackgroundOpacity = 0.7` は端末描画の背景にのみ作用する。cmuxのように背後のデスクトップや別ウィンドウを透かすことは、現行の設定では再現できない。`windowBackgroundBlur` もこの版ではWindowsのacrylicにのみ接続されており、macOSの透過を有効にする設定ではない。

## cmux（旧設定）

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
