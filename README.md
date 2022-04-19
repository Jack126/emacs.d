# About

This is `NOT` a part of [GNU Emacs](https://www.gnu.org/software/emacs/) but a personal-daily-use configuration maintained by © Cabins from China.

# Dev Code

`噪鹃`, English name? No, just Chinese `噪鹃`.

# Target

1. Works on Windows & macOS & GNU/Linux & Android (By [termux](https://termux.com/))
2. Lightweight
3. Only latest version of Emacs (current is 28) is supported

# Programming

By [Eglot](https://github.com/joaotavora/eglot) (default) / [lsp-mode](https://github.com/emacs-lsp/lsp-mode).

All you need to do is `install the specific server and put it into the PATH environment variable`. The supported servers are listed in Eglot / lsp-mode repo.

> Solution for jdtls on Windows issue: put the bin/jdtls.bat into the server/bin folder.

# Installation

1. Just run the code below:

```bash
git clone https://github.com/cabins/emacs.d ~/.emacs.d
```

2. Launch Emacs.
3. Enjoy the life.



快捷键 （NeoTree 窗口有效）
n 下一行 ， p 上一行。
SPC or RET or TAB 若是文件，在其他buffer打开；若是目录，可切换折叠、收起。
g 刷新树。
A 最大/最小化 NeoTree 窗口
H 切换显示隐藏文件。
C-c C-n 创建文件，若以 / 结尾则表示创建文件夹。
C-c C-d 删除文件或目录。
C-c C-r 重命名文件或目录。
C-c C-c 改变根目录。
