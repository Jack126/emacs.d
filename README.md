# About

forked [cabins](https://github.com/cabins/emacs.d) , 很喜欢作者一句 “酸枣，是产于中国华东地区的一种山枣，个头小，味道酸甜，营养丰富”。 感谢大佬

# Installation

1. Just run the code below:

```bash
git clone https://github.com/Jack126/emacs.d ~/.emacs.d
```

2. Launch Emacs.
3. Enjoy the life.

# PS

1. 目前emacs版本28.1 ，系统版本 mac 10.15.7 下正常使用，其他版本下未测
2. 目前支持终端下使用，gui模式亦可（该模式下文字大小需调整 lisp/common/init-fn.el 51行 height）
3. 需安装字体 Nerd Font 

(

    brew cask

    brew tap homebrew/cask-fonts 

    brew install font-fira-code

    brew install font-Fira-Code-nerd-font

    brew install font-hack-nerd-font
)

4. 建议使用iterm2（终端色彩下能好看一点） [iterm2](https://www.iterm2.com/downloads.html)

5. 使用auto-save 自动保存（C-x C-s)可以少按几次了。。

6. 使用eglot 需搭配其他开发环境使用，需另搭建（请自行查阅）

7. 需安装ctags（brew install ctags ) , ctags -R -e 可在当前项目下生成TAGS ，搭配快捷键 M-. , M-, 跳转方法，回跳 （ >  , < 去，回。简单不)




# 快捷键

## NeoTree (窗口有效)

1. n 下一行 ， p 上一行。
2. SPC or RET or TAB 若是文件，在其他buffer打开；若是目录，可切换折叠、收起。
3. g 刷新树。
4. A 最大/最小化 NeoTree 窗口
5. H 切换显示隐藏文件。
6. C-c C-n 创建文件，若以 / 结尾则表示创建文件夹。
7. C-c C-d 删除文件或目录。
8. C-c C-r 重命名文件或目录。
9. C-c C-c 改变根目录。

# 常用快捷键
1. C-c f格式化代码
2. M-q 查找替换
3. C-c p f 项目中文件查找
4. M-o 窗口切换
5. C-d 拷贝当前行 （搭配 M-y 粘贴 使用，贼好用）
6. shift -> , <- ,(Shift + 方向左右)， 可切换窗口
