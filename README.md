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

目前emacs新手一枚，摸索大佬配置中前进，望诸君共勉!

1. 目前emacs版本28.1 ，系统版本 mac 10.15.7 下正常使用，其他版本下未测
2. 目前支持终端下使用，gui模式亦可（该模式下文字大小需调整 lisp/common/init-fn.el 51行 height）
3. 需安装字体 Nerd Font 

```

    brew cask

    brew tap homebrew/cask-fonts 

    brew install font-fira-code

    brew install font-Fira-Code-nerd-font

    brew install font-hack-nerd-font
```

4. 建议使用iterm2（终端色彩下能好看一点） [iterm2](https://www.iterm2.com/downloads.html)

5. 使用[auto-save](https://github.com/manateelazycat/auto-save) 自动保存（C-x C-s)可以少按几次了。。

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
2. C-x p f 项目中文件查找
3. C-x p g 项目中关键字查找
4. C-x p p 项目切换
5. C-d 拷贝当前行 （搭配 M-y 粘贴 使用，贼好用）
6. shift -> , <- ,(Shift + 方向左右)， 可切换窗口 同6
7. C-c r 查看最近打开文件
8. C-x b 切换buffer
9. C-k 删除当前行
10. C-c z 查看当前文件绝对路径
11. C-c C-j imenu（查看当前文件菜单？）
12. C-x k 关掉当前buffer
13. C-x u 撤销
14. C-h k 查看按键是否绑定（快捷键冲突检测用）
15. C-s , C-r 查找（前后），查找下一个 继续按 C-s 或 C-r
16. F8 neotree 显示隐藏
17. 多行注释：
    1) 选中一段区域到最后一行行首（！很重要！）；
    2) 按 C-x r t ；
    3) 输入注释内容；
18. 反多行注释：
    1) 选中一段区域到最后一行，紧挨着注释字符之后的位置（！很重要！）；
    2) 按 c-x r k；
19. C-M ;  行尾添加注释
20. M-; 添加行注释
21. M-. 查找方法定义
22. M-, 跳回
23. M-q 查找替换
24. M-o 窗口切换
