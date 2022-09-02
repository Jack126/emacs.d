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

目前 emacs 新手一枚，摸索大佬配置中前进，望诸君共勉!

1. 目前 emacs 版本 28.1 ，系统版本 mac 10.15.7 下正常使用，其他版本下未测
2. 目前支持终端下使用，gui 模式亦可（该模式下文字大小需调整 lisp/common/init-fn.el 51 行 height）
3. 需安装字体 Nerd Font

```

    brew cask

    brew tap homebrew/cask-fonts

    brew install font-fira-code

    brew install font-Fira-Code-nerd-font

    brew install font-hack-nerd-font
```

4. 建议使用 iterm2（终端色彩下能好看一点） [iterm2](https://www.iterm2.com/downloads.html)

5. 使用[auto-save](https://github.com/manateelazycat/auto-save) 自动保存（C-x C-s)可以少按几次了。。

6. 使用 eglot 需搭配其他开发环境使用，需另搭建（请自行查阅）

7. 需安装 ctags（brew install ctags ) , ctags -R -e 可在当前项目下生成 TAGS ，搭配快捷键 M-. , M-, 跳转方法，回跳 （ > , < 去，回。简单不)

# 快捷键

## NeoTree (窗口有效)

1. n 下一行 ， p 上一行。
2. SPC or RET or TAB 若是文件，在其他 buffer 打开；若是目录，可切换折叠、收起。
3. g 刷新树。
4. A 最大/最小化 NeoTree 窗口
5. H 切换显示隐藏文件。
6. C-c C-n 创建文件，若以 / 结尾则表示创建文件夹。
7. C-c C-d 删除文件或目录。
8. C-c C-r 重命名文件或目录。
9. C-c C-c 改变根目录。

# 常用快捷键

1. C-c f 格式化代码
2. C-x p f 项目中文件查找
3. C-x p g 项目中关键字查找(可配合关键词TODO，DEBUG简单使用todo功能)
4. C-x p p 项目切换
5. C-c d 拷贝当前行 （搭配 M-y 粘贴 使用，贼好用）
6. C-c r 查看最近打开文件
7. C-x b 切换 buffer
8. C-k 删除当前行
9. C-c z 查看当前文件绝对路径
10. C-c C-j imenu（查看当前文件菜单？）
11. C-x k 关掉当前 buffer
12. C-x u 撤销
13. C-h k 查看按键是否绑定（快捷键冲突检测用）
14. C-s , C-r 查找（前后），查找下一个 继续查找按 C-s 或 C-r
15. F8 neotree 显示隐藏 (可屏蔽neotree，使用speedbar)
16. 多行注释：
    1. 选中一段区域到最后一行行首（！很重要！）；
    2. 按 C-x r t ；
    3. 输入注释内容；
17. 反多行注释：
    1. 选中一段区域到最后一行，紧挨着注释字符之后的位置（！很重要！）；
    2. 按 c-x r k；
18. C-M ; 行尾添加注释
19. M-; 添加行注释
20. M-. 查找方法定义
21. M-, 跳回
22. M-q 查找替换
23. M-o 窗口切换(M-数字，跳窗口)
24. M-g g 跳行
25. C-S SPC mark-set (C-SPC 替代品, S - Shift)
26. C-l 重新绘制屏幕画面，当前行放在画面中心处
27. C-x C-t 交换两行的位置(下交换上)
28. M-u 使从光标位置到单词结尾处的字母变成大写
29. M-l 与M-u 相反使从光标位置到单词结尾处的字母变成小写
30. M-c 使从光标位置开始的单词的首字母变为大写 (M-l)
31. f5 php-mode和web-mode之间快速切换
32. f12 calendar 打开日历 ,q 退出(init-calendar 详细说明)
33. f9 list bookmarks
34. home beginning-of-buffer
35. end end-of-buffer
36. C-c y 单词翻译
37. C-c t 展示 todo list
38. C-c w 显示天气
39. C-c o 当前行下插入一行
40. C-a 回到行首 (C-e 行尾)
41. f2打开默认emacs.d配置文件目录
