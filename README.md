emacs29


typescript-language-server找不到问题

```
lsp server 安装在 ~/.npm/typescript-language-server/ 下面

➜  cd ~/.npm/typescript-language-server 
➜  npm install typescript-language-server typescript
之后把 typescript-language-server 的 BIN 目录导出到 PATH 变量里

NODE_PACKAGE_HOME=$HOME/.npm

for NODE_PACKAGE in $NODE_PACKAGE_HOME/*; do
    NODE_PACKAGE_BIN=$NODE_PACKAGE/node_modules/.bin
    if [[ -d $NODE_PACKAGE_BIN ]]
    then
	PATH=$PATH:$NODE_PACKAGE_BIN
    fi
done
```