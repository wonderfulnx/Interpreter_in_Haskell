# Haskell大作业

## 介绍
这是一个使用Haskell编写的简单解释器，实现了解析语法树求类型求值，支持语法树中的代数数据类型，并实现了一个简单的REPL，遵循文法见`src/Parser.hs`。

## 环境说明

本作业使用`stack`构建项目，resolver版本为`lts-13.13`。

使用了如下库：

- `tasty`，`tasty-hunit`：用于构建自行编写的以及助教提供的测试
- `parser-combinators`，`megaparsec`：用于编写Parser
- `repline`：一个REPL的库，可以比较简便的实现REPL并提供了很多用户友好的功能

可在`package.yaml`中查看所有dependencies。

## 运行方式

- 对于必做部分和ADT直接放入测试文件使用`stack test`进行测试即可。
- 对于自行编写的两个测试文件，在`stack build`之后使用`stack runghc test/[name].hs`，具体见测试文件的`Usage`
- 对于REPL部分，使用`stack build`之后使用`stack exec fpproject-exe`运行即可。
  - 后加`parse`命令进入编译器的REPL，用来显示输入表达式的解析结果
  - 后加`repl`命令进入计算的REPL，支持表达式计算，赋值，`:t`查看后方输入表达式的类型，`:q`退出REPL。表达式文法见Parser.hs