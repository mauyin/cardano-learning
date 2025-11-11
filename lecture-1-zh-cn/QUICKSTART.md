# 快速开始指南 (Quick Start Guide)

欢迎来到 Haskell 第一课！这份指南将帮助你快速开始学习。

## 📋 前置要求

在开始之前，请确保你已经安装了 Haskell 工具链。

### 安装 Haskell 工具链

```bash
# 安装 ghcup（Haskell 工具链管理器）
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# 安装完成后，重启终端或运行：
source ~/.ghcup/env

# 验证安装
ghc --version
ghci --version
stack --version
```

## 🚀 项目设置

### 1. 克隆或下载项目

如果这是一个 Git 仓库：
```bash
git clone https://github.com/mauyin/lecture-1-zh-cn.git
cd lecture-1-zh-cn
```

### 2. 构建项目

使用 Stack 构建项目：
```bash
# 首次构建（会下载依赖，可能需要几分钟）
stack build

# 如果 stack.yaml 不存在，先运行：
stack init
```

或者使用 Cabal：
```bash
cabal update
cabal build
```

### 3. 运行测试

```bash
# 使用 Stack
stack test

# 使用 Cabal
cabal test
```

## 📚 学习步骤

### 第一步：观看幻灯片

在浏览器中打开 `slides/index.html`：

```bash
# macOS
open slides/index.html

# Linux
xdg-open slides/index.html

# Windows
start slides/index.html
```

仔细学习每一张幻灯片，理解 Haskell 的基本概念。

### 第二步：在 GHCi 中实验

启动 GHCi（Haskell 交互式解释器）：

```bash
stack ghci
# 或
ghci
```

尝试幻灯片中的所有示例：

```haskell
-- 算术运算
ghci> 3 + 4
7

-- 列表操作
ghci> [1..10]
[1,2,3,4,5,6,7,8,9,10]

-- 定义函数
ghci> let double x = x * 2
ghci> double 5
10

-- 退出 GHCi
ghci> :q
```

### 第三步：完成练习

#### 3.1 基础练习 (BasicDrills.hs)

**难度**: ⭐ | **时间**: 1-2 小时

```bash
# 在 GHCi 中加载
stack ghci
ghci> :load src/BasicDrills.hs

# 测试你的函数
ghci> double 5
-- 应该返回 10

# 运行测试
stack test --test-arguments "--match BasicDrills"
```

#### 3.2 核心练习 (Lecture1.hs)

**难度**: ⭐⭐ | **时间**: 2-3 小时

```bash
ghci> :load src/Lecture1.hs

# 测试练习 1
ghci> makeSnippet 7 "Hello, World!"
-- 应该返回 "Hello, ..."

# 运行测试
stack test --test-arguments "--match Lecture1"
```

#### 3.3 互动示例 (InteractiveExamples.hs)

**难度**: ⭐⭐ | **时间**: 2-3 小时

```bash
ghci> :load src/InteractiveExamples.hs

# 尝试示例
ghci> processText "  hello world  "
"Hello, hello world!"

# 修改函数，重新加载
ghci> :reload
```

#### 3.4 挑战题 (Challenges.hs)

**难度**: ⭐⭐⭐ | **时间**: 3-5 小时

```bash
ghci> :load src/Challenges.hs

# 测试挑战题
ghci> runLengthEncode "aaabbbcc"
[('a',3),('b',3),('c',2)]

# 运行测试
stack test --test-arguments "--match Challenges"
```

## 🔍 常用 GHCi 命令

| 命令 | 说明 | 示例 |
|------|------|------|
| `:load file` | 加载文件 | `:load src/Lecture1.hs` |
| `:reload` | 重新加载 | `:reload` |
| `:type expr` | 查看类型 | `:type double` |
| `:info name` | 查看信息 | `:info map` |
| `:quit` | 退出 GHCi | `:q` |
| `:help` | 帮助 | `:?` |

## 💡 学习技巧

### 1. 使用类型引导编程

类型签名是你最好的朋友：

```haskell
-- 先写类型签名
sumOfSquares :: Integer -> Integer

-- 让类型引导你的实现
sumOfSquares n = sum (map (^2) [1..n])
```

### 2. 在 GHCi 中实验

不确定某个函数怎么用？在 GHCi 中试试！

```haskell
ghci> :type map
map :: (a -> b) -> [a] -> [b]

ghci> map (*2) [1,2,3]
[2,4,6]
```

### 3. 阅读错误信息

Haskell 的错误信息很有帮助，仔细阅读：

```
• Couldn't match expected type 'Int' with actual type '[Char]'
```

这告诉你类型不匹配！

### 4. 从简单开始

先写一个简单版本，再优化：

```haskell
-- 简单版本
double x = x + x

-- 更好的版本
double x = x * 2
```

### 5. 使用测试

经常运行测试确保代码正确：

```bash
# 运行所有测试
stack test

# 运行特定测试
stack test --test-arguments "--match \"double\""
```

## 🆘 遇到问题？

### 常见问题

**问题 1**: `parse error on input`
- **原因**: 语法错误，通常是括号或缩进问题
- **解决**: 检查括号配对和缩进

**问题 2**: `No instance for (Show ...)`
- **原因**: 尝试打印无法显示的类型
- **解决**: 确保返回类型有 Show 实例

**问题 3**: `Non-exhaustive patterns`
- **原因**: 函数没有处理所有可能的输入
- **解决**: 添加更多模式匹配分支

**问题 4**: 测试失败
- **原因**: 你的实现与预期不符
- **解决**:
  1. 查看错误信息
  2. 在 GHCi 中测试你的函数
  3. 检查参考答案 (`solutions/` 目录)

### 获取帮助

1. 查看 `solutions/` 目录中的参考答案
2. 重新阅读幻灯片和题目说明
3. 在 GHCi 中实验和调试
4. 查阅 [Haskell 官方文档](https://www.haskell.org/documentation/)

## 📖 推荐资源

### 在线教程
- [Learn You a Haskell](http://learnyoua.haskell.sg/) - 中文版
- [Real World Haskell](http://cnhaskell.com/) - 中文版
- [Haskell 趣学指南](https://learnyouahaskell.mno2.org/)

### 练习平台
- [Exercism - Haskell Track](https://exercism.org/tracks/haskell)
- [HackerRank - Functional Programming](https://www.hackerrank.com/domains/fp)

### 社区
- [Haskell Reddit](https://www.reddit.com/r/haskell/)
- [Stack Overflow - Haskell Tag](https://stackoverflow.com/questions/tagged/haskell)

## ✅ 检查清单

学习 Lecture 1 的进度：

- [ ] 安装 Haskell 工具链
- [ ] 观看所有幻灯片
- [ ] 在 GHCi 中尝试所有示例
- [ ] 完成 BasicDrills.hs（18 题）
- [ ] 完成 Lecture1.hs（7 题）
- [ ] 探索 InteractiveExamples.hs（12 个示例）
- [ ] 挑战 Challenges.hs（7 题）
- [ ] 所有测试通过

完成以上所有项目后，你就掌握了 Haskell 的基础知识，可以继续学习 Lecture 2 了！

## 🎉 完成后

恭喜你完成了 Haskell 第一课！现在你已经：

✓ 理解了函数式编程的基本概念
✓ 掌握了 Haskell 的基本语法
✓ 学会了列表操作和高阶函数
✓ 能够编写递归函数
✓ 具备了解决实际问题的能力

**下一步：学习 Lecture 2 - Data Types！**

---

祝学习愉快！Happy Haskell coding! 🚀
