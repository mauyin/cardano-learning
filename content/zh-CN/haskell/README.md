# Haskell 学习资源

[← 返回主页](../../../README.zh-CN.md)

欢迎来到 Haskell 板块！在这里您将找到关于 Haskell 的全面资源，这是支撑 Cardano 的函数式编程语言。

## 为什么要为 Cardano 学习 Haskell？

Haskell 是 Cardano 开发的基石：
- **Cardano 节点**：完全用 Haskell 编写
- **Plutus**：基于 Haskell 的智能合约平台
- **形式化方法**：Haskell 与数学验证的天然契合
- **安全与可靠**：纯函数和不可变性防止常见错误

## 📚 可用主题

### 初级

#### [01 - Haskell 101：为什么 Cardano 选择 Haskell](01-haskell-101/)
**状态**: ✅ 完成

全面介绍 Haskell 和函数式编程，解释：
- Haskell 是什么以及为什么它重要
- 面向对象 vs 函数式编程
- Haskell 与 Cardano 理念的完美契合
- 带有可运行代码的实践示例

**涵盖主题**:
- ✅ 基本函数与不可变性
- ✅ 递归与模式匹配
- ✅ 列表操作与高阶函数
- ✅ 使用 Maybe 类型进行错误处理

**代码示例**:
- [basic.hs](01-haskell-101/examples/basic.hs) - 纯函数与不可变性
- [factorial.hs](01-haskell-101/examples/factorial.hs) - 递归与模式匹配
- [lists.hs](01-haskell-101/examples/lists.hs) - Map、filter、fold 和列表推导
- [account.hs](01-haskell-101/examples/account.hs) - Maybe 类型的错误处理

**从这里开始**: [为什么 Cardano 选择 Haskell](01-haskell-101/why-cardano-chose-haskell.md)

---

### 中级（即将推出）

#### 02 - 高级类型与类型类
- 代数数据类型（ADTs）
- 类型类与多态
- Functors、Applicatives、Monads
- 创建自定义类型

#### 03 - 真实世界的 Haskell
- 处理 IO
- 文件操作
- JSON 解析
- 构建 CLI 应用程序

---

### 高级（即将推出）

#### 04 - Monads 与 Effects
- 深入理解 monads
- State、Reader、Writer monads
- Monad transformers
- 效果系统

#### 05 - 性能与优化
- 惰性求值策略
- 严格性注解
- Haskell 代码性能分析
- 常见性能陷阱

---

## 🎯 学习路径

### 对于完全初学者
```
1. Haskell 101（当前）
   └─> 阅读文章
   └─> 运行所有代码示例
   └─> 修改示例并实验

2. 高级类型（即将推出）
3. 真实世界的 Haskell（即将推出）
```

### 对于经验丰富的程序员
```
1. Haskell 101（快速回顾 FP 概念）
2. 高级类型（专注于类型系统）
3. Monads 与 Effects
4. 跳转到 Plutus 智能合约
```

## 🛠️ 前置要求

### 安装 Haskell

**选项 1: GHCup（推荐）**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

**选项 2: 包管理器**
```bash
# macOS
brew install ghc cabal-install

# Ubuntu/Debian
sudo apt-get install ghc cabal-install

# Fedora
sudo dnf install ghc cabal-install
```

### 验证安装
```bash
ghc --version    # 应显示 GHC 版本
ghci             # 启动交互式环境
cabal --version  # 显示 Cabal 构建工具版本
```

## 📖 推荐阅读

### 书籍
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - 免费，适合初学者
- [Real World Haskell](http://book.realworldhaskell.org/) - 实用应用
- [Haskell Programming from First Principles](https://haskellbook.com/) - 全面，付费

### 在线资源
- [Haskell.org](https://www.haskell.org/) - 官方网站
- [Hoogle](https://hoogle.haskell.org/) - Haskell API 搜索
- [Hackage](https://hackage.haskell.org/) - Haskell 包仓库

### 练习
- [Exercism Haskell Track](https://exercism.io/tracks/haskell) - 交互式练习
- [Project Euler](https://projecteuler.net/) - 数学问题（非常适合 FP 练习）

## 🤔 常见问题

**问：Haskell 很难学吗？**
答：Haskell 有一定的学习曲线，但这是一段有价值的旅程。从基础开始，逐步构建。

**问：我需要成为数学专家吗？**
答：不需要！虽然 Haskell 有数学基础，但您可以通过示例进行实践学习。

**问：需要多长时间才能熟练？**
答：基础 2-4 周，熟悉 2-3 个月，通过定期练习 6-12 个月可以达到熟练。

**问：我可以构建真实应用程序吗？**
答：当然可以！Haskell 在许多公司的生产环境中使用，并支撑着整个 Cardano 区块链。

## 🔗 下一步

1. 完成 [Haskell 101](01-haskell-101/why-cardano-chose-haskell.md)
2. 运行并修改所有代码示例
3. 查看 [Plutus 资源](../../plutus/)（即将推出）
4. 加入 Cardano 开发者社区

---

**语言**: [English](../../en/haskell/README.md) | [简体中文](README.md)

[← 返回主页](../../../README.zh-CN.md)
