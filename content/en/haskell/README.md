# Haskell Learning Resources

[← Back to Main](../../../README.md)

Welcome to the Haskell section! Here you'll find comprehensive resources about Haskell, the functional programming language that powers Cardano.

## Why Learn Haskell for Cardano?

Haskell is the foundation of Cardano's development:
- **Cardano Node**: Written entirely in Haskell
- **Plutus**: Smart contract platform based on Haskell
- **Formal Methods**: Haskell's affinity for mathematical verification
- **Safety & Reliability**: Pure functions and immutability prevent common bugs

## 📚 Available Topics

### Beginner Level

#### [01 - Haskell 101: Why Cardano Chose Haskell](01-haskell-101/)
**Status**: ✅ Complete

A comprehensive introduction to Haskell and functional programming, explaining:
- What is Haskell and why it matters
- Object-Oriented vs Functional Programming
- Haskell's perfect fit with Cardano's philosophy
- Hands-on examples with runnable code

**Topics Covered**:
- ✅ Basic functions and immutability
- ✅ Recursion and pattern matching
- ✅ List operations and higher-order functions
- ✅ Error handling with Maybe types

**Code Examples**:
- [basic.hs](01-haskell-101/examples/basic.hs) - Pure functions and immutability
- [factorial.hs](01-haskell-101/examples/factorial.hs) - Recursion and pattern matching
- [lists.hs](01-haskell-101/examples/lists.hs) - Map, filter, fold, and comprehensions
- [account.hs](01-haskell-101/examples/account.hs) - Maybe type for error handling

**Start Here**: [Why Cardano Chose Haskell](01-haskell-101/why-cardano-chose-haskell.md)

---

### Intermediate Level (Coming Soon)

#### 02 - Advanced Types and Type Classes
- Algebraic Data Types (ADTs)
- Type classes and polymorphism
- Functors, Applicatives, Monads
- Creating your own types

#### 03 - Real-World Haskell
- Working with IO
- File operations
- JSON parsing
- Building a CLI application

---

### Advanced Level (Coming Soon)

#### 04 - Monads and Effects
- Understanding monads deeply
- State, Reader, Writer monads
- Monad transformers
- Effect systems

#### 05 - Performance and Optimization
- Lazy evaluation strategies
- Strictness annotations
- Profiling Haskell code
- Common performance pitfalls

---

## 🎯 Learning Path

### For Complete Beginners
```
1. Haskell 101 (Current)
   └─> Read the article
   └─> Run all code examples
   └─> Modify examples and experiment

2. Advanced Types (Coming Soon)
3. Real-World Haskell (Coming Soon)
```

### For Experienced Programmers
```
1. Haskell 101 (Quick review of FP concepts)
2. Advanced Types (Focus on type system)
3. Monads and Effects
4. Jump to Plutus smart contracts
```

## 🛠️ Prerequisites

### Installing Haskell

**Option 1: GHCup (Recommended)**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

**Option 2: Package Manager**
```bash
# macOS
brew install ghc cabal-install

# Ubuntu/Debian
sudo apt-get install ghc cabal-install

# Fedora
sudo dnf install ghc cabal-install
```

### Verifying Installation
```bash
ghc --version    # Should show GHC version
ghci             # Starts interactive environment
cabal --version  # Shows Cabal build tool version
```

## 📖 Recommended Reading

### Books
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - Free, beginner-friendly
- [Real World Haskell](http://book.realworldhaskell.org/) - Practical applications
- [Haskell Programming from First Principles](https://haskellbook.com/) - Comprehensive, paid

### Online Resources
- [Haskell.org](https://www.haskell.org/) - Official website
- [Hoogle](https://hoogle.haskell.org/) - Haskell API search
- [Hackage](https://hackage.haskell.org/) - Haskell package repository

### Practice
- [Exercism Haskell Track](https://exercism.io/tracks/haskell) - Interactive exercises
- [Project Euler](https://projecteuler.net/) - Mathematical problems (great for FP practice)

## 🤔 Common Questions

**Q: Is Haskell hard to learn?**
A: Haskell has a learning curve, but it's a rewarding journey. Start with basics and build gradually.

**Q: Do I need to be a math expert?**
A: No! While Haskell has mathematical foundations, you can learn it practically through examples.

**Q: How long to become proficient?**
A: Basics in 2-4 weeks, comfortable in 2-3 months, proficient in 6-12 months with regular practice.

**Q: Can I build real applications?**
A: Absolutely! Haskell is used in production at many companies and powers the entire Cardano blockchain.

## 🔗 Next Steps

1. Complete [Haskell 101](01-haskell-101/why-cardano-chose-haskell.md)
2. Run and modify all code examples
3. Check out [Plutus resources](../../plutus/) (coming soon)
4. Join the Cardano developer community

---

**Language**: [English](README.md) | [简体中文](../../zh-CN/haskell/README.md)

[← Back to Main](../../../README.md)
