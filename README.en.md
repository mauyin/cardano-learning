# Cardano Learning Resources

Welcome to the Cardano Learning Resources repository! This is a comprehensive, open-source collection of educational materials about Cardano, Haskell, Plutus, and the broader Cardano ecosystem.

## 🌍 Languages / 语言

- **English**: You're reading it! (Current file)
- **简体中文**: [README.md](README.md)

## 📚 Content Overview

This repository is organized by topics, with each topic containing articles, code examples, and practical exercises. All content is available in both English and Simplified Chinese.

### Current Topics

#### 🔷 Haskell
- **[Haskell 101](content/en/haskell/01-haskell-101/)** - Why Cardano Chose Haskell: A deep dive into functional programming and its perfect match with Cardano's philosophy
  - Basic functions and immutability
  - Recursion and pattern matching
  - List operations and higher-order functions
  - Error handling with Maybe types

#### 🔷 Plutus (Coming Soon)
- Smart contract development on Cardano
- From Haskell to on-chain code
- Real-world Plutus examples

#### 🔷 Cardano Blockchain (Coming Soon)
- Node operation
- Staking mechanisms
- Governance and voting
- Native assets

#### 🔷 Other Languages (Coming Soon)
- Aiken
- Rust in the Cardano ecosystem

## 🚀 Getting Started

### For Readers

1. **Choose your language**: Navigate to either `content/en/` or `content/zh-CN/`
2. **Pick a topic**: Start with Haskell 101 if you're new to functional programming
3. **Read and code**: Each article includes runnable code examples
4. **Experiment**: All code examples can be run locally with GHC/GHCi

### For Haskell Examples

To run the Haskell examples, you'll need GHC (Glasgow Haskell Compiler) installed:

```bash
# Install GHC and cabal (Haskell build tool)
# On macOS:
brew install ghc cabal-install

# On Linux:
sudo apt-get install ghc cabal-install

# On Windows:
# Download from https://www.haskell.org/platform/
```

Then navigate to any example and run it:

```bash
cd content/en/haskell/01-haskell-101/examples

# Run in GHCi (interactive mode)
ghci basic.hs
# In GHCi prompt, type: main

# Or compile and run
ghc basic.hs
./basic
```

## 🗂️ Repository Structure

```
cardano-learning/
├── README.md                          # Chinese main README
├── README.en.md                       # This file (English)
├── content/
│   ├── en/                           # English content
│   │   ├── haskell/
│   │   │   └── 01-haskell-101/
│   │   │       ├── why-cardano-chose-haskell.md
│   │   │       └── examples/
│   │   │           ├── basic.hs
│   │   │           ├── factorial.hs
│   │   │           ├── lists.hs
│   │   │           └── account.hs
│   │   ├── plutus/
│   │   ├── cardano/
│   │   └── other-languages/
│   └── zh-CN/                        # Simplified Chinese content
│       └── (mirrors en/ structure)
├── assets/
│   └── images/
├── CONTRIBUTING.md
└── LICENSE
```

## 🤝 Contributing

We welcome contributions! Whether it's:
- Fixing typos or improving explanations
- Translating content
- Adding new topics or examples
- Suggesting improvements

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## 📖 Learning Path Suggestions

### For Complete Beginners
1. Start with [Haskell 101](content/en/haskell/01-haskell-101/why-cardano-chose-haskell.md)
2. Run all code examples and experiment
3. Move to Plutus basics (coming soon)

### For Developers with OOP Background
1. Read the OOP vs FP comparison in Haskell 101
2. Focus on understanding immutability and pure functions
3. Practice with recursion and pattern matching examples

### For Blockchain Developers
1. Review Haskell fundamentals
2. Understand why Cardano chose Haskell
3. Dive into Plutus smart contracts (coming soon)

## 🔗 Official Resources

- [Cardano Official Documentation](https://docs.cardano.org/)
- [Cardano Foundation](https://cardanofoundation.org/)
- [Plutus Documentation](https://plutus.readthedocs.io/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)
- [Haskell.org](https://www.haskell.org/)

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🌟 Support

If you find this repository helpful, please consider:
- ⭐ Starring the repository
- 🔄 Sharing it with others
- 🤝 Contributing your own knowledge
- 💬 Opening issues for suggestions or questions

## 📬 Contact

For questions, suggestions, or collaboration opportunities, please open an issue on GitHub.

---

**Built with ❤️ for the Cardano community**
