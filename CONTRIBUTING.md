# Contributing to Cardano Learning Resources

Thank you for your interest in contributing to the Cardano Learning Resources! This repository thrives on community contributions. Whether you're fixing a typo, adding a new article, or translating content, your help is appreciated.

## 🌍 Language / 语言

This guide is available in:
- English (current)
- 简体中文 (coming soon)

## 📋 Table of Contents

- [Ways to Contribute](#ways-to-contribute)
- [Getting Started](#getting-started)
- [Content Guidelines](#content-guidelines)
- [Translation Guidelines](#translation-guidelines)
- [Code Examples Guidelines](#code-examples-guidelines)
- [Submitting Changes](#submitting-changes)
- [Style Guide](#style-guide)
- [Code of Conduct](#code-of-conduct)

## 🤝 Ways to Contribute

### 1. Fix Issues
- Typos and grammar errors
- Broken links
- Incorrect code examples
- Outdated information

### 2. Improve Existing Content
- Clarify confusing explanations
- Add more examples
- Improve code comments
- Enhance diagrams or visuals

### 3. Add New Content
- New articles on Haskell, Plutus, or Cardano
- Additional code examples
- Practical exercises
- Real-world case studies

### 4. Translate Content
- Translate English articles to Chinese
- Translate Chinese articles to English
- Add other language translations
- Keep translations up-to-date

### 5. Review & Feedback
- Review pull requests
- Test code examples
- Suggest improvements
- Report issues

## 🚀 Getting Started

### 1. Fork the Repository

Click the "Fork" button at the top right of the repository page.

### 2. Clone Your Fork

```bash
git clone https://github.com/YOUR_USERNAME/cardano-learning.git
cd cardano-learning
```

### 3. Create a Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/your-fix-name
```

### 4. Make Your Changes

Follow the guidelines below for the type of contribution you're making.

### 5. Test Your Changes

- For articles: Check markdown rendering
- For code: Run all code examples to ensure they work
- For translations: Have a native speaker review if possible

## 📝 Content Guidelines

### Article Structure

Each article should follow this structure:

```markdown
# Title

## Introduction
Brief overview of the topic

## Part 1: Main Concept
Detailed explanation with examples

## Part 2: Deeper Dive
More advanced concepts

## Practical Examples
Code examples with explanations

## Conclusion
Summary and next steps

---

**Related Resources:**
- Links to official docs
- External learning resources
```

### Writing Style

- **Clear and Concise**: Use simple language, short sentences
- **Beginner-Friendly**: Assume no prior knowledge unless specified
- **Practical**: Include real-world examples and use cases
- **Well-Structured**: Use headings, lists, and formatting effectively
- **Accurate**: Verify all technical information
- **Inclusive**: Use welcoming, inclusive language

### Code in Articles

- Use inline code for short snippets: `deposit 100 50`
- Use code blocks for longer examples:

```haskell
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

- Include comments explaining complex concepts
- Reference separate example files when appropriate

## 🌐 Translation Guidelines

### Translation Principles

1. **Accuracy**: Maintain technical accuracy
2. **Natural**: Use natural, idiomatic language in target language
3. **Consistency**: Use consistent terminology
4. **Context**: Preserve the original meaning and context

### Translation Process

1. **Check Existing Translations**
   - Look for partial translations
   - Check for translation consistency

2. **Translate Content**
   - Translate article content
   - Keep code examples identical (unless language-specific comments)
   - Translate image alt text and captions

3. **Update Navigation**
   - Update language switcher links
   - Update table of contents if present

4. **Maintain Structure**
   - Keep folder structure identical
   - Use same filenames
   - Mirror the organization

### Technical Terms

Some terms should remain in English or use standard translations:

**Keep in English:**
- Cardano
- Plutus
- Haskell
- Maybe
- Just / Nothing

**Standard Chinese Translations:**
- Function → 函数
- Type → 类型
- Recursion → 递归
- Pattern Matching → 模式匹配
- Immutability → 不可变性

## 💻 Code Examples Guidelines

### File Organization

```
topic-name/
├── article.md
└── examples/
    ├── example1.hs
    ├── example2.hs
    └── example3.hs
```

### Code Style

**Haskell Code:**
- Follow standard Haskell style (2-space indentation)
- Include type signatures for all top-level functions
- Add comments explaining non-obvious code
- Include usage examples in comments

```haskell
-- Good: Clear type signature and comments
factorial :: Integer -> Integer  -- Type signature
factorial 0 = 1                  -- Base case
factorial n = n * factorial (n - 1)  -- Recursive case
```

### Example Structure

Each example file should include:

1. **Header Comment**: Explain what the example demonstrates
2. **Code**: Well-commented, working code
3. **Main Function**: If executable, include a `main` function
4. **Usage Instructions**: How to run the example

Example template:

```haskell
{-
  Title of Example

  This example demonstrates:
  - Concept 1
  - Concept 2
  - Concept 3
-}

-- Your code here

main :: IO ()
main = do
    -- Demonstration code

{-
  To run this example:
  1. ghci example.hs
  2. main
-}
```

### Testing Code

Before submitting:

```bash
# Test in GHCi
ghci example.hs
# Run main or test functions

# Compile to check for errors
ghc -Wall example.hs
```

## 📤 Submitting Changes

### Commit Messages

Use clear, descriptive commit messages:

```bash
# Good
git commit -m "Add section on Maybe types to Haskell 101"
git commit -m "Fix typo in factorial example"
git commit -m "Translate Haskell 101 article to Chinese"

# Bad
git commit -m "Update"
git commit -m "Fix stuff"
git commit -m "Changes"
```

### Pull Request Process

1. **Push to Your Fork**
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Create Pull Request**
   - Go to the original repository
   - Click "New Pull Request"
   - Select your fork and branch
   - Fill in the PR template

3. **PR Title Format**
   - `[Content]` for new articles
   - `[Fix]` for corrections
   - `[Translation]` for translations
   - `[Code]` for code examples
   - `[Docs]` for documentation updates

   Examples:
   - `[Content] Add article on Plutus basics`
   - `[Fix] Correct factorial example in Haskell 101`
   - `[Translation] Translate Haskell 101 to Chinese`

4. **PR Description**
   Include:
   - What changes were made
   - Why the changes were needed
   - Any related issues
   - Testing performed

### PR Template

```markdown
## Description
Brief description of changes

## Type of Change
- [ ] New content
- [ ] Bug fix
- [ ] Translation
- [ ] Code example
- [ ] Documentation

## Checklist
- [ ] I have tested the changes
- [ ] Code examples run without errors
- [ ] Markdown renders correctly
- [ ] Links are working
- [ ] Language is clear and accurate
- [ ] Follows style guidelines

## Related Issues
Closes #(issue number)
```

## 📐 Style Guide

### Markdown

- Use ATX-style headers (`#`, `##`, `###`)
- Leave blank lines around headers, lists, and code blocks
- Use **bold** for emphasis, *italics* for terms
- Use backticks for inline code: `code here`
- Use fenced code blocks with language specification

### File Naming

- Use lowercase
- Use hyphens for spaces: `my-article.md`
- Be descriptive: `why-cardano-chose-haskell.md`
- Keep consistent with existing structure

### Images

- Store in `assets/images/`
- Use descriptive names: `haskell-type-system.png`
- Include alt text: `![Haskell Type System](path/to/image.png)`
- Optimize images before committing (< 1MB)

## 🤖 Code of Conduct

### Our Standards

- Be respectful and inclusive
- Welcome newcomers
- Accept constructive criticism gracefully
- Focus on what's best for the community
- Show empathy towards others

### Unacceptable Behavior

- Harassment or discrimination
- Trolling or insulting comments
- Personal or political attacks
- Publishing others' private information
- Unprofessional conduct

### Enforcement

Report issues to repository maintainers. All reports will be reviewed and investigated confidentially.

## ❓ Questions?

- Open an issue for general questions
- Tag issues with `question` label
- Check existing issues first
- Be respectful and patient

## 🎉 Recognition

All contributors will be:
- Listed in the repository contributors
- Credited in relevant articles (if substantial contribution)
- Part of the growing Cardano learning community!

---

**Thank you for contributing to Cardano Learning Resources!**

Together, we're building a comprehensive educational resource for the entire Cardano community. 🚀
